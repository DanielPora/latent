
# In case R studio throws errors when running the whole script at once,
# try to update all packages and run the library imports line by line.
# After that you can also run the whole script in one go, 


library(ggplot2)
library(MASS)
library(poLCA)
#install.packages("poLCA", dependencies = TRUE)
library(gridExtra)
library(tidyLPA)
#install.packages("tidyLPA")
library(data.table)
library(plyr)
library(dplyr)
library(lme4)
?lme4
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
##########################################################################
?dcast
# read data
dat_tar <- read.csv("data_s3_target.csv")
dat_id <- read.csv("data_s3_ID.csv")

# clean_F.csv concerns only columns that are relevant for the analysis
clean <- read.csv("clean_F.csv")


# Preconsideration

# Using a generative adversarial network (CTGAN) to generate synthetic data did
# not proof to be viable. It was not able to pick up several important 
# very strong correlations between columns. E.g. the one-hot encoding between 
# own.cod and other.cod, target and response object have to be of the same test
# type and so on. An example of this synthetic data can be found in synth1000.
# It's planned to train this model again on a much more sparse predictor space
# without the strong correlations and account for the multiple measurements
# coming from the same participant
synth1000 <- read.csv("synth1000.csv")

# In order to generate authentic data a good understanding of the data and it's
# distributions is necessary 



#### 1. Specific clusters regarding to the tests

# Subjects listed with their respective total scores for the 3 tests
subj_tests <- clean %>%
  dplyr::select("workerID", "aq_score_subset", "opt_score", "stroop_difference")%>%
  distinct()


## 1.1 Normalizing the tests data
subj_tests_norm <- as.data.frame(scale(subj_tests[2:4]))
subj_tests_norm <- subj_tests_norm%>%
  mutate("workerID" = subj_tests$workerID)

data_normalized <- subj_tests_norm %>%
  dplyr::select("workerID", "aq_score_subset", "opt_score", "stroop_difference")
tests_norm <- data_normalized[,-1]
rownames(tests_norm) <- data_normalized[,1]

ggplot(stack(tests_norm), aes(x = ind, y = values)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  labs(x="Tests", y="Normalized Value") +
  geom_boxplot() 

## 1.2 Plotting densities of each test
ggplot(tests_norm, aes(aq_score_subset))+
  geom_density()

ggplot(tests_norm, aes(opt_score))+
  geom_density()

ggplot(tests_norm, aes(stroop_difference))+
  geom_density()

## 1.3 Scatterplots of pairwise tests 
ggplot(tests_norm, aes(aq_score_subset, opt_score))+
  geom_point()

ggplot(tests_norm, aes(aq_score_subset, stroop_difference))+
  geom_point()

ggplot(tests_norm, aes(stroop_difference, opt_score))+
  geom_point()

corr_matrix <- cor(tests_norm)
corr_matrix


## 1.4 Summary

# The scatter plots don't hint at some correlation, using a correlation 
# matrix to confirm.
# Density of OPT shows a bi-modal distribution. Suggesting that participants
# can be grouped in two optical perspective related groups.
# There doesn't seem to be more of a structure.
 

#### 2. Perspective taking preferences

## 2.1. Identify problems for same perspective (s-p) task
problems <- clean %>%
  filter(clean$accuracy == 0)
# Front-Back errors in all s-p trails
fb_p <- problems %>%
  filter(problems$targetPos == "F" |problems$targetPos == "B")
# Left-Right errors in all s-p trails
lr_p <- problems %>%
  filter(problems$targetPos == "L" |problems$targetPos == "R")
# 74 FB errors
nrow(fb_p)
# 1 LR error
nrow(lr_p)

# Numbers of different and same perspective trials in total
clean %>%
  dplyr::count(perspective)

# subjects and the number of total FB trails
worker_fb <- clean %>%
  filter(perspective == "same", targetPos == "F" | targetPos == "B") %>%
  dplyr::count(workerID)
# subjects and the number of errors they made in same perspective FB trails
worker_fb_p <- fb_p %>%
  dplyr::count(workerID, sort = TRUE) 

# Workers and the proportion of errors they made in FB trails
diff <- left_join(worker_fb_p, worker_fb, by="workerID", suffix=c("_p", "_all"))
(diff <- diff %>%
  mutate(prop = round(n_p/n_all,2))%>%
  arrange(desc(prop)))


## 2.2 Summary

# In 1404 same perspective experiments occurred 1 left-right confusion (<0.1%),
# which is neglect able, and 74 front-back confusions (=5.3%)
# This suggest that in about 5% of the cases for a "different" perspective 
# front-back scenario the own-other perspective classification is not correct.
# In particular we can see that half of the participants who confused front-back
# did it systematically. Therefore it's suggested to invert the interpretation
# for their own.cod and other.cod entries or remove them from the analysis.

exclusions <- diff$workerID
clean_ex <- clean %>%
  filter(!(workerID %in% exclusions)) 
# Leaving out 412 observations


#### 3 Individual perspective preference


## 3.1 Investigating "different" perspective scenarios

# Using front-back, left-right... 

fb <- clean %>%
  filter(perspective == "different", targetPos == "F" | targetPos == "B")

fb_cl <- clean_ex %>%
  filter(perspective == "different", targetPos == "F" | targetPos == "B")

lr <- clean %>%
  filter(perspective == "different", targetPos == "L" | targetPos == "R")

lr_cl <- clean_ex %>%
  filter(perspective == "different", targetPos == "L" | targetPos == "R")

# ... and all scenarios together to analyse the perspective preference by subject
# own_tendency describes here whether the participant preferred the egocentric
# perspective versus the othercentric perspective.
# 1 = always egocentric, 0 = 50/50, -1 = always othercentric
by_subj_diff <- clean_ex %>%
  filter(perspective == "different") %>%
  group_by(workerID) %>%
  summarise(respTime_mean = mean(respTime), respTime_sd = sd(respTime), 
            own_sum_diff = sum(own.cod), other_sum_diff = sum(other.cod),
            own_tendency = (sum(own.cod)-sum(other.cod))/(sum(own.cod)+sum(other.cod)),
            aq_score = mean(aq_score_subset), 
            opt_score = mean(opt_score_total),stroop_difference = mean(stroop_difference))

by_subj_fb <- fb_cl %>%
  group_by(workerID) %>%
  summarise(respTime_mean = mean(respTime), respTime_sd = sd(respTime), 
            own_sum_diff = sum(own.cod), other_sum_diff = sum(other.cod),
            own_tendency = (sum(own.cod)-sum(other.cod))/(sum(own.cod)+sum(other.cod)),
            aq_score = mean(aq_score_subset), 
            opt_score = mean(opt_score_total),stroop_difference = mean(stroop_difference))

by_subj_lr <- lr_cl %>%
  group_by(workerID) %>%
  summarise(respTime_mean = mean(respTime), respTime_sd = sd(respTime), 
            own_sum_diff = sum(own.cod), other_sum_diff = sum(other.cod),
            own_tendency = (sum(own.cod)-sum(other.cod))/(sum(own.cod)+sum(other.cod)),
            aq_score = mean(aq_score_subset), 
            opt_score = mean(opt_score_total),stroop_difference = mean(stroop_difference))


## 3.2 Visualizing


p1 <- ggplot(by_subj_diff, aes(own_tendency))+
  geom_histogram()+
  ggtitle("Combined different perspective tasks")

ggplot(by_subj_diff, aes(aq_score, opt_score, color = own_tendency))+
  geom_point()

ggplot(by_subj_diff, aes(own_tendency, opt_score))+
  geom_point()

p2 <- ggplot(by_subj_fb, aes(own_tendency))+
  geom_histogram()+
  ggtitle("Only front-back different perspective tasks")

p3 <- ggplot(by_subj_lr, aes(own_tendency))+
  geom_histogram()+
  ggtitle("Only left-right different perspective tasks")

grid.arrange(p1, p2,p3, ncol=3)


## 3.3 Summary

# It is surprising that reference for perspective taking seems to be dependent on
# the direction. From the histograms we can see a strong egocentric tendency in 
# left-right trials and a medium tendency for a othercentric perspective in 
# front-back trials.


#### 4 LPA Individual differences effect on perspective taking

# Considered are only different perspective items grouped for front-back (FB)
# and left-right (LR) 

## 4.1 Plotting the tendencies
by_subj_lr <- by_subj_lr %>%
  filter(workerID %in% by_subj_fb$workerID)
  
df <- data.frame('workerID' =by_subj_lr$workerID,'fb_tend'=by_subj_fb$own_tendency,
                 'lr_tend'=by_subj_lr$own_tendency, 'aq'=by_subj_lr$aq_score,
                 'opt' =by_subj_lr$opt_score, 'stroop'=by_subj_lr$stroop_difference)

# We can see 3 main clusters -1/-1(pure othercentrics), -1/1(lr egocentrics) and 
# 1/1(pure egocentrics), highlighting that FB and LR perspective preference is
# not symmetrical, since there is no 1/-1 cluster.
ggplot(df, aes(lr_tend,fb_tend, color=aq))+
  geom_jitter(width=0.1, height=0.1)+
  scale_colour_gradientn(colours=rainbow(4))
ggplot(df, aes(lr_tend,fb_tend, color=stroop))+
  geom_jitter(width=0.1, height=0.1)+
  scale_colour_gradientn(colours=rainbow(4))
# Visually there does not seem to be a correlation to AQ and Stroop scores
# on their own.

# With OPT score on the other hand:
ggplot(df, aes( lr_tend,fb_tend, color=opt))+
  geom_jitter(width=0.1, height=0.1)+
  scale_colour_gradientn(colours=rainbow(4))
# Visually we get ideas about the distribution of OPT scores and tendencies.
# Subjects with good OPT score (lower) seem to prefer othercentric views, while
# worse OPT score subjects tend to LR-egocentrism. The class of pure egocentric
# subjects has a mixture of OPT scores.
# Indifferent/random or FB egocentric subjects are not common at all.


## 4.2 LPA for individual differences

# Based on AHP a 4 Class model is the best choice
df %>%
  dplyr::select(aq, opt, stroop) %>%
  single_imputation() %>%
  estimate_profiles(1:5)%>% 
  compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik"))

# No clear separation of classes was found. The classes are mostly governed by
# OPT score that is relatively good separated into 2 classes (1+2 and 3+4). 
df %>%
  dplyr::select(aq, opt, stroop) %>%
  scale() %>%
  estimate_profiles(4) %>%
  plot_profiles()

# 2 classes had also the best BIC value and looks much more fitting for OPT.
# The other scores seem to not fit very well to any class though.
df %>%
  dplyr::select(aq, opt, stroop) %>%
  scale() %>%
  estimate_profiles(2) %>%
  plot_profiles()

# LPA for 2 and 4 classes
LPA_2 <- df %>%
  dplyr::select(aq, opt, stroop) %>%
  single_imputation() %>%
  estimate_profiles(2)

classes_2 <- get_data(LPA_2)$Class %>%
  factor()

LPA_3 <- df %>%
  dplyr::select(aq, opt, stroop) %>%
  single_imputation() %>%
  estimate_profiles(3)

classes_3 <- get_data(LPA_3)$Class %>%
  factor()

LPA_4 <- df %>%
  dplyr::select(aq, opt, stroop) %>%
  single_imputation() %>%
  estimate_profiles(4)

classes_4 <- get_data(LPA_4)$Class %>%
  factor()

df_LPA_classes <- df %>%
  mutate('class_2' = classes_2, 'class_3'=classes_3, 'class_4'=classes_4)
  
# 2 classes plot, resembles tendencies plot from 4.1
ggplot(df_LPA_classes, aes( lr_tend,fb_tend, color=class_2))+
  geom_jitter(width=0.1, height=0.1)

# 3 classes plot
ggplot(df_LPA_classes, aes( lr_tend,fb_tend, color=class_3))+
  geom_jitter(width=0.1, height=0.1)

# 4 classes plot
ggplot(df_LPA_classes, aes( lr_tend,fb_tend, color=class_4))+
  geom_jitter(width=0.1, height=0.1)

# The classes of individual differences don't provide a good 
# clustering. Based on the weak correlations this is no surprise.

#### 5 LCA for perspective preference

# 5.1 transforming data to fit LCA
trails_diff <- clean_ex %>%
  filter(clean_ex$perspective == 'different')

# Different number of trails for subjects make an LCA encoding hard.
# 28 of 153 participant did not complete all different perspective 8 trails.
# LCA uses categorical data, so using the tendencies is not possible. 
count(trails_diff, workerID, sort=TRUE)

# Changing the data to a different format, where response to R1 (first 
# targetPos R trail) is 1 for subject choose egocentric and 2 for subject
# choose othercentric. The values arise from restriction on values of the poLCA
# package that will be used for LCA.
# Example:
# Subject  B1 B2 F1 F2 L1 L2 R1 R2
# Q3AAKY3  1  1  1  1  2  2  2  2

# Workers with one missing trial
tasks_7comp <- dplyr::count(trails_diff, workerID, sort=TRUE) %>%
  filter(n==7)%>%
  dplyr::select(workerID)

trails_diff %>%
  filter(workerID %in% tasks_7comp$workerID) %>%
  dplyr::select(workerID, targetPos, own.cod)

# filtering only workers who completed all 8 tasks
tasks_comp <- dplyr::count(trails_diff, workerID, sort=TRUE) %>%
  filter(n==8)%>%
  dplyr::select(workerID)

trails_comp <- trails_diff %>%
  filter(workerID %in% tasks_comp$workerID) %>%
  dplyr::select(workerID, targetPos, own.cod)

# make sure every targetPos has 2 entries for each subject
trails_comp %>%
  group_by(workerID) %>%
  dplyr::count(targetPos) %>%
  filter(n != 2)

# order by workerID and then tragetPos
tc_ordered <- trails_comp[with(trails_comp, order(workerID, targetPos)), ]
# extend to wide format and naming accordingly
data_with_index <- ddply(tc_ordered, .(workerID), mutate, 
                         index = c('B1','B2','F1','F2','L1','L2','R1','R2')[1:length(workerID)])
df.LCA_id <- dcast(data_with_index, workerID ~ index, value.var = 'own.cod')
# for poLCA we will need only the value column and no zero or negative values
df.LCA <- df.LCA_id %>% 
  dplyr::select(-workerID)
df.LCA <- df.LCA+1


## 5.2 Applying LCA with poLCA

  
f <- with(df.LCA , cbind(B1, B2, F1, F2, L1, L2, R1, R2)~1)

bics <- c()
for (x in 1:8) {
lca_x <- poLCA(f, df.LCA, nclass = x, nrep = 10, verbose=FALSE)$bic 
bics <- append(bics, paste("Classes:",x, "BIC:", lca_x))
}
# 3 Class model with the best BIC
bics
# printing output and graph
lca_3 <- poLCA(f, df.LCA, nclass = 3, nrep = 50, graphs = TRUE) 


## 5.3 Applying LCA classes to tendency plots

df.LCA_id_class <- df.LCA_id %>%
  mutate('lca.class' = factor(lca_3$predclass))%>%
  dplyr::select(workerID, lca.class)

responders <- clean_ex %>%
  dplyr::select(workerID, responderType)%>%
  distinct()%>%
  filter(workerID %in% df.LCA_id_class$workerID)

lca_tend <- df %>%
  filter(workerID %in% df.LCA_id_class$workerID) %>%
  merge(df.LCA_id_class, by='workerID',all.x=TRUE)%>%
  merge(responders, by='workerID', all.x=TRUE)

# LCA could retrieve the tendencies ...
ggplot(lca_tend, aes(lr_tend, fb_tend, color=lca.class))+
  geom_jitter(width=0.1, height=0.1)
# and it matches almost to the clusters in LPA for rate of egocentrism
# from the experiment paper
ggplot(lca_tend, aes( lr_tend,fb_tend, color=responderType))+
  geom_jitter(width=0.1, height=0.1)

# LCA classifies 4 outlier mixed responders differently and allocates them to
# othercentrics. This makes it harder to interpret the classes as egocentric,
# othercentric and mixed responders. 


## Interlude for data_gen
# proportion of tendencies
df %>%
  dplyr::filter(workerID %in% tasks_comp$workerID)%>%
  count(fb_tend)

df %>%
  dplyr::filter(workerID %in% tasks_comp$workerID)%>%
  count(lr_tend)

#### 6 LMEM


## 6.1 Specifying the model

# Create an lmem with targetPos as a random slope for workerID.
# Such that the individual changes in reaction for the subjects are computed.
lmem_dat <- trails_comp %>%
   mutate_if(is.character, as.factor)

summary(m0 <- lmer(own.cod ~ targetPos + (targetPos | workerID), data=lmem_dat))
summary(m1 <- lmer(own.cod ~ targetPos + (1 | workerID), data=lmem_dat))



summary(m0 <- glmer(own.cod ~ targetPos + (targetPos | workerID), data=lmem_dat, family = binomial()))

## 6.2 Grouping the coefficients
# The slope coefficients, Back is the intercept, because of alphabetical ordering
# Since other.cod is the DV 0 means egocentrical choice and 1 means othercentrical
# A value of 0.95 for F is the predicted value of other.cod for the respective subject.
# We can intrepret this as tendency
coeffs <- coef(m0)$workerID
coeffs$F <- coeffs$`(Intercept)`+coeffs$targetPosF
coeffs$B <- coeffs$`(Intercept)`
coeffs$L <- coeffs$`(Intercept)`+coeffs$targetPosL
coeffs$R <- coeffs$`(Intercept)`+coeffs$targetPosR

# Grouping to FB and LR by taking the mean of the components
coeffs$FB <- (coeffs$F+coeffs$B)/2
coeffs$LR <- (coeffs$L+coeffs$R)/2

# Getting the tendency by averaging again. Called "other" to interpert for other-
# centric tendency
coeffs$other = (coeffs$FB+coeffs$LR)/2

# simple classification into 3 classes. [0, .25] Class 0 ; (.25, .75] class 1 ; >.75 class 2
# Drawback: the number of classes are not automatically determined.
hist(coeffs$other)
coeffs$naive_class = ifelse(coeffs$other > 0.25, ifelse(coeffs$other > 0.6, 2, 1), 0)
coeffs$naive_class <- factor(coeffs$naive_class)
ggplot(lca_tend, aes( lr_tend,fb_tend, color=coeffs$naive_class))+
  geom_jitter(width=0.1, height=0.1)

## 6.3. Histograms and sum-approach
# Maybe just adding up everything also give es good classification basis
coeffs$sum <- coeffs$`(Intercept)`+coeffs$targetPosR+coeffs$targetPosF+coeffs$targetPosL
coeffs$sum <- coeffs$sum -min(coeffs$sum)
coeffs$sum <- coeffs$sum/max(coeffs$sum)
# Histograms show tri-modal distribution
hist(coeffs$sum)

# LPA on the "sum" or "other" column or  with 3 classes gives a nice separation,
# but with an important difference:
coeffs$sum %>%
single_imputation() %>%
  estimate_profiles(3)%>% 
  plot_profiles(alpha_range = c(0, 0.6))

coeffs$other %>%
  single_imputation() %>%
  estimate_profiles(3)%>% 
  plot_profiles(alpha_range = c(0, 0.6))


### plot for seminar

require(lattice)
qqmath(ranef(m1, condVar=TRUE))
dotplot(ranef(m1, condVar=TRUE))
## 6.4 Applying LPA on the random slopes coefficients to find classes


# For separate targetPos, AHP suggests 3 classes which results in a bad FB-LR tend plot
coeffs %>%
  dplyr::select(F, B, L, R) %>%
  single_imputation() %>%
  estimate_profiles(1:8)%>% 
  compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik"))

lpa_bflr_cl_3_mod <-coeffs %>%
  dplyr::select(F, B, L, R) %>%
  single_imputation() %>%
  estimate_profiles(3)

lpa_bflr_cl_3 <- get_data(lpa_bflr_cl_3_mod)$Class %>%
  factor()

ggplot(lca_tend, aes( lr_tend,fb_tend, color=lpa_bflr_cl_3))+
  geom_jitter(width=0.1, height=0.1)

# FB and LR aggregated give a better repesentation for the plot

coeffs %>%
  dplyr::select(FB, LR) %>%
  single_imputation() %>%
  estimate_profiles(1:9)%>% 
  compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik")) 
# 7 classes with best BIC withour warning
coeffs %>%
  dplyr::select(FB, LR) %>%
  single_imputation() %>%
  estimate_profiles(6) %>%
  plot_profiles(alpha_range = c(0, 0.6))



lpa_FL_cl_7_mod <-coeffs %>%
  dplyr::select(FB, LR) %>%
  single_imputation() %>%
  estimate_profiles(6)

lpa_FL_cl_7 <- get_data(lpa_FL_cl_7_mod)$Class %>%
  factor()

ggplot(lca_tend, aes(lr_tend,fb_tend, color=lpa_FL_cl_7))+
  geom_jitter(width=0.1, height=0.1)


# When the plot uses the new classes for "sum", the interpretation becomes
# difficult. Lowest BIC without warning is 3 classes but AHP suggests only 1
# Result for 3 ist very unintuative
coeffs$sum %>%
  single_imputation() %>%
  estimate_profiles(1:8)%>% 
  compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik"))

lmm_lpa_sum <- coeffs$sum %>%
  single_imputation() %>%
  estimate_profiles(4)

lmm_lpa_sumclass <- get_data(lmm_lpa_sum)$Class %>%
  factor()

ggplot(lca_tend, aes( lr_tend, fb_tend,color=lmm_lpa_sumclass))+
  geom_jitter(width=0.1, height=0.1)

# It works better with the "other" column:
# 4 is suggested but results in a class with only one subject
# 3 and 5 are good alternatives
coeffs$other %>%
  single_imputation() %>%
  estimate_profiles(1:10) %>%
  compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik"))

coeffs$other %>%
  single_imputation() %>%
  estimate_profiles(4) %>%  
  plot_profiles(alpha_range = c(0, 0.6))

lmm_lpa_other <- 
  coeffs$other %>%
  single_imputation() %>%
  estimate_profiles(5)

lmm_lpa_otherclass <- get_data(lmm_lpa_other)$Class %>%
  factor()

coeffs$other_lpaclass <- get_data(lmm_lpa_other)$Class %>%
  factor()

ggplot(lca_tend, aes(lr_tend, fb_tend, color=coeffs$other_lpaclass))+
  geom_jitter(width=0.1, height=0.1)

## 6.5 Summary

# The "sum" grouping does not account for FB and LR differences that are definitely
# present in the data. The "other" grouping shows the pattern we have already
# observed and that is natural to interpret


#### 7 Bayesian Linear Mixed Effect Models
library(brms)
?brms
library(yaml)
#install.packages("tidypredict")
library(tidypredict)

## 7.1 Specifying the BMEM

# trying uninformed
uninformed <- 
  brm(data = lmem_dat,
  family = gaussian(),
  own.cod ~ targetPos + (targetPos|workerID),
  prior = c(prior(uniform(-20,20), class = Intercept),
  prior(uniform(-5, 5), class = b),
  prior(uniform(-2, 2), class = sigma)),
  seed = 123,
  iter = 4000, warmup = 2000, chains = 4, cores = 4,
  file = "uninformed")
plot(uninformed)
summary(uninformed)

# uninformed priors are too weak to let the model converge
# what about normal priors with a little more insight 
normal_priors <- 
  brm(data = lmem_dat,
      family = gaussian(),
      own.cod ~ targetPos + (targetPos|workerID),
      prior = c(prior(normal(-5, 5), class = Intercept),
                prior(normal(-5, 5), class = b),
                prior(normal(0, 1), class = sigma)),
      seed = 123,
      iter = 4000, warmup = 2000, chains = 4, cores = 4,
      file = "normal_priors")
plot(normal_priors)
summary(normal_priors)


principled <- 
  brm(data = lmem_dat,
      family = gaussian(),
      own.cod ~ targetPos + (targetPos | workerID),
      prior = c(prior(normal(0, 1), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 8000, warmup = 4000, chains = 4, cores = 8,
      seed = 123,
      file = "principled")
plot(principled)
summary(principled)
pp_check(principled)
#standard model
bm0 <- brm(data = lmem_dat, own.cod ~ targetPos + (targetPos | workerID), cores = 8)
summary(bm0)
plot(bm0)
#flat priors
prior_summary(bm0)
pp_check(bm0)

bm1 <- brm(data = lmem_dat, own.cod ~ targetPos + (targetPos | workerID),
           prior = c(prior(normal(0.5,3), class = Intercept),
                     prior(normal(0,2), class = b),
                     prior(normal(0, 1), class = sd)),
           seed = 123,
           cores = 8, iter = 4000, warmup = 2000)

summary(bm1)
pp_check(bm1, ndraws = 100)

bm1_bernoulli <- brm(data = lmem_dat, own.cod ~ targetPos + (targetPos | workerID),
           family = bernoulli(),
           seed = 123,
           cores = 8,
           iter = 4000, warmup = 2000)

summary(bm1_bernoulli)
pp_check(bm1_bernoulli,ndraws = 100)
plot(bm1_bernoulli)

bm2_bernoulli <- brm(data = lmem_dat, own.cod ~ targetPos + (targetPos | workerID),
            family = bernoulli(),
            prior = c(prior(normal(0.5,3), class = Intercept),
                      prior(normal(0,2), class = b),
                      prior(normal(0, 1), class = sd)),
            seed = 123,
            cores = 8,
            iter = 4000, warmup = 2000)

summary(bm2_bernoulli)
pp_check(bm2_bernoulli, ndraws = 100)
plot(bm2_bernoulli)


# loading a BMEM model
# t1 <- readRDS("./m4_fit.rds")

## 7.2 Extracting random slopes gaussian model

bmcoeff <- coef(bm1)$workerID
head(bmcoeff)


bmcoeff[1:124,1,1:4]
coeffs <- coeffs %>%
  mutate("B_bmr" = bmcoeff[1:124,1,1]) %>%
  mutate("F_bmr" = bmcoeff[1:124,1,1] + bmcoeff[1:124,1,2]) %>%
  mutate("L_bmr" = bmcoeff[1:124,1,1] + bmcoeff[1:124,1,3]) %>%
  mutate("R_bmr" = bmcoeff[1:124,1,1] + bmcoeff[1:124,1,4])





## 7.3 Finding classes and variable transformation

# LPA for BMRS with 4 Separate variables suggests 2 classes, 
# which is very unfitting
coeffs %>%
  dplyr::select(F_bmr, B_bmr, L_bmr, R_bmr) %>%
  single_imputation() %>%
  estimate_profiles(2:9) %>%
  compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik"))

lpa_bflr_cl_2_mod_bmr <-coeffs %>%
  dplyr::select(F_bmr, B_bmr, L_bmr, R_bmr) %>%
  single_imputation() %>%
  estimate_profiles(2)
  
lpa_bflr_cl_2_bmr <- get_data(lpa_bflr_cl_2_mod_bmr)$Class %>%
  factor()

ggplot(lca_tend, aes(lr_tend,fb_tend,  color=lpa_bflr_cl_2_bmr))+
  geom_jitter(width=0.1, height=0.1)

# Summarizing to FB, LR and own
coeffs <- coeffs %>%
  mutate("FB_bmr" = (B_bmr+F_bmr)/2)%>%
  mutate("LR_bmr" = (L_bmr+R_bmr)/2)%>%
  mutate("own.bmr" = (FB_bmr+LR_bmr)/2)

# the histogram reveals the possibility for 4 classes for the own.bmr variable
hist(coeffs$own.bmr)

# 5 classes has the lowest BIC and no warnings
coeffs %>%
  dplyr::select(own.bmr) %>%
  single_imputation() %>%
  estimate_profiles(1:10)%>%
  compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik"))

bmr.own.mod <- coeffs %>%
  dplyr::select(own.bmr) %>%
  single_imputation() %>%
  estimate_profiles(5)

bmr.own.class <- get_data(bmr.own.mod)$Class %>%
  factor()

hist(coeffs$own.bmr)

ggplot(lca_tend, aes(lr_tend, fb_tend, color = bmr.own.class))+
  geom_jitter(width=0.1, height=0.1)

# For the fine grained FB-LR 3 classes are found
coeffs %>%
  dplyr::select(FB_bmr, LR_bmr) %>%
  single_imputation() %>%
  estimate_profiles(1:10)%>%
  compare_solutions(statistics=c("AIC", "BIC","LogLik")) 

bmr_classes_model_g <- coeffs %>%
  dplyr::select(FB_bmr, LR_bmr) %>%
  single_imputation() %>%
  estimate_profiles(3) 

bmr_classes_g <- get_data(bmr_classes_model_g)$Class %>%
  factor()

ggplot(lca_tend, aes(lr_tend, fb_tend, color = bmr_classes_g))+
  geom_jitter(width=0.1, height=0.1)

## 7.4 Summary

# BMEM tends to extract more diverse slopes and model comparison hints at more
# classes than LMEM did. Fitting process takes much longer compared to LMEM


## 7.5 Extracting random slopes bernoulli model

bmcoeff_g <- coef(bm2_bernoulli)$workerID
head(bmcoeff_g)

bmcoeff_g[1:124,1,1:4]
coeffs <- coeffs %>%
  mutate("B_bmr_g" = bmcoeff_g[1:124,1,1]) %>%
  mutate("F_bmr_g" = bmcoeff_g[1:124,1,1] + bmcoeff_g[1:124,1,2]) %>%
  mutate("L_bmr_g" = bmcoeff_g[1:124,1,1] + bmcoeff_g[1:124,1,3]) %>%
  mutate("R_bmr_g" = bmcoeff_g[1:124,1,1] + bmcoeff_g[1:124,1,4])

## 7.6 Finding classes and variable transformation

# LPA for BMRS with 4 Separate variables suggests 8 classes, 
# more classes may have lower BIC but performance might be inaccurate due to warnings
coeffs %>%
  dplyr::select(F_bmr_g, B_bmr_g, L_bmr_g, R_bmr_g) %>%
  single_imputation() %>%
  estimate_profiles(2:9) %>%
  compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik"))

lpa_bflr_cl_8_mod_bmr_g <-coeffs %>%
  dplyr::select(F_bmr_g, B_bmr_g, L_bmr_g, R_bmr_g) %>%
  single_imputation() %>%
  estimate_profiles(8)

lpa_bflr_cl_8_bmr_g <- get_data(lpa_bflr_cl_8_mod_bmr_g)$Class %>%
  factor()

ggplot(lca_tend, aes(lr_tend,fb_tend,  color=lpa_bflr_cl_8_bmr_g))+
  geom_jitter(width=0.1, height=0.1)

# Summarizing to FB, LR and own
coeffs <- coeffs %>%
  mutate("FB_bmr_g" = (B_bmr_g+F_bmr_g)/2)%>%
  mutate("LR_bmr_g" = (L_bmr_g+R_bmr_g)/2)%>%
  mutate("own.bmr_g" = (FB_bmr_g+LR_bmr_g)/2)

# the histogram reveals the possibility for 4 classes for the own.bmr variable
hist(coeffs$own.bmr_g)

# 5 classes has the lowest BIC and no warnings
coeffs %>%
  dplyr::select(own.bmr_g) %>%
  single_imputation() %>%
  estimate_profiles(1:10)%>%
  compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik"))

bmr.own.mod_g <- coeffs %>%
  dplyr::select(own.bmr_g) %>%
  single_imputation() %>%
  estimate_profiles(5)

bmr.own.class_g <- get_data(bmr.own.mod_g)$Class %>%
  factor()

ggplot(lca_tend, aes(lr_tend, fb_tend, color = bmr.own.class_g))+
  geom_jitter(width=0.1, height=0.1)

# For the fine grained FB-LR 8 classes are found
coeffs %>%
  dplyr::select(FB_bmr_g, LR_bmr_g) %>%
  single_imputation() %>%
  estimate_profiles(1:10)%>%
  compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik")) 

bmr_bern <- coeffs %>%
  dplyr::select(FB_bmr_g, LR_bmr_g) %>%
  single_imputation() %>%
  estimate_profiles(9) 

bmr_bern_classes <- get_data(bmr_bern)$Class %>%
  factor()

ggplot(lca_tend, aes(lr_tend, fb_tend, color = bmr_bern_classes))+
  geom_jitter(width=0.1, height=0.1)



cor(coeffs$B,coeffs$B_bmr)
cor(coeffs$F,coeffs$F_bmr)
cor(coeffs$L,coeffs$L_bmr)
cor(coeffs$R,coeffs$R_bmr)
