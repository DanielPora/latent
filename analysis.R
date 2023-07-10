library(dplyr)
library(ggplot2)
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggcorrplot)
library(tidyverse)
library('corrr')
#install.packages("tidyverse")
library("FactoMineR")
library("factoextra")
library(gridExtra)
#install.packages("tidyLPA")
library(tidyLPA)
#install.packages("poLCA", dependencies = TRUE)
library(poLCA)



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



# 1. Specific clusters regarding to the tests

# Subjects listed with their respective total scores for the 3 tests
subj_tests <- clean %>%
  select("workerID", "aq_score_subset", "opt_score", "stroop_difference")%>%
  distinct()



# 1.1. Normalizing the tests data
subj_tests_norm <- as.data.frame(scale(subj_tests[2:4]))
subj_tests_norm <- subj_tests_norm%>%
  mutate("workerID" = subj_tests$workerID)

data_normalized <- subj_tests_norm %>%
  select("workerID", "aq_score_subset", "opt_score", "stroop_difference")
tests_norm <- data_normalized[,-1]
rownames(tests_norm) <- data_normalized[,1]

ggplot(stack(tests_norm), aes(x = ind, y = values)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  labs(x="Tests", y="Normalized Value") +
  geom_boxplot() 

# 1.2 Plotting densities of each test
ggplot(tests_norm, aes(aq_score_subset))+
  geom_density()

ggplot(tests_norm, aes(opt_score))+
  geom_density()

ggplot(tests_norm, aes(stroop_difference))+
  geom_density()

# 1.3 Scatterplots of pairwise tests 
ggplot(tests_norm, aes(aq_score_subset, opt_score))+
  geom_point()

ggplot(tests_norm, aes(aq_score_subset, stroop_difference))+
  geom_point()

ggplot(tests_norm, aes(stroop_difference, opt_score))+
  geom_point()

corr_matrix <- cor(tests_norm)
corr_matrix

# 1.4 Summary
# The scatter plots don't hint at some correlation, using a correlation 
# matrix to confirm.
# Density of OPT shows a bi-modal distribution. Suggesting that participants
# can be grouped in two optical perspective related groups.
# There doesn't seem to be more of a structure.
 

# 2. Perspective taking preferences

# 2.1. Identify problems for same perspective (s-p) task
problems <- clean %>%
  filter(clean$accuracy == "0")
# Front-Back errors in all s-p trails
fb_p <- problems %>%
  filter(problems$targetPos == "F" |problems$targetPos == "B")
# Left-Right errors in all s-p trails
lr_p <- problems %>%
  filter(problems$targetPos == "L" |problems$targetPos == "R")
#74 FB errors
nrow(fb_p)
#1 LR error
nrow(lr_p)

#Numbers of different and same perspective trials in total
clean %>%
  count(perspective)

#subjects and the number of total FB trails
worker_fb <- clean %>%
  filter(perspective == "same", targetPos == "F" | targetPos == "B") %>%
  count(workerID)
#subjects and the number of errors they made in same perspective FB trails
worker_fb_p <- fb_p %>%
  count(workerID, sort = TRUE) 

#Workers and the proportion of errors they made in FB trails
diff <- left_join(worker_fb_p, worker_fb, by="workerID", suffix=c("_p", "_all"))
diff %>%
  mutate(prop = round(n_p/n_all,2))%>%
  arrange(desc(prop))

# 2.2 Summary
# In 1404 same perspective experiments occurred 1 left-right confusion (<0.1%),
# which is neglect able, and 74 front-back confusions (=5.3%)
# This suggest that in about 5% of the cases for a "different" perspective 
# front-back scenario the own-other perspective classification is not correct.
# In particular we can see that half of the participants who confused front-back
# did it systematically. Therefore it's suggested to invert the interpretation
# for their own.cod and other.cod entries or remove them from the analysis.

exclusions = diff$workerID
clean_ex <- clean %>%
  filter(!(workerID %in% exclusions)) 
#Leaving out 412 observations

# 3 Individual perspective preference


# 3.1 Investigating "different" perspective scenarios

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

# 3.2 Visualizing


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


# 3.3 Summary

# It is surprising that reference for perspective taking seems to be dependent on
# the direction. From the histograms we can see a strong egocentric tendency in 
# left-right trials and a medium tendency for a othercentric perspective in 
# front-back trials.


# 4 LCA Perspective preference

# Considered are only different perspective items grouped for front-back (FB)
# and left-right (LR) 

# 4.1 Plotting the tendencies
by_subj_lr <- by_subj_lr %>%
  filter(workerID %in% by_subj_fb$workerID)
  
df <- data.frame(by_subj_lr$workerID,'fb_tend'=by_subj_fb$own_tendency,
                 'lr_tend'=by_subj_lr$own_tendency, 'aq'=by_subj_lr$aq_score,
                 'opt' =by_subj_lr$opt_score, 'stroop'=by_subj_lr$stroop_difference)

ggplot(df, aes(fb_tend, lr_tend, color=opt))+
  geom_jitter(width=0.1, height=0.1)+
  scale_colour_gradientn(colours=rainbow(4))

# 4.2 LCA with poLCA

trails_diff <- clean_ex %>%
  filter(clean_ex$perspective == 'different')

# Different number of trails for subjects make an LCA encoding hard.
# 28 of 153 participant did not complete all different perspective 8 trails.
count(trails_diff, workerID, sort=TRUE)






lmer(own.cod ~targetPos + opt_score_total + (age|opt_score_total), data=trails_diff) %>%
  summary()

data("USArrests")      # Loading the data set
df_bsp <- scale(USArrests) # Scaling the data

# View the first 3 rows of the data
head(df, n = 3)

res_kmean <- kmeans(df[c('opt')], 2, iter.max = 120, nstart = 10)

cluster <- res_kmean$cluster %>%
  factor()

df_km <- df %>%
  mutate('kmean_class' = cluster)

ggplot(df_km, aes(fb_tend, lr_tend, color=kmean_class))+
  geom_jitter(width=0.1, height=0.1)
  


by_subj <- clean %>%
  group_by(workerID) %>%
  summarise(resp_all = mean(respTime), sd_all = sd(respTime), own_sum_all = sum(own.cod), other_sum_all = sum(other.cod))

by_subj_same <- clean %>%
  filter(perspective == "same") %>%
  group_by(workerID) %>%
  summarise(resp_same = mean(respTime), sd_same = sd(respTime), own_sum_same = sum(own.cod), other_sum_same = sum(other.cod))


by_subj <- mutate(by_subj, total_sum_all = own_sum_all+other_sum_all)
by_subj <- mutate(by_subj, direction_all = (own_sum_all-other_sum_all)/total_sum_all)

by_subj_diff <- mutate(by_subj_diff, total_sum_diff = own_sum_diff+other_sum_diff)
by_subj_diff <- mutate(by_subj_diff, direction_diff = (own_sum_diff-other_sum_diff)/total_sum_diff)

by_subj_same <- mutate(by_subj_same, total_sum_same = own_sum_same+other_sum_same)
by_subj_same <- mutate(by_subj_same, direction_same = (own_sum_same-other_sum_same)/total_sum_same)




direction_density <- density(by_subj$direction_all)
direction_density_diff <- density(by_subj_diff$direction_diff)
direction_density_same <- density(by_subj_same$direction_same)


plot(direction_density_diff, ylim = c(0,2), col = "blue")
lines(direction_density, col = "red")
lines(direction_density_same, col = "green")
legend("topleft", legend = c("All trials", "same", "different"),
       lwd = 3, col = c("red", "green", "blue"))


hist(by_subj$direction_all, prob = TRUE, xlim = c(-1,1), ylim = c(0,10), col = "blue", breaks = 9)
hist(by_subj_diff$direction_diff, prob = TRUE, add=TRUE, col = "red", breaks = 9)
hist(by_subj_same$direction_same, prob = TRUE, add=TRUE, col = "green", breaks = 9)

ggplot(by_subj, aes(direction_all))+
  geom_histogram()
  




subj_tests <- clean %>%
  select("workerID", "aq_score_subset", "opt_score", "stroop_difference")%>%
  distinct()




  
  
ggplot(subj_tests_norm, aes(aq_score_subset, opt_score, size = stroop_difference))+
  geom_point()

data_normalized <- subj_tests_norm %>%
  select("workerID", "aq_score_subset", "opt_score", "stroop_difference")
d2 <- data_normalized[,-1]
rownames(d2) <- data_normalized[,1]

corr_matrix <- cor(d2)
corr_matrix



data.pca <- princomp(corr_matrix)
summary(data.pca)


 


res.pca <- PCA(d2, graph = TRUE)

ggplot(d2, aes(aq_score_subset))+
  geom_density()

ggplot(d2, aes(opt_score))+
  geom_density()

ggplot(d2, aes(stroop_difference))+
  geom_density()

ggplot(d2, aes(aq_score_subset, opt_score))+
  geom_point()

ggplot(d2, aes(aq_score_subset, stroop_difference))+
  geom_point()

ggplot(d2, aes(stroop_difference, opt_score))+
  geom_point()

pca <- prcomp(d2)
rotated <- as.data.frame(scale(d2, pca$center, pca$scale) %*% pca$rotation)

ggplot(d2, aes(aq_score_subset, opt_score, size = stroop_difference))+
  geom_point()

ggplot(rotated, aes(PC1, PC2, size = PC3))+
  geom_point()

pca2 <- prcomp(d2[c("stroop_difference", "aq_score_subset")])
rotated2 <- as.data.frame(scale(d2[c("stroop_difference", "aq_score_subset")], pca2$center, pca2$scale) %*% pca2$rotation)

summary(pca2)
summary(pca)

ggplot(rotated2, aes(PC1, PC2))+
  geom_point()
