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
subj_tests <- clean %>%
  select("workerID", "aq_score_subset", "opt_score", "stroop_difference")%>%
  distinct()

write.csv(subj_tests, "subj_test.csv", row.names = FALSE)

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
  labs(x="Test", y="Normalized Value") +
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
# There doesn't seem to be more of a structure. PCA was done in a separated
# analysis, but without any insightful result.
 

# 2. Perspective taking preferences

# 2.1. Identify problems for same perspective task
problems <- clean %>%
  filter(clean$accuracy == "0")
# Front-Back problems
fb_p <- problems %>%
  filter(problems$targetPos == "F" |problems$targetPos == "B")
# Left-Right problems
lr_p <- problems %>%
  filter(problems$targetPos == "L" |problems$targetPos == "R")
nrow(fb_p)
nrow(lr_p)

clean %>%
  count(perspective)

worker_fb_p <- fb_p %>%
  count(workerID, sort = TRUE) 

worker_fb <- clean %>%
  filter(perspective == "same", targetPos == "F" | targetPos == "B") %>%
  count(workerID)

diff <- left_join(worker_fb_p, worker_fb, by="workerID", suffix=c("_p", "_all"))
diff %>%
  mutate(prop = round(n_p/n_all,2))%>%
  arrange(desc(prop))

# 2.2 Summary
# In 1404 same perspective experiments occurred 1 left-right confusion (>0.001),
# which is neglect able, and 74 front-back confusions (=0.05)
# This suggest that in about 5% of the cases for a "different" perspective 
# front-back scenario the own-other perspective classification is not correct.
# In particular we can see that half of the participants who confused front-back
# did it systematically. Therefore it's suggested to invert the interpretation
# for their own.cod and other.cod entries


# 3 Individual perspective preference

# Individual perspective preference is in direct relation to the interpretation 
# of LCA yielded groups

# 3.1 Relevant here are the "different" perspective scenarios

# Using front-back, left-right... 
fb <- clean %>%
  filter(perspective == "different", targetPos == "F" | targetPos == "B")

lr <- clean %>%
  filter(perspective == "different", targetPos == "L" | targetPos == "R")

# ... and all scenarios together to analyse the perspective preference by subject
# own_tendency describes here whether the participant preferred the egocentric
# perspective versus the othercentric perspective.
# 1 = always egocentric, 0 = 50/50, -1 = always othercentric
by_subj_diff <- clean %>%
  filter(perspective == "different") %>%
  group_by(workerID) %>%
  summarise(respTime_mean = mean(respTime), respTime_sd = sd(respTime), 
            own_sum_diff = sum(own.cod), other_sum_diff = sum(other.cod),
            own_tendency = (sum(own.cod)-sum(other.cod))/(sum(own.cod)+sum(other.cod)),
            aq_score = mean(aq_score_subset), 
            opt_score = mean(opt_score_total),stroop_difference = mean(stroop_difference))


by_subj_fb <- fb %>%
  group_by(workerID) %>%
  summarise(respTime_mean = mean(respTime), respTime_sd = sd(respTime), 
            own_sum_diff = sum(own.cod), other_sum_diff = sum(other.cod),
            own_tendency = (sum(own.cod)-sum(other.cod))/(sum(own.cod)+sum(other.cod)),
            aq_score = mean(aq_score_subset), 
            opt_score = mean(opt_score_total),stroop_difference = mean(stroop_difference))

by_subj_lr <- lr %>%
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
# left-right tests and a medium tendency for a othercentric perspective in 
# front-back tests.
# Again, this could also be the result of some participant being
# confused about where the front and back in the experiment from their own 
# perspective was. This could explain why for the combined setting the counts
# even out a bit. 










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
