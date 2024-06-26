library(dplyr)
library(ggplot2)
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#################
# Functions
#
#


make_subj <- function(class, obs_per_trial) {
  # creates a subject of a given class and with number obs_per_trial for each targetPos
  
  ## Creating a class probability distributions
  # From what we could see from the analysis is that there seem to be certain types
  # for perspective choices also depending on FB and LR trials
  # In this work, it is assumed that each subject has an individual percentage to
  # take an egocentric perspective for a given FB or LR trial. 
  # The underlying probabilities for a subject of a certain class, to choose ego-
  # centric for front-back and. Each list entry represents a class and works as follows:
  # c( P(egocentric|FB), P(egocentric|LR) )
  # Assumed are 5 type probabilities: none, low, middle, high and full; representing
  # the probability that a subject uses a egocentric perspective for a trial
  
  # type probabilities with individual difference
  diff_FB <- rnorm(1,0,0.05)
  diff_LR <- rnorm(1,0,0.05)
  
  FB_none <- ifelse(diff_FB<0, 0 ,diff_FB)
  FB_low <- ifelse(0.25+diff_FB<0, 0, 0.25+diff_FB)
  FB_mid <- 0.5+diff_FB
  FB_high <- ifelse(0.75+diff_FB>1, 1, 0.75+diff_FB)
  FB_full <- ifelse(diff_FB+1>1, 1, 1+diff_FB)
  
  LR_none <- ifelse(diff_LR<0, 0 ,diff_LR)
  LR_low <- ifelse(0.25+diff_LR<0, 0, 0.25+diff_LR)
  LR_mid <- 0.5+diff_LR
  LR_high <- ifelse(0.75+diff_LR>1, 1, 0.75+diff_LR)
  LR_full <- ifelse(diff_LR+1>1, 1, 1+diff_LR)
  
  # this results in 25 possible combination of classes
  class_prob <- 
    list(c(FB_full, LR_none), c(FB_full, LR_low), c(FB_full, LR_mid), c(FB_full, LR_high), c(FB_full, LR_full),
         c(FB_high,LR_none),  c(FB_high, LR_low), c(FB_high, LR_mid), c(FB_high, LR_high), c(FB_high, LR_full),
         c(FB_mid,LR_none),   c(FB_mid, LR_low),  c(FB_mid, LR_mid),  c(FB_mid, LR_high),  c(FB_mid, LR_full),
         c(FB_low, LR_none),  c(FB_low, LR_low),  c(FB_low, LR_mid),  c(FB_low, LR_high),  c(FB_low, LR_full),
         c(FB_none, LR_none), c(FB_none, LR_low), c(FB_none, LR_mid), c(FB_none, LR_high), c(FB_none, LR_full))

  subj <- c()
  nam <- c()
  true_FB <- class_prob[[class]][1]
  true_LR <- class_prob[[class]][2]
  
  for (pos in c('B', 'F', 'L', 'R')){
    prob <- ifelse(pos == 'B' | pos == 'F', true_FB, true_LR)
    for (i in 1:obs_per_trial){
      vec <- rbinom(1, 1,prob)
      nam <- cbind(nam, paste(pos,i,sep = ""))
      subj <- cbind(subj,vec)
    }
  }
  
  subj <- cbind(subj, class)
  subj <- cbind(subj, true_FB)
  subj <- cbind(subj, true_LR)
  nam <- cbind(nam, 'true_class')
  nam <- cbind(nam, 'true_FB')
  nam <- cbind(nam, 'true_LR')
  colnames(subj)<- nam
  
  return(subj)
}
gen_data <- function(size, obs_per_trial, c_dist){
  # generate the data with given parameters and adds a column true class
  # size : how many subjects will be generated
  # obs_per_trial : how many trails from the same perspective for each subject
  # c_dist : Probability-list of subject being in a certain class (influences number points per cluster)
  # c_prob : probabilities for egocentric choice for the classes (influences where the clusters are)
  c_dist <- c_dist/sum(c_dist)
  n_classes <- length(c_dist)
  subject_classes <- sample(seq(25) , size, replace=TRUE, prob=c_dist)
  df <- data.frame(matrix(ncol = 4*obs_per_trial, nrow = 0))  
  
  for (class in subject_classes){
    subj <- make_subj(class, obs_per_trial)
    #subj <- c(subj, class)
    df <- rbind(df,subj)
  }
  
  df<- df %>%
    mutate('ego.tend' = rowSums(df)/(4*obs_per_trial))%>%
    mutate('FB.tend' = rowSums(df[1:(2*obs_per_trial)])/(2*obs_per_trial))%>%
    mutate('LR.tend' = rowSums(df[(2*obs_per_trial+1):(4*obs_per_trial)])/(2*obs_per_trial))
  df$true_class <- factor(df$true_class)  
  return(df)
}






make_subj_FB_LR <- function(indx, class, obs_per_trial) {
  # creates a subject of a given class and with number obs_per_trial for each targetPos
  
  ## Creating a class probability distributions
  # From what we could see from the analysis is that there seem to be certain types
  # for perspective choices also depending on FB and LR trials
  # In this work, it is assumed that each subject has an individual percentage to
  # take an egocentric perspective for a given FB or LR trial. 
  # The underlying probabilities for a subject of a certain class, to choose ego-
  # centric for front-back and. Each list entry represents a class and works as follows:
  # c( P(egocentric|FB), P(egocentric|LR) )
  # Assumed are 5 type probabilities: none, low, middle, high and full; representing
  # the probability that a subject uses a egocentric perspective for a trial
  
  # type probabilities with individual difference
  diff_FB <- rnorm(1,0,0.05)
  diff_LR <- rnorm(1,0,0.05)
  
  FB_none <- ifelse(diff_FB<0, 0 ,diff_FB)
  FB_low <- ifelse(0.25+diff_FB<0, 0, 0.25+diff_FB)
  FB_mid <- 0.5+diff_FB
  FB_high <- ifelse(0.75+diff_FB>1, 1, 0.75+diff_FB)
  FB_full <- ifelse(diff_FB+1>1, 1, 1+diff_FB)
  
  LR_none <- ifelse(diff_LR<0, 0 ,diff_LR)
  LR_low <- ifelse(0.25+diff_LR<0, 0, 0.25+diff_LR)
  LR_mid <- 0.5+diff_LR
  LR_high <- ifelse(0.75+diff_LR>1, 1, 0.75+diff_LR)
  LR_full <- ifelse(diff_LR+1>1, 1, 1+diff_LR)
  
  # this results in 25 possible combination of classes
  class_prob <- 
    list(c(FB_full, LR_none), c(FB_full, LR_low), c(FB_full, LR_mid), c(FB_full, LR_high), c(FB_full, LR_full),
         c(FB_high,LR_none),  c(FB_high, LR_low), c(FB_high, LR_mid), c(FB_high, LR_high), c(FB_high, LR_full),
         c(FB_mid,LR_none),   c(FB_mid, LR_low),  c(FB_mid, LR_mid),  c(FB_mid, LR_high),  c(FB_mid, LR_full),
         c(FB_low, LR_none),  c(FB_low, LR_low),  c(FB_low, LR_mid),  c(FB_low, LR_high),  c(FB_low, LR_full),
         c(FB_none, LR_none), c(FB_none, LR_low), c(FB_none, LR_mid), c(FB_none, LR_high), c(FB_none, LR_full))
  
  
  
  subj <- c()
  cols <- c()
  nam <- cbind('Subject','trial_type','own.cod', 'true_class', 'true_FB', 'true_LR')
  true_FB <- class_prob[[class]][1]
  true_LR <- class_prob[[class]][2]
  
  
  for (pos in c('FB', 'LR')){
    prob <- ifelse(pos == 'FB', true_FB, true_LR)
    for (i in 1:(obs_per_trial*2)){
      
      own.cod <- rbinom(1, 1, prob)
      subj <- cbind(indx,pos,own.cod, class, round(true_FB, 3), round(true_LR,3))
      cols <- rbind(cols, subj)
    }
  }
  

  colnames(cols)<- nam
  
  return(cols)
}

gen_data_FB_LR <- function(size, obs_per_trial, c_dist){
  # generate the data with given parameters and adds a column true class
  # size : how many subjects will be generated
  # obs_per_trial : how many trails from the same perspective for each subject
  # c_dist : Probability-list of subject being in a certain class (influences number points per cluster)
  # c_prob : probabilities for egocentric choice for the classes (influences where the clusters are)
  c_dist <- c_dist/sum(c_dist)
  n_classes <- length(c_dist)
  subject_classes <- sample(seq(25) , size, replace=TRUE, prob=c_dist)
  df <- data.frame(matrix(ncol = 4*obs_per_trial, nrow = 0))  
  
  indx = 1
  for (class in subject_classes){
    subj <- make_subj_FB_LR(indx,class, obs_per_trial)
    #subj <- c(subj, class)
    df <- rbind(df,subj)
    indx <- indx + 1
  }
  
 " df<- df %>%
    mutate('ego.tend' = rowSums(df)/(4*obs_per_trial))%>%
    mutate('FB.tend' = rowSums(df[1:(2*obs_per_trial)])/(2*obs_per_trial))%>%
    mutate('LR.tend' = rowSums(df[(2*obs_per_trial+1):(4*obs_per_trial)])/(2*obs_per_trial))
  df$true_class <- factor(df$true_class)  "
  return(df)
}

#
#
############


# The classes should also vary in size and in some cases they are not present at all.
# Class distribution reflects the proportion or size of the classes in the population
# class_dist will be scaled later so can also be of ratio
#(1,1,1,1,2,
# 2,2,2,4,2,
# 1,1,4,2,3,
# 0,1,0,2,2,
# 1,2,3,4,5) 
# Classes 5,6,7 or 8 have on average double the amount of subjects of classes 1,2,3 or 4
# Class 9 is 4 times as big as classes 1,2,3 or 4 and so on.

# The class centroids are on the FB-LR-trend graph are located like this
#
#    1  2  3  4  5
#    6  7  8  9 10
#   11 12 13 14 15
#   16 17 18 19 20
#   21 22 23 24 25
# 
# Trying to recreate the class sizes from the experiment
#                
class_dist <- c( 1,  0,  0,  0,  1,
                 0,  0,  0,  0,  0,
                 0,  0,  0,  0,  0,
                 0,  0,  0,  0,  0,
                 1,  0,  0,  0,  1)

# number of classes
sum(class_dist != 0)

# generate the data with numbers from the experiment
n_subjects = 50
obs_per_trial = 2



test_df <- gen_data_FB_LR(n_subjects, obs_per_trial, class_dist)

write.csv(test_df, "test_df_FB_LR.csv", row.names=FALSE)

# columns explained:
# B1 - R_ : the trials determined by obs_per_trial with values 0 for othercentric
# perspective choice and 1 for egocentric perspective choice
#
# true_class : underlying class for the subject it was simulated from
#
# diff : individual tendency towards egocentric choices. (negatve means tendency to 
# othercentric choices)

# Plot with true classes
ggplot(test_df, aes(LR.tend,FB.tend, color=true_class)) +
  geom_jitter(width=0.05, height=0.05)+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)

ggplot(test_df, aes(shape = true_class, alpha = 0.7))+
  geom_jitter(aes(LR.tend, FB.tend, color = 'Generated (Jitter)'), width=0.04, height=0.04)+
  geom_point(aes(true_LR,true_FB, color = 'True Probabilities'))+
  labs(title = '2 Observations per trial type')



ggplot(test_df, aes(color = true_class, alpha = 0.9))+
  geom_jitter(aes(LR.tend, FB.tend, shape = 'Generated (Jitter)'), width=0.03, height=0.03)+
  geom_point(aes(true_LR,true_FB, shape = 'True Probabilities'))+
  labs(title = '2 Observations per trial type, 400 Subjects')


ggplot(test_df, aes(color = true_class, alpha = 0.9))+
  geom_jitter(aes(LR.tend, FB.tend, shape = 'Generated (Jitter)'), width=0.03, height=0.03)+

  labs(title = '2 Observations per trial type, 400 Subjects')

######
# Influence of parameters with the following class distribution.
# It's a medium difficult setting with slightly overlapping but not directly
# neighboring clusters

class_dist_animation <-
  c( 0,  0,  1,  0,  0,
     0,  1,  0,  1,  0,
     0,  0,  1,  0,  0,
     0,  1,  0,  1,  0,
     0,  0,  1,  0,  0)

## Observations per trial
for (obs in c(1,2,4,10,20)){
  data_animation_o <- gen_data(n_subjects, obs, class_dist_animation)
  Sys.sleep(1) # give the cpu time to build the datasets
  ggo <- ggplot(data_animation_o, aes(LR.tend,FB.tend, color=true_class)) +
    geom_jitter(width=0.1/obs, height=0.1/obs)+
    xlim(-0.1,1.1)+
    ylim(-0.1,1.1)+
    labs(title = paste(obs, 'Observation per trial'))
  print(ggo)
  Sys.sleep(0.5)
}
# 1 obs. per trial make mixed clusters. Any clustering or latent class algorithm would
# classify subject often into the wrong clusters. More obs. lower the mixing in
# a cluster

## Number of subjects
for (n_subjects_s in c(100, 200, 500, 1000)){
  data_animation_s <- gen_data(n_subjects_s, obs_per_trial, class_dist_animation)
  Sys.sleep(1) # give the cpu time to build the datasets
  ggs <- ggplot(data_animation_s, aes(LR.tend,FB.tend, color=true_class)) +
    geom_jitter(width=0.1/obs_per_trial, height=0.1/obs_per_trial)+
    xlim(-0.1,1.1)+
    ylim(-0.1,1.1)+
    labs(title = paste(n_subjects, 'Subjects'))
  print(ggs)
  Sys.sleep(0.5)
}
# More subjects do not help with correctly identifying a true class, but can
# still help with identifying the position of centroids


## Combination of both
for (obs_c in c(1,2,5,10, 20)){
  for (n_subjects_c in c(100, 200, 500, 1000, 2000)){
    data_animation_c <- gen_data(n_subjects_c, obs_c, class_dist_animation)
    Sys.sleep(1) # give the cpu time to build the datasets
    ggc <- ggplot(data_animation_c, aes(LR.tend,FB.tend, color=true_class)) +
      geom_jitter(width=0.1/obs_c, height=0.1/obs_c)+
      xlim(-0.1,1.1)+
      ylim(-0.1,1.1)+
      labs(title = paste(n_subjects_c, 'Subjects and', obs_c, 'Observations' ))
    print(ggc)
    Sys.sleep(0.5)
  }
}
  
### Scenarios

# To test the ability of a method to find clusters, different scenarios will be
# created. They are categorized by the amount of mixing. Noise will be introduced
# either via using more classes with mid, low and high chances or by changing
# the values for type probabilities. 

n_subjects_sc = 200 # number of subjects for the scenarios
obs_per_trial_sc <- 2 # number of obs. per trial for the scenarios

# Easy scenarios with increasing difficulty
## Subjects have clear tendencies, cluster centroids far apart, 3-4 classes,
## same class sizes

# Easy1
sc_name <- 'easy1'
class_dist_easy1 <-
  c( 1,  0,  0,  0,  1,
     0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,
     1,  0,  0,  0,  1)

data_easy1 <- gen_data(n_subjects_sc, obs_per_trial_sc, class_dist_easy1)
ggplot(data_easy1, aes(LR.tend,FB.tend, color=true_class)) +
  geom_jitter(width=0.03, height=0.03)+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)+
  labs(title = sc_name)

write.csv(data_easy1, paste("./data_obs",obs_per_trial_sc,"_nSubj",n_subjects_sc,"_",sc_name, ".csv", sep=""), row.names=FALSE)

# Easy2
sc_name <- 'easy2'
class_dist_easy2 <-
  c( 0,  1,  0,  0,  0,
     0,  0,  0,  0,  1,
     0,  0,  0,  0,  0,
     1,  0,  0,  0,  0,
     0,  0,  0,  1,  0)

data_easy2 <- gen_data(n_subjects_sc, obs_per_trial_sc, class_dist_easy2)
ggplot(data_easy2, aes(LR.tend,FB.tend, color=true_class)) +
  geom_jitter(width=0.03, height=0.03)+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)+
  labs(title = sc_name)

write.csv(data_easy2, paste("./data_obs",obs_per_trial_sc,"_nSubj",n_subjects_sc,"_",sc_name, ".csv", sep=""), row.names=FALSE)

# Easy3
sc_name <- 'easy3'
class_dist_easy3 <-
  c( 0,  0,  1,  0,  0,
     0,  0,  0,  0,  0,
     1,  0,  0,  0,  1,
     0,  0,  0,  0,  0,
     0,  0,  1,  0,  0)

data_easy3 <- gen_data(n_subjects_sc, obs_per_trial_sc, class_dist_easy3)
ggplot(data_easy3, aes(LR.tend,FB.tend, color=true_class)) +
  geom_jitter(width=0.03, height=0.03)+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)+
  labs(title = sc_name)

write.csv(data_easy3, paste("./data_obs",obs_per_trial_sc,"_nSubj",n_subjects_sc,"_",sc_name, ".csv", sep=""), row.names=FALSE)

# Medium scenarios
## Subjects have mixed tendencies, cluster centroids are sometimes near,
## 4-5 classes, same class sizes 

# medium1
sc_name <- 'medium1'
class_dist_medium1 <-
  c( 0,  0,  0,  0,  7,
     0,  1,  0,  0,  2,
     0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,
     8,  1,  0,  0,  6)

data_medium1 <- gen_data(n_subjects_sc, obs_per_trial_sc, class_dist_medium1)
ggplot(data_medium1, aes(LR.tend,FB.tend, color=true_class)) +
  geom_jitter(width=0.03, height=0.03)+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)+
  labs(title = sc_name)

write.csv(data_medium1, paste("./data_obs",obs_per_trial_sc,"_nSubj",n_subjects_sc,"_",sc_name, ".csv", sep=""), row.names=FALSE)

# medium2
sc_name <- 'medium2'
class_dist_medium2 <-
  c( 0,  1,  0,  0,  1,
     1,  0,  0,  1,  0,
     0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,
     1,  0,  1,  0,  1)

data_medium2 <- gen_data(n_subjects_sc, obs_per_trial_sc, class_dist_medium2)
ggplot(data_medium2, aes(LR.tend,FB.tend, color=true_class)) +
  geom_jitter(width=0.03, height=0.03)+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)+
  labs(title = sc_name)

write.csv(data_medium2, paste("./data_obs",obs_per_trial_sc,"_nSubj",n_subjects_sc,"_",sc_name, ".csv", sep=""), row.names=FALSE)


# medium3
sc_name <- 'medium3'
class_dist_medium3 <-
  c( 0,  1,  0,  0,  0,
     0,  0,  0,  0,  1,
     0,  0,  0,  0,  0,
     1,  0,  2,  0,  0,
     0,  0,  0,  0,  2)

data_medium3 <- gen_data(n_subjects_sc, obs_per_trial_sc, class_dist_medium3)
ggplot(data_medium3, aes(LR.tend,FB.tend, color=true_class)) +
  geom_jitter(width=0.03, height=0.03)+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)+
  labs(title = sc_name)

write.csv(data_medium3, paste("./data_obs",obs_per_trial_sc,"_nSubj",n_subjects_sc,"_",sc_name, ".csv", sep=""), row.names=FALSE)


# hard1
sc_name <- 'hard1'
class_dist_hard1 <-
  c( 1,  1,  0,  0,  0,
     0,  0,  0,  5,  0,
     0,  0,  0,  0,  0,
     0,  3,  0,  0,  0,
     0,  0,  0,  2,  0)

data_hard1 <- gen_data(n_subjects_sc, obs_per_trial_sc, class_dist_hard1)
ggplot(data_hard1, aes(LR.tend,FB.tend, color=true_class)) +
  geom_jitter(width=0.03, height=0.03)+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)+
  labs(title = sc_name)

write.csv(data_hard1, paste("./data_obs",obs_per_trial_sc,"_nSubj",n_subjects_sc,"_",sc_name, ".csv", sep=""), row.names=FALSE)

# hard2
sc_name <- 'hard2'
class_dist_hard2 <-
  c( 0,  1,  0,  1,  0,
     1,  0,  0,  0,  0,
     0,  0,  0,  1,  0,
     0,  1,  0,  0,  1,
     0,  1,  0,  0,  0)

data_hard2 <- gen_data(n_subjects_sc, obs_per_trial_sc, class_dist_hard2)
ggplot(data_hard2, aes(LR.tend,FB.tend, color=true_class)) +
  geom_jitter(width=0.03, height=0.03)+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)+
  labs(title = sc_name)

write.csv(data_hard2, paste("./data_obs",obs_per_trial_sc,"_nSubj",n_subjects_sc,"_",sc_name, ".csv", sep=""), row.names=FALSE)


# hard3
sc_name <- 'hard3'
class_dist_hard3 <-
  c( 0,  0,  0,  1,  0,
     0,  6,  0,  0,  0,
     0,  0,  0,  0,  4,
     0,  0,  5,  0,  0,
     0,  1,  0,  0,  0)

data_hard3 <- gen_data(n_subjects_sc, obs_per_trial_sc, class_dist_hard3)
ggplot(data_hard3, aes(LR.tend,FB.tend, color=true_class)) +
  geom_jitter(width=0.03, height=0.03)+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)+
  labs(title = sc_name)

write.csv(data_hard3, paste("./data_obs",obs_per_trial_sc,"_nSubj",n_subjects_sc,"_",sc_name, ".csv", sep=""), row.names=FALSE)



#
#
##############
#            #
# Evaluation #
#            #
##############
library(fossil)
#install.packages("fossil")
library(poLCA)
#install.packages("aricode")
library(aricode)
#####
#
# Function

eval_lca <- function(df){
  # works only for 2 obs per trial at the moment
  
  lca_data1 <- df
  for (col in 1:ncol(lca_data1)){
    if(colnames(lca_data1[col]) == "true_class") {break}
    lca_data1[col] <- lca_data1[col] + 1
  }
  
  f <- with(lca_data1, cbind(B1, B2, F1, F2, L1, L2, R1, R2)~1)
  bics <- c()
  bics_num <- c()
  best_bic <- Inf
  for (x in 1:8) {
    lca_x <- poLCA(f, lca_data1, nclass = x, nrep = 10, verbose=FALSE)$bic 
    bics_num <- append(bics_num, lca_x)
    bics <- append(bics, paste("Classes:",x, "BIC:", lca_x))
    ifelse(best_bic > lca_x, best_bic <- lca_x, break)
  }
  
  bic_class <- which.min(bics_num)
  
  
  lca_data1_res <- poLCA(f, lca_data1, nclass = bic_class, nrep = 50, graphs = FALSE, verbose=FALSE) 
  
  df <- df %>%
    mutate('lca.class' = factor(lca_data1_res$predclass))
  
  ggpl <- ggplot(df, aes(LR.tend, FB.tend, color = lca.class))+
    geom_jitter()
  print(ggpl)
  # Sys.sleep(0.5)
  
  c(rand.index(as.numeric(df$true_class), as.numeric(df$lca.class)),
    NMI(as.numeric(df$true_class), as.numeric(df$lca.class)))
}


n_subjects_ev = 200 # number of subjects for the scenarios
obs_per_trial_ev <- 2 # number of obs. per trial for the scenarios

rand_vals <- c()
mi_vals <- c()
for (i in 1:5){
  g_data <- gen_data(n_subjects_ev, obs_per_trial_ev, class_dist_medium_2)
  eval_res <- eval_lca(g_data) 
  lca_rand <- eval_res[1]
  lca_mi <- eval_res[2]
  rand_vals <- append(rand_vals, lca_rand)
  mi_vals <- append(mi_vals, lca_mi)
}

rand_vals
mi_vals

g_data <- gen_data(n_subjects_ev, obs_per_trial_ev, class_dist_medium_2)
eval_res <- eval_lca(g_data)


for (i in 1:50) {
  

a <- as.numeric(sample(1:5, 100, replace=TRUE))
b <- as.numeric(sample(1:5, 100, replace=TRUE))
nv <- NMI(a,b)
ri <- rand.index(a,b)

if ( (nv - ri)**2 < 0.6 ){
  re <- data.frame(a,b)
  break
}
}
NMI(a,b)
rand.index(a,b)
