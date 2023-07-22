library(dplyr)
library(ggplot2)


make_subj <- function(class, obs_per_trial) {
  # creates a subject of a given class and with number obs_per_trial for each targetPos
  subj <- c()
  nam <- c()
  for (pos in c('B', 'F', 'L', 'R')){
    p1 <- class_prob[[class]][1]
    p2 <- class_prob[[class]][2]
    prob <- ifelse(pos == 'B' | pos == 'F', p1, p2)
    for (i in 1:obs_per_trial){
      vec <- rbinom(1, 1,prob)
      nam <- cbind(nam, paste(pos,i,sep = ""))
      subj <- cbind(subj,vec)
    }
  }
  subj <- cbind(subj, class)
  nam <- cbind(nam, 'true_class')
  colnames(subj)<- nam
  
  return(subj)
}


gen_data <- function(size, obs_per_trial, c_dist, c_prob){
  # generate the data with given parameters and adds a column true class
  # size : how many subjects will be generated
  # obs_per_trial : how many trails from the same perspective for each subject
  # c_dist : Probability-list of subject being in a certain class (influences number points per cluster)
  # c_prob : probabilities for egocentric choice for the classes (influences where the clusters are)
  c_dist <- c_dist/sum(c_dist)
  subject_classes <- sample(seq(9) , size, replace=TRUE, prob=c_dist)
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

## Creating a class probability distributions
# The underlying probabilities for a subject of a certain class, to choose ego-
# centric for front-back and. Each list entry represents a class and works as follows:
# c( P(egocentric|FB), P(egocentric|LR) )
low <- 0.02
mid <- 0.5
high <- 0.98

class_prob <- list(c(low, low),c(low, mid), c(low, high),
                   c(mid, low), c(mid, mid), c(mid, high),
                   c(high, low), c(high, mid), c(high, high))

# Class distribution reflect the proportion or size of the classes in the population
# class_dist will be scaled later so can also be of ratio (1,1,1,1,2,2,2,2,4) <-
# Classes 5,6,7 or 8 have on average double the amount of subjects of classes 1,2,3 or 4
# Class 9 is 4 times as big as classes 1,2,3 or 4

# The class centroids are on the FB-LR-trend graph are located like this
#   3 6 9
#   2 5 8
#   1 4 7

# Trying to recreate the class sizes from the experiment
#               1   2   3    4   5   6    7    8     9
class_dist <- c(10, 3 , 12,  1 , 1,  1 , 0.5 , 0.5 , 10)


# generate the data
n_subjects = 120
obs_per_trial = 2

test_df <- gen_data(n_subjects, obs_per_trial, class_dist, class_prob)

# Plot with true classes
ggplot(test_df, aes(FB.tend,LR.tend, color=true_class)) +
  geom_jitter(width=0.05, height=0.05)




