library(dplyr)

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
  
 
  df$own.cod <- as.numeric(df$own.cod)
  df$Subject <- as.numeric(df$Subject)
  df$true_class <- factor(df$true_class)
  df$true_LR <- as.numeric(df$true_LR)
  df$true_FB <- as.numeric(df$true_FB)
  
  return(df)
}
