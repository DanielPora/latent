library(ggplot2)
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("dataset_generator.R")

sc1 <- 
  c( 1,  0,  0,  0,  1,
     0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,
     1,  0,  0,  0,  1)
sc2 <-
  c( 0,  1,  0,  0,  0,
     0,  0,  0,  0,  1,
     0,  0,  0,  0,  0,
     1,  0,  0,  0,  0,
     0,  0,  0,  1,  0)
sc3 <- 
  c( 0,  1,  0,  2,  0,
     0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,
     0,  2,  0,  1,  0)
sc4 <- 
  c( 0,  0,  1,  0,  0,
     0,  0,  0,  0,  0,
     1,  0,  0,  0,  1,
     0,  0,  0,  0,  0,
     0,  0,  1,  0,  0)
sc5 <- 
  c( 0,  0,  0,  0,  0,
     0,  1,  0,  1,  0,
     0,  0,  0,  0,  0,
     0,  1,  0,  1,  0,
     0,  0,  0,  0,  0)
sc6 <- 
  c( 0,  0,  0,  0,  0,
     0,  0,  0,  1,  0,
     0,  0,  0,  0,  0,
     0,  1,  0,  0,  0,
     0,  0,  0,  0,  0)
sc7 <- 
  c( 1,  0,  0,  0,  0,
     0,  0,  0,  0,  0,
     0,  0,  1,  0,  0,
     0,  0,  0,  0,  0,
     0,  0,  0,  0,  1)
sc8 <- 
  c( 0,  0,  0,  0,  0,
     0,  1,  0,  2,  0,
     0,  0,  0,  0,  0,
     0,  2,  0,  1,  0,
     0,  0,  0,  0,  0)
sc9 <- 
  c( 1,  0,  0,  0,  5,
     0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,
     0,  1,  0,  0,  1,
     5,  0,  0,  0,  5)


scenarios <- list(sc1, sc2, sc3, sc4, sc5, sc6, sc7, sc8, sc9)
names(scenarios) <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", "Scenario 6", "Scenario 7", "Scenario 8", "Scenario 9")

size <- 200
obs <- 8
scenario <- 8




for (scenario in 1:9){
  for (obs in c(2,4,8)){
  
sc_name <- names(scenarios)[scenario]
sc_dist <- scenarios[[scenario]]
data <- gen_data_FB_LR(size, obs, sc_dist)

# True number of classes
n_true <- length(levels(data$true_class))
# Number of obs. per trial
n_obs <- nrow(dplyr::filter(data, Subject == 1))/2


# sort the data
data <- data[with(data, order(Subject, trial_type, own.cod)), ]
by_subj_fb <- dplyr::filter(data, trial_type == 'FB') %>%
  group_by(Subject) %>%
  summarise(fb_tend = sum(own.cod)/n_obs)
by_subj_lr <- dplyr::filter(data, trial_type == 'LR') %>%
  group_by(Subject) %>%
  summarise(lr_tend = sum(own.cod)/n_obs)

data <- left_join(data, by_subj_fb, by="Subject")
data <- left_join(data, by_subj_lr, by="Subject")

class_compare <- distinct(data[ , c('Subject','true_class','fb_tend', 'lr_tend')])

folder = paste("./plots/",sc_name,"/", sep='')

if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
png(paste(folder,"obs",obs,'.png', sep=''))
trend_plot <- ggplot(class_compare, aes(lr_tend,fb_tend, color=true_class))+
  geom_jitter(width=0.02, height=0.02)+
  ylab('FB-ratio')+
  xlab('LR-ratio')+
  theme(legend.position="none")+
  labs(title = sc_name)
Sys.sleep(0.5)
print(trend_plot)
dev.off()
}
}

