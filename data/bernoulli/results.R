library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(rstudioapi)
library(tidyr)
library(dplyr)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

results <- read.csv('./analysis_data.csv')
results1 <- read.csv('./analysis_data1.csv')
results2 <- read.csv('./analysis_data2.csv')
results3 <- read.csv('./analysis_data3.csv')


grouped <- results %>%
  group_by(scenario, obs_per_trial, n_subj, method)%>%
  summarise(RI_mean = mean(RI), RI_sd = sd(RI), NMI_mean = mean(NMI), NMI_sd = sd(NMI), RIc_mean = mean(RIc), RIc_sd = sd(RIc), NMIc_mean = mean(NMIc), NMIc = sd(NMIc))
grouped$scenario <- factor(grouped$scenario)
grouped$obs_per_trial <- factor(grouped$obs_per_trial)
grouped$n_subj <- factor(grouped$n_subj)
grouped$method <- factor(grouped$method)


grouped1 <- results1 %>%
  group_by(scenario, obs_per_trial, n_subj, method)%>%
  summarise(RI_mean = mean(RI), RI_sd = sd(RI), NMI_mean = mean(NMI), NMI_sd = sd(NMI), RIc_mean = mean(RIc), RIc_sd = sd(RIc), NMIc_mean = mean(NMIc), NMIc = sd(NMIc))
grouped1['method'][grouped1['method'] == 'BMEM'] <- 'BMEM Gauss'
grouped1['method'][grouped1['method'] == 'GLM'] <- 'LMEM'
grouped1['method'][grouped1['method'] == 'LCA'] <- 'LCA 2'
grouped1$scenario <- factor(grouped1$scenario)
grouped1$obs_per_trial <- factor(grouped1$obs_per_trial)
grouped1$n_subj <- factor(grouped1$n_subj)
grouped1$method <- factor(grouped1$method)

grouped2 <- results2 %>%
  group_by(scenario, obs_per_trial, n_subj, method)%>%
  summarise(RI_mean = mean(RI), RI_sd = sd(RI), NMI_mean = mean(NMI), NMI_sd = sd(NMI), RIc_mean = mean(RIc), RIc_sd = sd(RIc), NMIc_mean = mean(NMIc), NMIc = sd(NMIc))

grouped2['method'][grouped1['method'] == 'LCA'] <- 'LCA 3'
grouped2$scenario <- factor(grouped2$scenario)
grouped2$obs_per_trial <- factor(grouped2$obs_per_trial)
grouped2$n_subj <- factor(grouped2$n_subj)
grouped2$method <- factor(grouped2$method)



grouped3 <- results3 %>%
  group_by(scenario, obs_per_trial, n_subj, method)%>%
  summarise(RI_mean = mean(RI), RI_sd = sd(RI), NMI_mean = mean(NMI), NMI_sd = sd(NMI), RIc_mean = mean(RIc), RIc_sd = sd(RIc), NMIc_mean = mean(NMIc), NMIc = sd(NMIc))

grouped3['method'][grouped1['method'] == 'LCA'] <- 'LCA 4'
grouped3$scenario <- factor(grouped3$scenario)
grouped3$obs_per_trial <- factor(grouped3$obs_per_trial)
grouped3$n_subj <- factor(grouped3$n_subj)
grouped3$method <- factor(grouped3$method)

m <- lm(RI_mean ~ n_subj, data=grouped)
summary(m)

df = rbind(grouped, grouped1)

ggplot(grouped, aes(x=obs_per_trial, y=RI_mean, color=method))+
  geom_jitter(height = 0, width = 0.5)


ggplot(grouped, aes(x = obs_per_trial, y = RIc_mean, fill = method)) +
  geom_boxplot()+
  facet_wrap(~ scenario, ncol=3)


m1 <- lm(RI_mean ~ n_subj, data=grouped1)
summary(m1)

ggplot(grouped1, aes(x=obs_per_trial, y=RI_mean, color=method))+
  geom_jitter(height = 0, width = 0.5)


ggplot(grouped1, aes(x = obs_per_trial, y = RIc_mean, fill = method)) +
  geom_boxplot()+
  facet_wrap(~ scenario, ncol=3)




## NMIc

ggplot(filter(df, method != 'LCA 2' & method != 'LCA 3'), aes(x = obs_per_trial, y = NMIc_mean, fill = method)) +
  geom_boxplot()+
  facet_wrap(~ scenario, ncol=3)+
  ylab("NMIc")+
  xlab("Observations per trial type")+
  labs(fill='Method')



## RIc

ggplot(filter(df, method != 'LCA 2'), aes(x = obs_per_trial, y = RIc_mean, fill = method)) +
  geom_boxplot()+
  facet_wrap(~ scenario, ncol=3)+
  ylab("RIc")+
  xlab("Observations per trial type")+
  labs(fill='Method')


## NMI

ggplot(filter(df, method != 'LCA 2'), aes(x = obs_per_trial, y = NMI_mean, fill = method)) +
  geom_boxplot()+
  facet_wrap(~ scenario, ncol=3)+
  ylab("NMI")+
  xlab("Observations per trial type")+
  labs(fill='Method')

## RI

ggplot(filter(df, method != 'LCA 2'), aes(x = obs_per_trial, y = RI_mean, fill = method)) +
  geom_boxplot()+
  facet_wrap(~ scenario, ncol=3)+
  ylab("RI")+
  xlab("Observations per trial type")+
  labs(fill='Method')




