library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("data.csv")
names(data)[names(data) == "obs_per_trial"] <- "obs"
names(data)[names(data) == "n_subj"] <- "nsub"
library(lsr)
library(dplyr)
library(ggplot2)
library(lme4)
library(betareg)

# [data$obs_per_trial == n,]
n = 400
data_diff <- data %>%
  mutate(diff = RIc - RI)


class_test <- t.test(data_diff$RIc, data_diff$RI, paired = TRUE)

data_LCA <- data_diff[data$method == "LCA", ]
class_test_LCA <- t.test(data_LCA$RIc, data_LCA$RI, paired = TRUE)
effect_size_LCA <- cohensD(data_LCA$RIc, data_LCA$RI, method = "paired")


data_GLM <- data_diff[data$method == "GLM", ]
class_test_GLM <- t.test(data_GLM$RIc, data_GLM$RI, paired = TRUE)
effect_size_GLM <- cohensD(data_GLM$RIc, data_GLM$RI, method = "paired")

data_BMEM <- data_diff[data$method == "BMEM", ]

class_test_BMEM <- t.test(data_BMEM$RIc, data_BMEM$RI, paired = TRUE)
effect_size_BMEM <- cohensD(data_BMEM$RIc, data_BMEM$RI, method = "paired")

print(paste("LCA mean:", class_test_LCA$estimate))
print(paste("LCA d:",effect_size_LCA))
print(paste("GLM mean:",class_test_GLM$estimate))
print(paste("GLM d:",effect_size_GLM))
print(paste("BMEM mean:",class_test_BMEM$estimate))
print(paste("BMEM d:",effect_size_BMEM))

class_test_LCA
class_test_GLM
class_test_BMEM

# scenario difficulties

summary_data <- data %>%
  group_by(scenario) %>%
  summarise(avg_RI = mean(RI),sd_RI = sd(RI),
            avg_RIc = mean(RIc), sd_RIc = sd(RIc))

print(summary_data)

# sort RI
summary_data_sorted_RI <- summary_data %>%
  arrange(desc(avg_RI))

print(summary_data_sorted_RI)

# sort RIc
summary_data_sorted_RIc <- summary_data %>%
  arrange(desc(avg_RIc))

print(summary_data_sorted_RIc)


#### ANOVA methods mean difference

sc = subset(data, scenario == "sc6")

# Perform ANOVA
anova_result <- aov(RIc ~ method, data = sc)

# Summary of ANOVA
summary(anova_result)

TukeyHSD(anova_result, ordered = TRUE, conf.level=.95)


#### LM for number of observations and subjects

sc_met <- subset(data, scenario == "sc9" & method == "GLM")

model <- lm(RIc ~ obs * nsub , data=sc_met)

(model_summary <- summary(model))


coefficients <- model_summary$coefficients[, "Estimate"]
p_values <- model_summary$coefficients[, "Pr(>|t|)"]


significant_coefficients <- coefficients[p_values < 0.05]


f_value <- model_summary$fstatistic[1]
adj_r_squared <- model_summary$adj.r.squared
df_model <- model_summary$df[1]


significant_estimates <- significant_coefficients[-1]  # Intercept is not important here
f_value <- f_value[1]
adj_r_squared <- adj_r_squared
df_model <- df_model

significant_estimates
f_value
df_model


