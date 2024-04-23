library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("bernoulli.csv")
library(lsr)
library(dplyr)
library(ggplot2)

# [data$obs_per_trial == n,]
n = 400
data_diff <- data[data$n_subj == n,] %>%
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

print(paste(n, "LCA mean:", class_test_LCA$estimate))
print(paste(n,"LCA d:",effect_size_LCA))
print(paste(n,"GLM mean:",class_test_GLM$estimate))
print(paste(n,"GLM d:",effect_size_GLM))
print(paste(n,"BMEM mean:",class_test_BMEM$estimate))
print(paste(n,"BMEM d:",effect_size_BMEM))



# scenarios


# Daten nach Methode und Szenario gruppieren und Durchschnitt der RI und RIc Werte berechnen
summary_data <- data %>%
  group_by(scenario) %>%
  summarise(avg_RI = mean(RI),sd_RI = sd(RI),
            avg_RIc = mean(RIc), sd_RIc = sd(RIc))

# Ausgabe der zusammengefassten Daten
print(summary_data)

# Sortieren nach avg_RI
summary_data_sorted_RI <- summary_data %>%
  arrange(desc(avg_RI))

# Ausgabe der sortierten Daten nach avg_RI
print(summary_data_sorted_RI)

# Sortieren nach avg_RIc
summary_data_sorted_RIc <- summary_data %>%
  arrange(desc(avg_RIc))

# Ausgabe der sortierten Daten nach avg_RIc
print(summary_data_sorted_RIc)





