library(dplyr)
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


data <- read.csv("subj_tests_syth.csv")