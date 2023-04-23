library(dplyr)
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


experiment <- read.csv("subj_test_synth.csv")
experiment <- distinct(experiment) 
