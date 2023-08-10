library(aricode)
library(fossil)
library(poLCA)
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
# Evaluation of a given dataset


data  <- read.csv("./test_df_FB_LR.csv")
data$Subject <- factor(data$Subject)
data$true_class <- factor(data$true_class)
n_obs <- nrow(dplyr::filter(data, Subject == 1))/2


# order the data
data <- data[with(data, order(Subject, trial_type, own.cod)), ]


# extend to wide format and naming accordingly
name_vec <- c()
suffixes <- rep(seq(1:(n_obs)), 2)
for (elem in dplyr::filter(data, Subject == 1)[2]){
  name_vec <- append(name_vec, elem)
}
for (i in 1:length(name_vec)){
  name_vec[i] <- paste(name_vec[i],suffixes[i], sep="")
}


data_with_index <- ddply(data, .(Subject), mutate, 
                         index = name_vec[1:length(Subject)])

df.LCA_id <- reshape2::dcast(data_with_index, Subject ~ index, value.var = 'own.cod')
# for poLCA we will need only the value column and no zero or negative values
df.LCA <- df.LCA_id %>% 
  dplyr::select(-Subject)
df.LCA <- df.LCA+1


formula_func <- function(colnms1, dat) {
  fmla <- as.formula(paste0("cbind(", 
                            paste(colnms1, collapse=","), ")", " ~ ", 1))
  mva <- manova(fmla, data = dat)
  mva$call <- fmla
  mva
}

f <- as.formula(with(df.LCA, formula_func(colnames(df.LCA), df.LCA)))
bics <- c()
for (x in 1:8) {
  lca_x <- poLCA(f, df.LCA, nclass = x, nrep = 10, verbose=FALSE)$bic 
  bics <- append(bics, lca_x)
}
# 4 Class model with the best BIC                                                 
(opt_class <- which.min(bics))

# printing output and graph
lca_3 <- poLCA(f, df.LCA, nclass = opt_class, nrep = 50, verbose=FALSE, graphs = FALSE) 

df.LCA_classes <- df.LCA_id %>%
  mutate('lca.class' = factor(lca_3$predclass))%>%
  dplyr::select(Subject, lca.class)


data <- left_join(data, df.LCA_classes, by="Subject")

by_subj_fb <- dplyr::filter(data, trial_type == 'FB') %>%
  group_by(Subject) %>%
  summarise(fb_tend = sum(own.cod)/n_obs)
by_subj_lr <- dplyr::filter(data, trial_type == 'LR') %>%
  group_by(Subject) %>%
  summarise(lr_tend = sum(own.cod)/n_obs)

data <- left_join(data, by_subj_fb, by="Subject")
data <- left_join(data, by_subj_lr, by="Subject")

(RI_LCA <- rand.index(as.numeric(data$true_class),as.numeric(data$lca.class)))
(NMI_LCA <- NMI(as.numeric(data$true_class),as.numeric(data$lca.class)))

## eval plot for lca
ggplot(data, aes(lr_tend,fb_tend, color=true_class))+
  geom_jitter(width=0.05, height=0.05, aes(shape=lca.class))



