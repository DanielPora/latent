
library(fossil)
#install.packages("fossil")
library(poLCA)
library(ggplot2)
library(dplyr)

eval_lca <- function(df){

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
  
  print(rand.index(as.numeric(df$true_class), as.numeric(df$lca.class)))
}

easy_1 <- read.csv("./data_easy_1.csv", header=TRUE, stringsAsFactors=TRUE)
eval_lca(easy_1)

easy_2 <- read.csv("./data_easy_2.csv", header=TRUE, stringsAsFactors=TRUE)
eval_lca(easy_2)

easy_3 <- read.csv("./data_easy_3.csv", header=TRUE, stringsAsFactors=TRUE)
eval_lca(easy_3)

medium_1 <- read.csv("./data_medium_1.csv", header=TRUE, stringsAsFactors=TRUE)
eval_lca(medium_1)
medium_2 <- read.csv("./data_medium_2.csv", header=TRUE, stringsAsFactors=TRUE)
eval_lca(medium_2)


