
library(aricode)
library(fossil)
library(poLCA)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyLPA)
library(lme4)
library(brms)
# Evaluation of a given dataset

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

Sys.time()
time_id <- gsub(' ', '', gsub(':', '', gsub('-','',Sys.time())), fixed = TRUE)

scenarios <- list(sc1, sc2, sc3, sc4, sc5, sc6, sc7, sc8, sc9)
names(scenarios) <- c("sc1", "sc2", "sc3", "sc4", "sc5", "sc6", "sc7", "sc8", "sc9")

row.names = c("scenario", "obs_per_trial", "n_subj", "method", "class_true", "run", "class_pred", "RI", "RIc", "adj.RI", "adj.RIc", "NMI", "NMIc", "warnings")
length(row.names)
measure_df = data.frame(matrix(ncol = length(row.names), nrow = 0))
names(measure_df) = row.names

folder = paste('./data/',time_id,"/", sep="")

if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
if (!file.exists(paste(folder,'./analysis_data.csv', sep=''))){
  write.csv(measure_df, paste(folder,"analysis_data.csv", sep=''), row.names=FALSE)
}else{
  var = readline(prompt = "File already exists. Override? (y/n) ")
  if (var == 'y'){
    write.csv(measure_df, paste(folder,"analysis_data.csv", sep=''), row.names=FALSE)
  }else{
    print("Creating backup")
    backup <- read.csv(paste(folder,"analysis_data.csv", sep=''))
    write.csv(backup, paste(paste(folder,"backup_analysis_data", sep=''),time_id,".csv",sep=''), row.names=FALSE)
    write.csv(measure_df, paste(folder,"analysis_data.csv", sep=''), row.names=FALSE)
  }
}
warn <- 'none'

scenario <- 5
obs_per_trial <- 2
n_subject <- 100
run <- 2


for (scenario in 7:9){  
  for (obs_per_trial in c(2,4,8)){
    for (n_subject in c(100,200,400)){ 
      for (run in 1:5){
        
       
        sc_name <- names(scenarios)[scenario]
        sc_dist <- scenarios[[scenario]]
        
        filepath <- paste("./data/",time_id,"/",sc_name,"/obs",obs_per_trial,"/nSubj",n_subject,"/", sep="")
        if (!dir.exists(filepath)) dir.create(filepath, recursive = TRUE)
        
        msg <- paste("Scenario: ",sc_name,", Obs: ",obs_per_trial,", Subjects: ", n_subject, ", Run: ", run, sep = '')
        print(msg)
        measure_df <- read.csv(paste(folder,"analysis_data.csv", sep=""))
        
        set.seed(3141+run)
        data <- gen_data_FB_LR(n_subject, obs_per_trial, sc_dist)
        raw_data <- data
        # True number of classes
        n_true <- length(levels(data$true_class))
        # Number of obs. per trial
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
          dplyr::select(-Subject)%>%
          mutate_if(is.character,as.numeric)
        
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
        for (x in 1:6) {
          lca_x <- poLCA(f, df.LCA, nclass = x, nrep = 10, verbose=FALSE)$bic 
          bics <- append(bics, lca_x)
        }
        
                                                      
        opt_class <- which.min(bics)

        
        ww <- c()
        tryCatch(
          withCallingHandlers(lca_3 <- poLCA(f, df.LCA, nclass = opt_class, nrep = 10, verbose=FALSE, graphs = FALSE), warning = function(w) ww <<- c(ww, list(w)))
        )
        lca_3 <- poLCA(f, df.LCA, nclass = opt_class, nrep = 10, verbose=FALSE, graphs = FALSE)
        
        
        wlen <- length(ww)
        ww <- unlist(ww)
        
        if (wlen > 0) {
          warn <- paste(wlen, " Warnings: ", paste(ww, collapse = '. Next warning: '), sep='')
        } else {
          warn <- 'none'
        }
        
        
        lca_tc <- poLCA(f, df.LCA, nclass = n_true, nrep = 10, verbose=FALSE, graphs = FALSE) 
        
        df.LCA_classes <- df.LCA_id %>%
          mutate('lca.class' = factor(lca_3$predclass))%>%
          mutate('lca.tc' = factor(lca_tc$predclass))%>%
          dplyr::select(Subject, lca.class, lca.tc)
        
        data <- left_join(data, df.LCA_classes, by="Subject")
        
        
       
        dplyr::filter(data, trial_type == 'FB')%>%
          dplyr::group_by(Subject)%>%
          summarize(fb_tend = sum(own.cod)/n_obs)
        
        
        by_subj_fb <- dplyr::filter(data, trial_type == 'FB') %>%
          group_by(Subject) %>%
          summarise( fb_tend = sum(own.cod)/n_obs)
        by_subj_lr <- dplyr::filter(data, trial_type == 'LR') %>%
          group_by(Subject) %>%
          summarise(lr_tend = sum(own.cod)/n_obs)
        
        data <- left_join(data, by_subj_fb, by="Subject")
        data <- left_join(data, by_subj_lr, by="Subject")
        
        # measure row for LCA
        class_compare <- distinct(data[ , c('Subject', 'lca.class', 'true_class', 'lca.tc')])
        
        adj.RI <- adj.rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$lca.class))
        RI <- rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$lca.class))
        NMI <- NMI(as.numeric(class_compare$true_class),as.numeric(class_compare$lca.class))
        adj.RIc <- adj.rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$lca.tc))
        RIc <- rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$lca.tc))
        NMIc <- NMI(as.numeric(class_compare$true_class),as.numeric(class_compare$lca.tc))
        n_pred <- length(levels(class_compare$lca.class))
        
        mes_LCA <- c(sc_name, obs_per_trial, n_subject, 'LCA', n_true, run, n_pred, RI, RIc, adj.RI, adj.RIc, NMI, NMIc, warn)
        measure_df[nrow(measure_df)+1,] <- mes_LCA
        
        ## eval plot for lca
        plot_data <- left_join(class_compare, distinct(data[ , c('Subject','true_class','fb_tend', 'lr_tend')]))
        
        png(paste(filepath,'lca',run,'.png', sep=''))
        lca_plot <- ggplot(plot_data, aes(lr_tend,fb_tend, color=lca.class))+
          geom_jitter(width=0.05, height=0.05, aes(shape=true_class))
        Sys.sleep(0.5)
        print(lca_plot)
        dev.off()
        
        
        png(paste(filepath,'lca_tc',run,'.png', sep=''))
        lca_tc_plot <- ggplot(plot_data, aes(lr_tend,fb_tend, color=lca.tc))+
          geom_jitter(width=0.05, height=0.05, aes(shape=true_class))
        print(lca_tc_plot)
        Sys.sleep(0.5)
        dev.off()
        
        ############ GLM
        ww <- c()
        
        tryCatch(
          withCallingHandlers(m0 <- glmer(own.cod ~ trial_type + (1+trial_type | Subject), data=data, family = binomial())
                               , warning = function(w) ww <<- c(ww, list(w)))
         )
       
        wlen <- length(ww)
        ww <- unlist(ww)
        
        if (wlen > 0) {
          warn <- paste(wlen, " Warnings: ", paste(ww, collapse = '. Next warning: '), sep='')
        } else {
          warn <- 'none'
        }
        
        coeffs <- coef(m0)$Subject
        coeffs$FB <-  coeffs$`(Intercept)`
        coeffs$LR <-  coeffs$trial_typeLR
       
        ahp <- coeffs %>%
          dplyr::select(FB, LR) %>%
          single_imputation() %>%
          estimate_profiles(1:6)%>%
          compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik", "CLC", "KIC"))
        
        glm_mod <- coeffs %>%
          dplyr::select(FB, LR) %>%
          single_imputation() %>%
          estimate_profiles(ahp$AHP)
        
        glm_tc <- coeffs %>%
          dplyr::select(FB, LR) %>%
          single_imputation() %>%
          estimate_profiles(n_true)
        
        glm_class <- get_data(glm_mod)$Class %>%
          factor()
        
        glm_class_tc <- get_data(glm_tc)$Class %>%
          factor()
        
        glm_class <- data.frame(Subject = 1:length(glm_class),glm.class = glm_class)
        glm_class$Subject <- factor(glm_class$Subject)
        
        glm_class_tc <- data.frame(Subject = 1:length(glm_class_tc),glm.tc = glm_class_tc)
        glm_class_tc$Subject <- factor(glm_class_tc$Subject)
        
        data$Subject <- factor(data$Subject)
        
        data <- left_join(data, glm_class, by="Subject")
        data <- left_join(data, glm_class_tc, by="Subject")
        
       
        # measure for GMEM
        class_compare <- distinct(data[ , c('Subject', 'glm.class', 'true_class', 'glm.tc')])
        
        adj.RI <- adj.rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$glm.class))
        RI <- rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$glm.class))
        NMI <- NMI(as.numeric(class_compare$true_class),as.numeric(class_compare$glm.class))
        RIc <- rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$glm.tc))
        adj.RIc <- adj.rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$glm.tc))
        NMIc<- NMI(as.numeric(class_compare$true_class),as.numeric(class_compare$glm.tc))
        n_pred <- length(levels(class_compare$glm.class))
       
        mes_GLM <- c(sc_name, obs_per_trial, n_subject, 'GLM', n_true, run, n_pred, RI, RIc, adj.RI, adj.RIc, NMI, NMIc, warn)
        measure_df[nrow(measure_df)+1,] <- mes_GLM
        
        
        # eval plot for GLM
        
        plot_data <- left_join(class_compare, distinct(data[ , c('Subject','true_class','fb_tend', 'lr_tend')]))
        
        png(paste(filepath,'glm',run,'.png', sep=''))
        glm_plot <- ggplot(plot_data, aes(lr_tend,fb_tend, color=glm.class))+
          geom_jitter(width=0.05, height=0.05, aes(shape=true_class))
        Sys.sleep(0.5)
        print(glm_plot)
        dev.off()
        
        
        png(paste(filepath,'glm_tc',run,'.png', sep=''))
        glm_tc_plot <- ggplot(plot_data, aes(lr_tend,fb_tend, color=glm.tc))+
          geom_jitter(width=0.05, height=0.05, aes(shape=true_class))
        print(glm_tc_plot)
        Sys.sleep(0.5)
        dev.off()
        
        
        
         
        ######### lmem  #########
        ww <- c()
        
        
        tryCatch(
         withCallingHandlers(m0 <- lmer(own.cod ~ trial_type + (1+trial_type | Subject), data=data)
                           , warning = function(w) ww <<- c(ww, list(w)))
        )
        
        
        wlen <- length(ww)
        ww <- unlist(ww)
        
        if (wlen > 0) {
          warn <- paste(wlen, " Warnings: ", paste(ww, collapse = '. Next warning: '), sep='')
        } else {
          warn <- 'none'
        }
        
        coeffs <- coef(m0)$Subject
        coeffs
        
        coeffs$FB <-  coeffs$`(Intercept)`
        coeffs$LR <-  coeffs$trial_typeLR

        
        ahp <- coeffs %>%
          dplyr::select(FB, LR) %>%
          single_imputation() %>%
          estimate_profiles(1:6)%>%
          compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik", "CLC", "KIC"))
  
        lmem_mod <- coeffs %>%
          dplyr::select(FB, LR) %>%
          single_imputation() %>%
          estimate_profiles(ahp$AHP)
        
        lmem_tc <- coeffs %>%
          dplyr::select(FB, LR) %>%
          single_imputation() %>%
          estimate_profiles(n_true)
        
        lmem_class <- get_data(lmem_mod)$Class %>%
          factor()
        
        lmem_class_tc <- get_data(lmem_tc)$Class %>%
          factor()
        
        lmem_class <- data.frame(Subject = 1:length(lmem_class),lmem.class = lmem_class)
        lmem_class$Subject <- factor(lmem_class$Subject)
        
        lmem_class_tc <- data.frame(Subject = 1:length(lmem_class_tc),lmem.tc = lmem_class_tc)
        lmem_class_tc$Subject <- factor(lmem_class_tc$Subject)
        
        data$Subject <- factor(data$Subject)
        
        data <- left_join(data, lmem_class, by="Subject")
        data <- left_join(data, lmem_class_tc, by="Subject")
        
        
        
        # measure for GMEM
        class_compare <- distinct(data[ , c('Subject', 'lmem.class', 'true_class', 'lmem.tc')])
        
        RI <- rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$lmem.class))
        adj.RI <- adj.rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$lmem.class))
        NMI <- NMI(as.numeric(class_compare$true_class),as.numeric(class_compare$lmem.class))
        RIc <- rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$lmem.tc))
        adj.RIc <- adj.rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$lmem.tc))
        NMIc<- NMI(as.numeric(class_compare$true_class),as.numeric(class_compare$lmem.tc))
        n_pred <- length(levels(class_compare$lmem.class))
        
        mes_GLM <- c(sc_name, obs_per_trial, n_subject, 'LMEM', n_true, run, n_pred, RI, RIc, adj.RI, adj.RIc, NMI, NMIc, warn)
        measure_df[nrow(measure_df)+1,] <- mes_GLM
        
        # eval plot for GLM
        plot_data <- left_join(class_compare, distinct(data[ , c('Subject','true_class','fb_tend', 'lr_tend')]))
        
        png(paste(filepath,'lmem',run,'.png', sep=''))
        lmem_plot <- ggplot(plot_data, aes(lr_tend,fb_tend, color=lmem.class))+
          geom_jitter(width=0.05, height=0.05, aes(shape=true_class))
        Sys.sleep(0.5)
        print(lmem_plot)
        dev.off()
        
        
        png(paste(filepath,'lmem_tc',run,'.png', sep=''))
        lmem_tc_plot <- ggplot(plot_data, aes(lr_tend,fb_tend, color=lmem.tc))+
          geom_jitter(width=0.05, height=0.05, aes(shape=true_class))
        print(lmem_tc_plot)
        Sys.sleep(0.5)
        dev.off()
        
        #### BMEM analysis

        ww <- c()
        
        tryCatch(
        withCallingHandlers(bm2 <- brm(data = data, own.cod ~ trial_type + (1+trial_type | Subject),
                                      seed = 123,
                                     cores = 8,
                                    iter = 4000, warmup = 2000,
                                   file = paste(filepath, 'bm', run, sep = ""))
                       , warning = function(w) ww <<- c(ww, list(w)))
               )
        
        
        wlen <- length(ww)
        ww <- unlist(ww)
        
        if (wlen > 0) {
          warn <- paste(wlen, " Warnings: ", paste(ww, collapse = '. Next warning: '), sep='')
        } else {
          warn <- 'none'
        }
        
        bmcoeff <- coef(bm2)$Subject
        
        
        
        
        coeffs <- coeffs %>%
          mutate("FB_bmr" = bmcoeff[1:nrow(bmcoeff),1,1]) %>%
          mutate("LR_bmr" = bmcoeff[1:nrow(bmcoeff),1,1] + bmcoeff[1:nrow(bmcoeff),1,2])
        

        ahp <- coeffs %>%
          dplyr::select(FB_bmr, LR_bmr) %>%
          single_imputation() %>%
          estimate_profiles(1:6)%>% 
          compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik"))
        
        bm_mod <- coeffs %>%
          dplyr::select(FB_bmr, LR_bmr) %>%
          single_imputation() %>%
          estimate_profiles(ahp$AHP)
        
        bm_mod_tc <- coeffs %>%
          dplyr::select(FB_bmr, LR_bmr) %>%
          single_imputation() %>%
          estimate_profiles(n_true)
        
        bm_class_tc <- get_data(bm_mod_tc)$Class %>%
          factor()
        
        bm_class <- get_data(bm_mod)$Class %>%
          factor()
        
        bm_class <- data.frame(Subject = 1:length(bm_class),bm.class = bm_class)
        bm_class$Subject <- factor(bm_class$Subject)
        bm_class_tc <- data.frame(Subject = 1:length(bm_class_tc),bm.tc = bm_class_tc)
        bm_class_tc$Subject <- factor(bm_class_tc$Subject)
        
        data <- left_join(data, bm_class, by="Subject")
        data <- left_join(data, bm_class_tc, by="Subject")
        
         
        # measure for BMEM
        class_compare <- distinct(data[ , c('Subject', 'bm.class', 'true_class', 'bm.tc')])
        
        RI <- rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$bm.class))
        adj.RI <- adj.rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$bm.class))
        NMI <- NMI(as.numeric(class_compare$true_class),as.numeric(class_compare$bm.class))
        RIc <- rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$bm.tc))
        adj.RIc <- adj.rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$bm.tc))
        NMIc <- NMI(as.numeric(class_compare$true_class),as.numeric(class_compare$bm.tc))
        n_pred <- length(levels(class_compare$bm.class))
        
      
        mes_BM <- c(sc_name, obs_per_trial, n_subject, 'BMEM', n_true, run, n_pred, RI, RIc, adj.RI, adj.RIc, NMI, NMIc, warn)
        measure_df[nrow(measure_df)+1,] <- mes_BM
        
        ## plot BM
        plot_data <- left_join(class_compare, distinct(data[ , c('Subject','true_class','fb_tend', 'lr_tend')]))
        
        png(paste(filepath,'bm',run,'.png', sep=''))
        plot_bm <- ggplot(plot_data, aes(lr_tend,fb_tend, color=bm.class))+
          geom_jitter(width=0.05, height=0.05, aes(shape=true_class))
        print(plot_bm)
        Sys.sleep(0.5)
        dev.off()
        
        
        png(paste(filepath,'bm_tc',run,'.png', sep=''))
        plot_tc_bm <- ggplot(plot_data, aes(lr_tend,fb_tend, color=bm.tc))+
          geom_jitter(width=0.05, height=0.05, aes(shape=true_class))
        print(plot_tc_bm)
        Sys.sleep(0.5)
        dev.off()
        
        
        
        #### BBM analysis
        
        ww <- c()
        
        
         tryCatch(
            withCallingHandlers(bm2 <- brm(data = data, own.cod ~ trial_type + (1+trial_type | Subject),
                                           family = bernoulli(),
                                           seed = 123,
                                           cores = 8,
                                           iter = 4000, warmup = 2000,
                                           file = paste(filepath, 'bbm', run, sep = ""))
                                , warning = function(w) ww <<- c(ww, list(w)))
         )
        
        
        
        wlen <- length(ww)
        ww <- unlist(ww)
        
        if (wlen > 0) {
          warn <- paste(wlen, " Warnings: ", paste(ww, collapse = '. Next warning: '), sep='')
        } else {
          warn <- 'none'
        }
        
        bmcoeff <- coef(bm2)$Subject
        
        coeffs <- coeffs %>%
          mutate("FB_bmr" = bmcoeff[1:nrow(bmcoeff),1,1]) %>%
          mutate("LR_bmr" = bmcoeff[1:nrow(bmcoeff),1,1] + bmcoeff[1:nrow(bmcoeff),1,2])
        
       
        ahp <- coeffs %>%
          dplyr::select(FB_bmr, LR_bmr) %>%
          single_imputation() %>%
          estimate_profiles(1:6)%>% 
          compare_solutions(statistics=c("AIC", "BIC", "Entropy", "LogLik"))
        
        bbm_mod <- coeffs %>%
          dplyr::select(FB_bmr, LR_bmr) %>%
          single_imputation() %>%
          estimate_profiles(ahp$AHP)
        
        bbm_mod_tc <- coeffs %>%
          dplyr::select(FB_bmr, LR_bmr) %>%
          single_imputation() %>%
          estimate_profiles(n_true)
        
        bbm_class_tc <- get_data(bbm_mod_tc)$Class %>%
          factor()
        
        bbm_class <- get_data(bbm_mod)$Class %>%
          factor()
        
        bbm_class <- data.frame(Subject = 1:length(bbm_class),bbm.class = bbm_class)
        bbm_class$Subject <- factor(bbm_class$Subject)
        bbm_class_tc <- data.frame(Subject = 1:length(bbm_class_tc),bbm.tc = bbm_class_tc)
        bbm_class_tc$Subject <- factor(bbm_class_tc$Subject)
        
        data <- left_join(data, bbm_class, by="Subject")
        data <- left_join(data, bbm_class_tc, by="Subject")
        
  
        # measure for BMEM
        class_compare <- distinct(data[ , c('Subject', 'bbm.class', 'true_class', 'bbm.tc')])
        
        adj.RI <- adj.rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$bbm.class))
        RI <- rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$bbm.class))
        NMI <- NMI(as.numeric(class_compare$true_class),as.numeric(class_compare$bbm.class))
        adj.RIc <- adj.rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$bbm.tc))
        RIc <- rand.index(as.numeric(class_compare$true_class),as.numeric(class_compare$bbm.tc))
        NMIc <- NMI(as.numeric(class_compare$true_class),as.numeric(class_compare$bbm.tc))
        n_pred <- length(levels(class_compare$bm.class))
        
        
        
        
        mes_BM <- c(sc_name, obs_per_trial, n_subject, 'BBMEM', n_true, run, n_pred, RI, RIc, adj.RI, adj.RIc, NMI, NMIc, warn)
        measure_df[nrow(measure_df)+1,] <- mes_BM
        
        ## plot BM
        plot_data <- left_join(class_compare, distinct(data[ , c('Subject','true_class','fb_tend', 'lr_tend')]))
        
        png(paste(filepath,'bbm',run,'.png', sep=''))
        plot_bm <- ggplot(plot_data, aes(lr_tend,fb_tend, color=bbm.class))+
          geom_jitter(width=0.05, height=0.05, aes(shape=true_class))
        print(plot_bm)
        Sys.sleep(0.5)
        dev.off()
        
        
        png(paste(filepath,'bbm_tc',run,'.png', sep=''))
        plot_tc_bm <- ggplot(plot_data, aes(lr_tend,fb_tend, color=bbm.tc))+
          geom_jitter(width=0.05, height=0.05, aes(shape=true_class))
        print(plot_tc_bm)
        Sys.sleep(0.5)
        dev.off()
        
        
        
        ### add to summary file
        write.csv(data, paste(filepath,"run",run,".csv", sep=""), row.names=FALSE)
        
        write.csv(measure_df, paste(folder,"analysis_data.csv", sep=''), row.names=FALSE)
        
      }
      
    }
  }
  
}



  
