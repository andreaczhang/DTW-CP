# run aki with knn, k = 1, 5, 50
library(magrittr)
library(purrr)
library(pROC)
library(Metrics)  
library(PRROC)  

source('./Utilities/helpers-rev-6knn.R')
source('./Utilities/helpers-rev-6classify.R')
dataPath <- '~/Documents/Data/Project2/'

out_aki_d1 <-  readRDS(paste0(dataPath, 'AKI/REVISION-AKI/alloutcomes_aki_day1.RData'))
out_aki_d2 <-  readRDS(paste0(dataPath, 'AKI/REVISION-AKI/alloutcomes_aki_day2.RData'))
out_aki_d3 <-  readRDS(paste0(dataPath, 'AKI/REVISION-AKI/alloutcomes_aki_day3.RData'))
out_aki_d4 <-  readRDS(paste0(dataPath, 'AKI/REVISION-AKI/alloutcomes_aki_day4.RData'))
out_aki_d5 <-  readRDS(paste0(dataPath, 'AKI/REVISION-AKI/alloutcomes_aki_day5.RData'))
out_aki_d6 <-  readRDS(paste0(dataPath, 'AKI/REVISION-AKI/alloutcomes_aki_day6.RData'))
out_aki_d7 <-  readRDS(paste0(dataPath, 'AKI/REVISION-AKI/alloutcomes_aki_day7.RData'))

disvec_aki <- readRDS(file = paste0(dataPath, 'AKI/REVISION-AKI/knn_disvecs_aki.RData'))

# put outcomes in a list
outs_aki_list <- list(out_aki_d1 = out_aki_d1, 
                      out_aki_d2 = out_aki_d2, 
                      out_aki_d3 = out_aki_d3, 
                      out_aki_d4 = out_aki_d4, 
                      out_aki_d5 = out_aki_d5, 
                      out_aki_d6 = out_aki_d6, 
                      out_aki_d7 = out_aki_d7)


# ================== #
# run 
# ================== #

K <- 2

knnres_aki_all <- list()
for(s in 1:50){
  knnres_aki_alldays_eachsp <- list()
  
  for(d in 1:7){
    knnres_aki_alldays_eachsp[[d]] <- knn_everytest_wrap(distvecList = disvec_aki[[d]][[s]], 
                                                         trdf = outs_aki_list[[d]][[s]]$trdf, 
                                                         tedf = outs_aki_list[[d]][[s]]$tedf, 
                                                         k = K)
    cat('day', d, 'split', s, 'done\n')
    
  }
  names(knnres_aki_alldays_eachsp) <- paste0('day', 1:7)
  knnres_aki_all[[s]] <- knnres_aki_alldays_eachsp %>% do.call(rbind, .)
  
}

names(knnres_aki_all) <- paste0('split', 1:50)


saveRDS(knnres_aki_all, file = paste0(dataPath, 'AllResults/Baseline_aki/knn_k', K, '_aki.RData'))



# do some simple plotting 

knnres_aki_all_k1 <- readRDS(paste0('~/Documents/Data/Project2/AllResults/Baseline_aki/knn_k', 1, '_aki.RData'))
knnres_aki_all_k2 <- readRDS(paste0('~/Documents/Data/Project2/AllResults/Baseline_aki/knn_k', 2, '_aki.RData'))

knnres_aki_all_k3 <- readRDS(paste0('~/Documents/Data/Project2/AllResults/Baseline_aki/knn_k', 3, '_aki.RData'))

knnres_aki_all_k5 <- readRDS(paste0('~/Documents/Data/Project2/AllResults/Baseline_aki/knn_k', 5, '_aki.RData'))
# knnres_aki_all_k15 <- readRDS(paste0('~/Documents/Data/Project2/AllResults/Baseline_aki/knn_k', 15, '_aki.RData'))



auc_k1 <- map_df(knnres_aki_all_k1, function(x){x[, 1]})
auc_k2 <- map_df(knnres_aki_all_k2, function(x){x[, 1]})
auc_k3 <- map_df(knnres_aki_all_k3, function(x){x[, 1]})
auc_k5 <- map_df(knnres_aki_all_k5, function(x){x[, 1]})
auc_k15 <- map_df(knnres_aki_all_k15, function(x){x[, 1]})

auprc_k1 <- map_df(knnres_aki_all_k1, function(x){x[, 2]})
auprc_k2 <- map_df(knnres_aki_all_k2, function(x){x[, 2]})
auprc_k3 <- map_df(knnres_aki_all_k3, function(x){x[, 2]})
auprc_k5 <- map_df(knnres_aki_all_k5, function(x){x[, 2]})
auprc_k15 <- map_df(knnres_aki_all_k15, function(x){x[, 2]})

accu_k1 <- map_df(knnres_aki_all_k1, function(x){x[, 3]})
accu_k2 <- map_df(knnres_aki_all_k2, function(x){x[, 3]})
accu_k3 <- map_df(knnres_aki_all_k3, function(x){x[, 3]})
accu_k5 <- map_df(knnres_aki_all_k5, function(x){x[, 3]})
accu_k15 <- map_df(knnres_aki_all_k15, function(x){x[, 3]})


par(mfrow = c(1, 2))
boxplot(auc_k1, ylim = c(0.5, 1))
boxplot(auc_k2, ylim = c(0.5, 1))
boxplot(auc_k3, ylim = c(0.5, 1))
boxplot(auc_k5, ylim = c(0.5, 1))
boxplot(auc_k15, ylim = c(0.5, 1))

boxplot(auprc_k1, ylim = c(0.5, 1))
boxplot(auprc_k2, ylim = c(0.5, 1))
boxplot(auprc_k3, ylim = c(0.5, 1))
boxplot(auprc_k5, ylim = c(0.5, 1))
boxplot(auprc_k15, ylim = c(0.5, 1))

boxplot(accu_k1, ylim = c(0.6, 1))
boxplot(accu_k2, ylim = c(0.6, 1))
boxplot(accu_k3, ylim = c(0.6, 1))
boxplot(accu_k5, ylim = c(0.6, 1))
boxplot(accu_k15, ylim = c(0.6, 1))





