# run sep with knn, k = 1, 5, 10
library(magrittr)
library(purrr)
library(pROC)
library(Metrics)  
library(PRROC)  

source('./Utilities/helpers-rev-6knn.R')
source('./Utilities/helpers-rev-6classify.R')
dataPath <- '~/Documents/Data/Project2/'

out_sep_d1 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day1.RData'))
out_sep_d2 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day2.RData'))
out_sep_d3 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day3.RData'))
out_sep_d4 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day4.RData'))
out_sep_d5 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day5.RData'))
out_sep_d6 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day6.RData'))
out_sep_d7 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day7.RData'))

disvec_sep <- readRDS(file = paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/knn_disvecs_sep.RData'))

# put outcomes in a list
outs_sep_list <- list(out_sep_d1 = out_sep_d1, 
                      out_sep_d2 = out_sep_d2, 
                      out_sep_d3 = out_sep_d3, 
                      out_sep_d4 = out_sep_d4, 
                      out_sep_d5 = out_sep_d5, 
                      out_sep_d6 = out_sep_d6, 
                      out_sep_d7 = out_sep_d7)


# ================== #
# run 
# ================== #

K <- 3

knnres_sep_all <- list()
for(s in 1:10){
  knnres_sep_alldays_eachsp <- list()
  
  for(d in 1:7){
    knnres_sep_alldays_eachsp[[d]] <- knn_everytest_wrap(distvecList = disvec_sep[[d]][[s]], 
                                                         trdf = outs_sep_list[[d]][[s]]$trdf, 
                                                         tedf = outs_sep_list[[d]][[s]]$tedf, 
                                                         k = K)
    cat('day', d, 'split', s, 'done\n')
    
  }
  names(knnres_sep_alldays_eachsp) <- paste0('day', 1:7)
  knnres_sep_all[[s]] <- knnres_sep_alldays_eachsp %>% do.call(rbind, .)

}

names(knnres_sep_all) <- paste0('split', 1:10)

knnres_sep_all$split1




saveRDS(knnres_sep_all, file = paste0(dataPath, 'AllResults/Baseline_sep/knn_k', K, '_sep.RData'))

auc_k3 <- map_df(knnres_sep_all, function(x){x[, 1]})
auprc_k3 <- map_df(knnres_sep_all, function(x){x[, 2]})
accu_k3 <- map_df(knnres_sep_all, function(x){x[, 3]})

boxplot(auc_k3)
boxplot(auprc_k3)
boxplot(accu_k3)
