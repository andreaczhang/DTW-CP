# run aki with knn, k = 1, 3, 5
library(magrittr)
library(purrr)
library(pROC)
library(Metrics)  
library(PRROC)  

source('./utility/b1-knn.R')
source('./utility/4-classify.R')
dataPath <- 'data/path'

out_aki_d1 <-  readRDS(paste0(dataPath, 'alloutcomes_aki_day1.RData'))
out_aki_d2 <-  readRDS(paste0(dataPath, 'alloutcomes_aki_day2.RData'))
out_aki_d3 <-  readRDS(paste0(dataPath, 'alloutcomes_aki_day3.RData'))
out_aki_d4 <-  readRDS(paste0(dataPath, 'alloutcomes_aki_day4.RData'))
out_aki_d5 <-  readRDS(paste0(dataPath, 'alloutcomes_aki_day5.RData'))
out_aki_d6 <-  readRDS(paste0(dataPath, 'alloutcomes_aki_day6.RData'))
out_aki_d7 <-  readRDS(paste0(dataPath, 'alloutcomes_aki_day7.RData'))

disvec_aki <- readRDS(file = paste0(dataPath, 'knn_disvecs_aki.RData'))

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

K <- 1

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






