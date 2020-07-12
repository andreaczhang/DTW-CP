# test my KNN classifier's performance 

library(magrittr)
library(purrr)
library(pROC)
library(Metrics)  
library(PRROC)  

source('./Utilities/helpers-rev-6knn.R')
dataPath <- '~/Documents/Data/Project2/'

out_sep_d1 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day1.RData'))
out_sep_d2 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day2.RData'))
out_sep_d3 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day3.RData'))
out_sep_d4 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day4.RData'))
out_sep_d5 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day5.RData'))
out_sep_d6 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day6.RData'))
out_sep_d7 <-  readRDS(paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/alloutcomes_sep_day7.RData'))

disvec_sep <- readRDS(file = paste0(dataPath, 'Ripoll2014/REVISION-RIPOLL/knn_disvecs_sep.RData'))


# ======= start simple ==== # 

out_sep_d1$split1$trdf$icustay_id %>% head

disvec_sep$distvec_sep_d1$split1$icustay_251972 %>% rownames

# everytime, check if the names match
# xte <- as.numeric(disvec_sep$distvec_sep_d1$split1[[2]])
ytr <- out_sep_d1[[2]]$trdf
yte <- out_sep_d1[[2]]$tedf

distlist <- disvec_sep$distvec_sep_d2[[2]]


tryrun <- knn_everytest_wrap(distvecList = disvec_sep$distvec_sep_d1[[2]], 
                             trdf = out_sep_d1[[2]]$trdf, 
                             tedf = out_sep_d1[[2]]$tedf, 
                             k = 1)


# ======================== #
# go through one whole split
# ======================== # 

knnres_d1_k1 <- list()
for(s in 1:10){
  knnres_d1_k1[[s]] <- knn_everytest_wrap(distvecList = disvec_sep$distvec_sep_d1[[s]], 
                                          trdf = out_sep_d1[[s]]$trdf, 
                                          tedf = out_sep_d1[[s]]$tedf, 
                                          k = 1)
  cat('split', s, 'done\n')
}

knnres_d1_k1 %>% do.call(rbind, .)







