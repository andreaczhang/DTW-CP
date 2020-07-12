# prepare datasets for lstm experiments 

library(magrittr)
library(purrr)
library(data.table)
library(dplyr)
library(mltools)

# source('./Utilities/helpers-1prepare.R')
source('~/Documents/PhdProjects/Project-Paper2/Utilities/helpers-1prepareLSTM.R')

# ============================= # 
# AKI first 
# ============================= # 

dataPathAKI <- '~/Documents/Data/Project2/AKI/'

plistAKI_day1 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/pList1day.RData'))
plistAKI_day2 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/pList2day.RData'))
plistAKI_day3 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/pList3day.RData'))
plistAKI_day4 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/pList4day.RData'))
plistAKI_day5 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/pList5day.RData'))
plistAKI_day6 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/pList6day.RData'))
plistAKI_day7 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/pList7day.RData'))

# check how many would be below 24 in each cohort (before splitting )
# the problem would be most relevant for day1 


outcomes_day1_aki <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/alloutcomes_aki_day1.RData'))
outcomes_day2_aki <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/alloutcomes_aki_day2.RData'))
outcomes_day3_aki <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/alloutcomes_aki_day3.RData'))
outcomes_day4_aki <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/alloutcomes_aki_day4.RData'))
outcomes_day5_aki <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/alloutcomes_aki_day5.RData'))
outcomes_day6_aki <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/alloutcomes_aki_day6.RData'))
outcomes_day7_aki <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/alloutcomes_aki_day7.RData'))

# tr1 <- outcomes_day1_aki$split1$trdf
# te1 <- outcomes_day1_aki$split1$tedf


# try1 <- baselineDataPrepare(outcome_tr = tr1, outcome_te = te1, wholedata = plistAKI_day1, maxwindow = 48)
# 
# try1$basedata_tr_xlist$X_ge18 %>% dim
# 
# try1$basedata_tr_ylist$ge24 %>% dim
# for day1, should only take up to 20 points


# try <- baselineDataPrepare(outcome_tr = outcomes_day2_aki$split1$trdf, 
#                     outcome_te = outcomes_day2_aki$split1$tedf, 
#                     wholedata = plistAKI_day2, 
#                     maxwindow = 48)
# 
# names(plistAKI_day2) %>% head
# paste0('icustay_', outcomes_day2_aki$split1$trdf$icustay_id) %in% names(plistAKI_day2)

# this should generate 350 datasets 
basedata_day1 <- list()
basedata_day2 <- list()
basedata_day3 <- list()
basedata_day4 <- list()
basedata_day5 <- list()
basedata_day6 <- list()
basedata_day7 <- list()

for(s in 1:50){
  basedata_day1[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day1_aki[[s]]$trdf, 
                                            outcome_te = outcomes_day1_aki[[s]]$tedf, 
                                            wholedata = plistAKI_day1, 
                                            maxwindow = 48)
  cat('day1 split ', s, ' done\n' )
  basedata_day2[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day2_aki[[s]]$trdf, 
                                            outcome_te = outcomes_day2_aki[[s]]$tedf, 
                                            wholedata = plistAKI_day2, 
                                            maxwindow = 48)
  cat('day2 split ', s, ' done\n' )
  
  basedata_day3[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day3_aki[[s]]$trdf, 
                                            outcome_te = outcomes_day3_aki[[s]]$tedf, 
                                            wholedata = plistAKI_day3, 
                                            maxwindow = 48)
  cat('day3 split ', s, ' done\n' )
  
  basedata_day4[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day4_aki[[s]]$trdf, 
                                            outcome_te = outcomes_day4_aki[[s]]$tedf, 
                                            wholedata = plistAKI_day4, 
                                            maxwindow = 48)
  cat('day4 split ', s, ' done\n' )
  
  basedata_day5[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day5_aki[[s]]$trdf, 
                                            outcome_te = outcomes_day5_aki[[s]]$tedf, 
                                            wholedata = plistAKI_day5, 
                                            maxwindow = 48)
  cat('day5 split ', s, ' done\n' )
  
  basedata_day6[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day6_aki[[s]]$trdf, 
                                            outcome_te = outcomes_day6_aki[[s]]$tedf, 
                                            wholedata = plistAKI_day6, 
                                            maxwindow = 48)
  cat('day6 split ', s, ' done\n' )
  
  basedata_day7[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day7_aki[[s]]$trdf, 
                                            outcome_te = outcomes_day7_aki[[s]]$tedf, 
                                            wholedata = plistAKI_day7, 
                                            maxwindow = 48)
  cat('day7 split ', s, ' done\n' )
  
}


names(basedata_day1) <- paste0('split', 1:50)
names(basedata_day2) <- paste0('split', 1:50)
names(basedata_day3) <- paste0('split', 1:50)
names(basedata_day4) <- paste0('split', 1:50)
names(basedata_day5) <- paste0('split', 1:50)
names(basedata_day6) <- paste0('split', 1:50)
names(basedata_day7) <- paste0('split', 1:50)



saveRDS(basedata_day1, file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day1_50splits.RData'))
saveRDS(basedata_day2, file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day2_50splits.RData'))
saveRDS(basedata_day3, file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day3_50splits.RData'))
saveRDS(basedata_day4, file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day4_50splits.RData'))
saveRDS(basedata_day5, file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day5_50splits.RData'))
saveRDS(basedata_day6, file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day6_50splits.RData'))
saveRDS(basedata_day7, file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day7_50splits.RData'))






