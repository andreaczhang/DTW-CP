# prepare datasets for sepsis cohort 

library(magrittr)
library(purrr)
library(data.table)
library(dplyr)
library(mltools)

# source('./Utilities/helpers-1prepare.R')
source('~/Documents/PhdProjects/Project-Paper2/Utilities/helpers-1prepareLSTM.R')

# ============================= # 
# SEPSIS cohort
# ============================= # 

dataPathSep <- '~/Documents/Data/Project2/Ripoll2014/'

plist_sep_day1 <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/pList1day_sep.RData'))
plist_sep_day2 <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/pList2day_sep.RData'))
plist_sep_day3 <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/pList3day_sep.RData'))
plist_sep_day4 <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/pList4day_sep.RData'))
plist_sep_day5 <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/pList5day_sep.RData'))
plist_sep_day6 <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/pList6day_sep.RData'))
plist_sep_day7 <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/pList7day_sep.RData'))


outcomes_day1_Sep <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/alloutcomes_sep_day1.RData'))
outcomes_day2_Sep <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/alloutcomes_sep_day2.RData'))
outcomes_day3_Sep <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/alloutcomes_sep_day3.RData'))
outcomes_day4_Sep <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/alloutcomes_sep_day4.RData'))
outcomes_day5_Sep <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/alloutcomes_sep_day5.RData'))
outcomes_day6_Sep <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/alloutcomes_sep_day6.RData'))
outcomes_day7_Sep <- readRDS(file = paste0(dataPathSep, 'REVISION-RIPOLL/alloutcomes_sep_day7.RData'))


# paste0('icustay_',  outcomes_day1_Sep$split1$trdf$icustay_id) %in% names(plist_sep_day1)




basedata_day1 <- list()
basedata_day2 <- list()
basedata_day3 <- list()
basedata_day4 <- list()
basedata_day5 <- list()
basedata_day6 <- list()
basedata_day7 <- list()


for(s in 1:10){
  basedata_day1[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day1_Sep[[s]]$trdf, 
                                            outcome_te = outcomes_day1_Sep[[s]]$tedf, 
                                            wholedata = plist_sep_day1, 
                                            maxwindow = 48)
  cat('day1 split ', s, ' done\n' )
  basedata_day2[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day2_Sep[[s]]$trdf, 
                                            outcome_te = outcomes_day2_Sep[[s]]$tedf, 
                                            wholedata = plist_sep_day2, 
                                            maxwindow = 48)
  cat('day2 split ', s, ' done\n' )
  
  basedata_day3[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day3_Sep[[s]]$trdf, 
                                            outcome_te = outcomes_day3_Sep[[s]]$tedf, 
                                            wholedata = plist_sep_day3, 
                                            maxwindow = 48)
  cat('day3 split ', s, ' done\n' )
  
  basedata_day4[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day4_Sep[[s]]$trdf, 
                                            outcome_te = outcomes_day4_Sep[[s]]$tedf, 
                                            wholedata = plist_sep_day4, 
                                            maxwindow = 48)
  cat('day4 split ', s, ' done\n' )
  
  basedata_day5[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day5_Sep[[s]]$trdf, 
                                            outcome_te = outcomes_day5_Sep[[s]]$tedf, 
                                            wholedata = plist_sep_day5, 
                                            maxwindow = 48)
  cat('day5 split ', s, ' done\n' )
  basedata_day6[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day6_Sep[[s]]$trdf, 
                                            outcome_te = outcomes_day6_Sep[[s]]$tedf, 
                                            wholedata = plist_sep_day6, 
                                            maxwindow = 48)
  cat('day6 split ', s, ' done\n' )
  
  basedata_day7[[s]] <- baselineDataPrepare(outcome_tr = outcomes_day7_Sep[[s]]$trdf, 
                                            outcome_te = outcomes_day7_Sep[[s]]$tedf, 
                                            wholedata = plist_sep_day7, 
                                            maxwindow = 48)
  cat('day7 split ', s, ' done\n' )
  

}


names(basedata_day1) <- paste0('split', 1:10)
names(basedata_day2) <- paste0('split', 1:10)
names(basedata_day3) <- paste0('split', 1:10)
names(basedata_day4) <- paste0('split', 1:10)
names(basedata_day5) <- paste0('split', 1:10)
names(basedata_day6) <- paste0('split', 1:10)
names(basedata_day7) <- paste0('split', 1:10)



saveRDS(basedata_day1, file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/day1_10splits.RData'))
saveRDS(basedata_day2, file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/day2_10splits.RData'))
saveRDS(basedata_day3, file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/day3_10splits.RData'))
saveRDS(basedata_day4, file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/day4_10splits.RData'))
saveRDS(basedata_day5, file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/day5_10splits.RData'))
saveRDS(basedata_day6, file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/day6_10splits.RData'))
saveRDS(basedata_day7, file = paste0(dataPathSep, 'REVISION-RIPOLL/data_baseline_sep/day7_10splits.RData'))







