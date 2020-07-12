# do the classification for all days, all splits 

library(magrittr)
library(purrr)
library(tictoc)
library(caret)
library(caretEnsemble)
library(Metrics)
library(pROC)
library(Metrics)  
library(PRROC)  
source('./utility/4-classify.R')

dataPath <- 'data/path'
readyTRTE_path <- 'readyTRTE-sep/'

dtwType <- 'default'

cat('Processing data type ', dtwType, '\n')

day1_readytrte <- readRDS(paste0(dataPath, readyTRTE_path, 'day1-', dtwType, '-readytrte.RData'))
day2_readytrte <- readRDS(paste0(dataPath, readyTRTE_path, 'day2-', dtwType, '-readytrte.RData'))
day3_readytrte <- readRDS(paste0(dataPath, readyTRTE_path, 'day3-', dtwType, '-readytrte.RData'))
day4_readytrte <- readRDS(paste0(dataPath, readyTRTE_path, 'day4-', dtwType, '-readytrte.RData'))
day5_readytrte <- readRDS(paste0(dataPath, readyTRTE_path, 'day5-', dtwType, '-readytrte.RData'))
day6_readytrte <- readRDS(paste0(dataPath, readyTRTE_path, 'day6-', dtwType, '-readytrte.RData'))
day7_readytrte <- readRDS(paste0(dataPath, readyTRTE_path, 'day7-', dtwType, '-readytrte.RData'))


readyTRTElist <- list(day1 = day1_readytrte, 
                      day2 = day2_readytrte, 
                      day3 = day3_readytrte, 
                      day4 = day4_readytrte, 
                      day5 = day5_readytrte, 
                      day6 = day6_readytrte, 
                      day7 = day7_readytrte)
# the function being used in the past: shouuld still work 



for(i in 1:7){
  
  for(s in 1:10){
    reslist <- runLearners_allcomp(Xtr_allcomp = readyTRTElist[[i]][[s]]$Xtrlist, 
                                   Xte_allcomp = readyTRTElist[[i]][[s]]$Xtelist, 
                                   Ytr = readyTRTElist[[i]][[s]]$ytr, 
                                   Yte = readyTRTElist[[i]][[s]]$yte)
    
    saveRDS(reslist, file = paste0('result/path', 
                                   'sep-day', i, '-type-', dtwType, '-sp-', s, '.RData'))
    cat('day ', i , 'split ', s, ' for datatype ', dtwType, ' complete\n')
    
  }
}




