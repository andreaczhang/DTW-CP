library(magrittr)
library(purrr)
source('~/Documents/Work/Paper2/Utilities/helpers-rev-5postTensor.R')

dataPath <- '~/Documents/Data_backup/REVProject2/'
splitPath_sep <- 'splitdata-sepsis-rev/'


# outcomes 
outlist_day1 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day1.RData'))
outlist_day2 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day2.RData'))
outlist_day3 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day3.RData'))
outlist_day4 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day4.RData'))
outlist_day5 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day5.RData'))
outlist_day6 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day6.RData'))
outlist_day7 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day7.RData'))


# IMPORTANT: MUST MATCH THE DATA TYPE! 
# pivot matrices (10 splits)
dtwType <- 'sc05'  
# itakura, sc05

trteMat_d1_10splits <- readRDS(file = paste0(dataPath, splitPath_sep, 'day1-', dtwType, '-pivotMats.RData'))
trteMat_d2_10splits <- readRDS(file = paste0(dataPath, splitPath_sep, 'day2-', dtwType, '-pivotMats.RData'))
trteMat_d3_10splits <- readRDS(file = paste0(dataPath, splitPath_sep, 'day3-', dtwType, '-pivotMats.RData'))
trteMat_d4_10splits <- readRDS(file = paste0(dataPath, splitPath_sep, 'day4-', dtwType, '-pivotMats.RData'))
trteMat_d5_10splits <- readRDS(file = paste0(dataPath, splitPath_sep, 'day5-', dtwType, '-pivotMats.RData'))
trteMat_d6_10splits <- readRDS(file = paste0(dataPath, splitPath_sep, 'day6-', dtwType, '-pivotMats.RData'))
trteMat_d7_10splits <- readRDS(file = paste0(dataPath, splitPath_sep, 'day7-', dtwType, '-pivotMats.RData'))

# feat matries 
fmList_R_10splits <- readRDS(file = paste0(dataPath, splitPath_sep, 'fm-', dtwType, '-7d10splits.RData'))



# ==== put projection and outcomes together ==== # 

# outlist_day1$split1$trdf$death
# trteMat_d1_10splits$split1$trMat
# fmList_R_10splits$day1$split1$comp2


# d1s1$Xtrlist$comp2 %>% head
# d1s1$Xtrlist$comp20 %>% head

day1_trte <- list()
day2_trte <- list()
day3_trte <- list()
day4_trte <- list()
day5_trte <- list()
day6_trte <- list()
day7_trte <- list()

for(s in 1:10){
  day1_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day1[[s]], 
                                         pivotMatlist = trteMat_d1_10splits[[s]], 
                                         fmList = fmList_R_10splits$day1[[s]])
  
  day2_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day2[[s]], 
                                              pivotMatlist = trteMat_d2_10splits[[s]], 
                                              fmList = fmList_R_10splits$day2[[s]])
  
  day3_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day3[[s]], 
                                              pivotMatlist = trteMat_d3_10splits[[s]], 
                                              fmList = fmList_R_10splits$day3[[s]])
  
  day4_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day4[[s]], 
                                              pivotMatlist = trteMat_d4_10splits[[s]], 
                                              fmList = fmList_R_10splits$day4[[s]])
  
  day5_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day5[[s]], 
                                              pivotMatlist = trteMat_d5_10splits[[s]], 
                                              fmList = fmList_R_10splits$day5[[s]])
  
  day6_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day6[[s]], 
                                              pivotMatlist = trteMat_d6_10splits[[s]], 
                                              fmList = fmList_R_10splits$day6[[s]])
  
  day7_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day7[[s]], 
                                              pivotMatlist = trteMat_d7_10splits[[s]], 
                                              fmList = fmList_R_10splits$day7[[s]])
  
}

names(day1_trte) <- paste0('split', 1:10)
names(day2_trte) <- paste0('split', 1:10)
names(day3_trte) <- paste0('split', 1:10)
names(day4_trte) <- paste0('split', 1:10)
names(day5_trte) <- paste0('split', 1:10)
names(day6_trte) <- paste0('split', 1:10)
names(day7_trte) <- paste0('split', 1:10)

# day7_trte$split1$Xtrlist$comp2 %>% nrow
# day1_trte$split1$Xtrlist
readyPath <- 'readyTRTE-sep/'
saveRDS(day1_trte, file = paste0(dataPath, readyPath, 'day1-', dtwType, '-readytrte.RData'))
saveRDS(day2_trte, file = paste0(dataPath, readyPath, 'day2-', dtwType, '-readytrte.RData'))
saveRDS(day3_trte, file = paste0(dataPath, readyPath, 'day3-', dtwType, '-readytrte.RData'))
saveRDS(day4_trte, file = paste0(dataPath, readyPath, 'day4-', dtwType, '-readytrte.RData'))
saveRDS(day5_trte, file = paste0(dataPath, readyPath, 'day5-', dtwType, '-readytrte.RData'))
saveRDS(day6_trte, file = paste0(dataPath, readyPath, 'day6-', dtwType, '-readytrte.RData'))
saveRDS(day7_trte, file = paste0(dataPath, readyPath, 'day7-', dtwType, '-readytrte.RData'))





