library(magrittr)
library(purrr)
source('~/Documents/Work/Paper2/Utilities/helpers-rev-5postTensor.R')

dataPath <- '~/Documents/Data_backup/REVProject2/'
splitPath_aki <- 'splitdata-aki-rev/'


# outcomes 
outlist_day1 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day1.RData'))
outlist_day2 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day2.RData'))
outlist_day3 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day3.RData'))
outlist_day4 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day4.RData'))
outlist_day5 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day5.RData'))
outlist_day6 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day6.RData'))
outlist_day7 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day7.RData'))


# IMPORTANT: MUST MATCH THE DATA TYPE! 
# pivot matrices (50 splits)
dtwType <- 'sc05'  
# itakura, sc05

trteMat_d1_50splits <- readRDS(file = paste0(dataPath, splitPath_aki, 'day1-', dtwType, '-pivotMats.RData'))
trteMat_d2_50splits <- readRDS(file = paste0(dataPath, splitPath_aki, 'day2-', dtwType, '-pivotMats.RData'))
trteMat_d3_50splits <- readRDS(file = paste0(dataPath, splitPath_aki, 'day3-', dtwType, '-pivotMats.RData'))
trteMat_d4_50splits <- readRDS(file = paste0(dataPath, splitPath_aki, 'day4-', dtwType, '-pivotMats.RData'))
trteMat_d5_50splits <- readRDS(file = paste0(dataPath, splitPath_aki, 'day5-', dtwType, '-pivotMats.RData'))
trteMat_d6_50splits <- readRDS(file = paste0(dataPath, splitPath_aki, 'day6-', dtwType, '-pivotMats.RData'))
trteMat_d7_50splits <- readRDS(file = paste0(dataPath, splitPath_aki, 'day7-', dtwType, '-pivotMats.RData'))

# feat matries 
fmList_R_50splits <- readRDS(file = paste0(dataPath, splitPath_aki, 'fm-', dtwType, '-7d50splits.RData'))



# ==== put projection and outcomes together ==== # 

# outlist_day1$split1$trdf$death
# trteMat_d1_50splits$split1$trMat
# fmList_R_50splits$day1$split1$comp2


# d1s1$Xtrlist$comp2 %>% head
# d1s1$Xtrlist$comp20 %>% head

day1_trte <- list()
day2_trte <- list()
day3_trte <- list()
day4_trte <- list()
day5_trte <- list()
day6_trte <- list()
day7_trte <- list()

for(s in 1:50){
  day1_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day1[[s]], 
                                              pivotMatlist = trteMat_d1_50splits[[s]], 
                                              fmList = fmList_R_50splits$day1[[s]])
  
  day2_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day2[[s]], 
                                              pivotMatlist = trteMat_d2_50splits[[s]], 
                                              fmList = fmList_R_50splits$day2[[s]])
  
  day3_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day3[[s]], 
                                              pivotMatlist = trteMat_d3_50splits[[s]], 
                                              fmList = fmList_R_50splits$day3[[s]])
  
  day4_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day4[[s]], 
                                              pivotMatlist = trteMat_d4_50splits[[s]], 
                                              fmList = fmList_R_50splits$day4[[s]])
  
  day5_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day5[[s]], 
                                              pivotMatlist = trteMat_d5_50splits[[s]], 
                                              fmList = fmList_R_50splits$day5[[s]])
  
  day6_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day6[[s]], 
                                              pivotMatlist = trteMat_d6_50splits[[s]], 
                                              fmList = fmList_R_50splits$day6[[s]])
  
  day7_trte[[s]] <- getProj_response_onesplit(outlist = outlist_day7[[s]], 
                                              pivotMatlist = trteMat_d7_50splits[[s]], 
                                              fmList = fmList_R_50splits$day7[[s]])
  
}

names(day1_trte) <- paste0('split', 1:50)
names(day2_trte) <- paste0('split', 1:50)
names(day3_trte) <- paste0('split', 1:50)
names(day4_trte) <- paste0('split', 1:50)
names(day5_trte) <- paste0('split', 1:50)
names(day6_trte) <- paste0('split', 1:50)
names(day7_trte) <- paste0('split', 1:50)

# day7_trte$split1$Xtrlist$comp2 %>% nrow
# day1_trte$split1$Xtrlist
readyPath <- 'readyTRTE-aki/'
saveRDS(day1_trte, file = paste0(dataPath, readyPath, 'day1-', dtwType, '-readytrte.RData'))
saveRDS(day2_trte, file = paste0(dataPath, readyPath, 'day2-', dtwType, '-readytrte.RData'))
saveRDS(day3_trte, file = paste0(dataPath, readyPath, 'day3-', dtwType, '-readytrte.RData'))
saveRDS(day4_trte, file = paste0(dataPath, readyPath, 'day4-', dtwType, '-readytrte.RData'))
saveRDS(day5_trte, file = paste0(dataPath, readyPath, 'day5-', dtwType, '-readytrte.RData'))
saveRDS(day6_trte, file = paste0(dataPath, readyPath, 'day6-', dtwType, '-readytrte.RData'))
saveRDS(day7_trte, file = paste0(dataPath, readyPath, 'day7-', dtwType, '-readytrte.RData'))





