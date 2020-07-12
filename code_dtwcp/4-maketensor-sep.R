# make tensor for: each day's training set (3d array). convert to .mat
# make slices for: the corresponding slices of pivot (a few matrices) (this can wait)

library(magrittr)
library(purrr)
library(R.matlab)
source('./utility/3-tensors.R')

dataPath <- 'path/to/data/aki/'
splitPath_sep <- 'splitdata-sepsis-rev/'
simMatPath_sep <- 'dtw-sep-proc/'

dtwoption <- 'sc05'
# default, itakura, sc05

dtwsimmat_name <- paste0('sepfull-', dtwoption, '-feat')
# 'sepfull-default-feat'
# 'sepfull-itakura-feat'
# 'sepfull-sc03-feat'
# 'sepfull-sc05-feat'

outcomes_day1 <- readRDS(paste0(dataPath, splitPath_sep, 'day1_sep_10splits/alloutcomes_sep.RData'))
outcomes_day2 <- readRDS(paste0(dataPath, splitPath_sep, 'day2_sep_10splits/alloutcomes_sep.RData'))
outcomes_day3 <- readRDS(paste0(dataPath, splitPath_sep, 'day3_sep_10splits/alloutcomes_sep.RData'))
outcomes_day4 <- readRDS(paste0(dataPath, splitPath_sep, 'day4_sep_10splits/alloutcomes_sep.RData'))
outcomes_day5 <- readRDS(paste0(dataPath, splitPath_sep, 'day5_sep_10splits/alloutcomes_sep.RData'))
outcomes_day6 <- readRDS(paste0(dataPath, splitPath_sep, 'day6_sep_10splits/alloutcomes_sep.RData'))
outcomes_day7 <- readRDS(paste0(dataPath, splitPath_sep, 'day7_sep_10splits/alloutcomes_sep.RData'))



# produce the ID for each day, 10 splits together
trID_day1 <- map(outcomes_day1, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day2 <- map(outcomes_day2, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day3 <- map(outcomes_day3, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day4 <- map(outcomes_day4, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day5 <- map(outcomes_day5, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day6 <- map(outcomes_day6, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day7 <- map(outcomes_day7, function(x){paste0('icustay_', x$trdf$icustay_id)})



matlist_day1 <- list()
matlist_day2 <- list()
matlist_day3 <- list()
matlist_day4 <- list()
matlist_day5 <- list()
matlist_day6 <- list()
matlist_day7 <- list()

for(f in 1:52){  
  feat <- readRDS(paste0(dataPath, simMatPath_sep, dtwsimmat_name, f, '.RData'))
  cat('Feat ', f, ' loaded\n')

  # ======== query all splits, all days for this feature ========= #
  matchID_day1 <- map(trID_day1, function(x){match(x, names(feat))})
  matchID_day2 <- map(trID_day2, function(x){match(x, names(feat))})
  matchID_day3 <- map(trID_day3, function(x){match(x, names(feat))})
  matchID_day4 <- map(trID_day4, function(x){match(x, names(feat))})
  matchID_day5 <- map(trID_day5, function(x){match(x, names(feat))})
  matchID_day6 <- map(trID_day6, function(x){match(x, names(feat))})
  matchID_day7 <- map(trID_day7, function(x){match(x, names(feat))})
  
  # this one should be the same for all 52 feats
   
     # produce the simmat for this day, 10 splits together
  matlist_day1[[f]] <- map(matchID_day1, function(x){feat[x, x]})
  matlist_day2[[f]] <- map(matchID_day2, function(x){feat[x, x]})
  matlist_day3[[f]] <- map(matchID_day3, function(x){feat[x, x]})
  matlist_day4[[f]] <- map(matchID_day4, function(x){feat[x, x]})
  matlist_day5[[f]] <- map(matchID_day5, function(x){feat[x, x]})
  matlist_day6[[f]] <- map(matchID_day6, function(x){feat[x, x]})
  matlist_day7[[f]] <- map(matchID_day7, function(x){feat[x, x]})

   cat('Feat ', f, ' done\n')
}




# ======== put all splits, all days, all 52 features into array ========= # 

# when putting inside the array, need to match split! 
tensor_day1 <- list()
tensor_day2 <- list()
tensor_day3 <- list()
tensor_day4 <- list()
tensor_day5 <- list()
tensor_day6 <- list()
tensor_day7 <- list()

for(s in 1:10){
# this is for each split, and saved into each split folder

  tensor_day1[[s]] <- map(matlist_day1, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_sep, 'day1_sep_10splits/', s, '/trtensor_', dtwoption, '.mat'), trtensor = tensor_day1[[s]])
  cat('Day 1 ', dtwoption, ' trtensor ', s, ' split done\n')

  tensor_day2[[s]] <- map(matlist_day2, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_sep, 'day2_sep_10splits/', s, '/trtensor_', dtwoption, '.mat'), trtensor = tensor_day2[[s]])
  cat('Day 2 ', dtwoption, ' trtensor ', s, ' split done\n')

  tensor_day3[[s]] <- map(matlist_day3, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_sep, 'day3_sep_10splits/', s, '/trtensor_', dtwoption, '.mat'), trtensor = tensor_day3[[s]])
  cat('Day 3 ', dtwoption, ' trtensor ', s, ' split done\n')

  tensor_day4[[s]] <- map(matlist_day4, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_sep, 'day4_sep_10splits/', s, '/trtensor_', dtwoption, '.mat'), trtensor = tensor_day4[[s]])
  cat('Day 4 ', dtwoption, ' trtensor ', s, ' split done\n')

  tensor_day5[[s]] <- map(matlist_day5, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_sep, 'day5_sep_10splits/', s, '/trtensor_', dtwoption, '.mat'), trtensor = tensor_day5[[s]])
  cat('Day 5 ', dtwoption, ' trtensor ', s, ' split done\n')
  #
  tensor_day6[[s]] <- map(matlist_day6, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_sep, 'day6_sep_10splits/', s, '/trtensor_', dtwoption, '.mat'), trtensor = tensor_day6[[s]])
  cat('Day 6 ', dtwoption, ' trtensor ', s, ' split done\n')

  tensor_day7[[s]] <- map(matlist_day7, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_sep, 'day7_sep_10splits/', s, '/trtensor_', dtwoption, '.mat'), trtensor = tensor_day7[[s]])
  cat('Day 7 ', dtwoption, ' trtensor ', s, ' split done\n')

}








