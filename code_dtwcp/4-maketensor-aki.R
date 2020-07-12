# make tensor for: each day's training set (3d array). convert to .mat
# make slices for: the corresponding slices of pivot (a few matrices) (this can wait)

library(magrittr)
library(purrr)
library(R.matlab)
source('./utility/3-tensors.R')

dataPath <- 'path/to/data/aki/'

splitPath_aki <- 'splitdata-aki-rev/'
simMatPath_aki <- 'dtw-aki-proc/'

dtwoption <- 'sc05'
# default, itakura, sc05

dtwsimmat_name <- paste0('aki-', dtwoption, '-feat')



outcomes_day1 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day1.RData'))
outcomes_day2 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day2.RData'))
outcomes_day3 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day3.RData'))
outcomes_day4 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day4.RData'))
outcomes_day5 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day5.RData'))
outcomes_day6 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day6.RData'))
outcomes_day7 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day7.RData'))



# produce the ID for each day, 10 splits together
trID_day1 <- map(outcomes_day1, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day2 <- map(outcomes_day2, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day3 <- map(outcomes_day3, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day4 <- map(outcomes_day4, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day5 <- map(outcomes_day5, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day6 <- map(outcomes_day6, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day7 <- map(outcomes_day7, function(x){paste0('icustay_', x$trdf$icustay_id)})
# 

matlist_day1 <- list()
matlist_day2 <- list()
matlist_day3 <- list()
matlist_day4 <- list()
matlist_day5 <- list()
matlist_day6 <- list()
matlist_day7 <- list()


matchID_day1 <- map(trID_day1, function(x){match(x, names(feat))})
matchID_day2 <- map(trID_day2, function(x){match(x, names(feat))})
matchID_day3 <- map(trID_day3, function(x){match(x, names(feat))})
matchID_day4 <- map(trID_day4, function(x){match(x, names(feat))})
matchID_day5 <- map(trID_day5, function(x){match(x, names(feat))})
matchID_day6 <- map(trID_day6, function(x){match(x, names(feat))})
matchID_day7 <- map(trID_day7, function(x){match(x, names(feat))})
# 
# saveRDS(list(matchID_day1 = matchID_day1, 
#              matchID_day2 = matchID_day2, 
#              matchID_day3 = matchID_day3, 
#              matchID_day4 = matchID_day4, 
#              matchID_day5 = matchID_day5, 
#              matchID_day6 = matchID_day6, 
#              matchID_day7 = matchID_day7), 
#         file = paste0(paste0(dataPath, splitPath_aki, 'IDtrte_all_aki.RData')))



IDs <- readRDS(file = paste0(paste0(dataPath, splitPath_aki, 'IDtrte_all_aki.RData')))


feat <- list()
for(f in 1:52){  
  feat[[f]] <- readRDS(paste0(dataPath, simMatPath_aki, dtwsimmat_name, f, '.RData'))
  cat('Feat ', f, ' loaded\n')
  # this one should be the same for all 52 feats
}




# ======== put all splits, all days, all 52 features into array ========= # 

# when putting inside the array, need to match split! 


# =========== start making tensors ======== # 
matlist_day <- list()
for(f in 1:52){
  matlist_day[[f]] <- map(IDs$matchID_day1, function(x){feat[[f]][x, x]})
  cat('feat ', f, ' done\n')
}


for(s in 1:50){
  # this is for each split, and saved into each split folder
  
  tensor_day <- map(matlist_day, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_aki, 'tensors_aki/day1_aki_trtensor_', dtwoption, '_split', s, '.mat'), 
           trtensor = tensor_day)
  
  cat('Day 1 ', dtwoption, ' trtensor ', s, ' split done\n')
}


rm(matlist_day)
rm(tensor_day)
gc()


# ========== day 2 ========= # 

matlist_day <- list()
for(f in 1:52){
  matlist_day[[f]] <- map(IDs$matchID_day2, function(x){feat[[f]][x, x]})
  cat('feat ', f, ' done\n')
}


for(s in 1:50){
  # this is for each split, and saved into each split folder
  
  tensor_day <- map(matlist_day, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_aki, 'tensors_aki/day2_aki_trtensor_', dtwoption, '_split', s, '.mat'), 
           trtensor = tensor_day)

  cat('Day 2 ', dtwoption, ' trtensor ', s, ' split done\n')
}


rm(matlist_day)
rm(tensor_day)
gc()


# ========== day 3 ========= # 

matlist_day <- list()
for(f in 1:52){
  matlist_day[[f]] <- map(IDs$matchID_day3, function(x){feat[[f]][x, x]})
  cat('feat ', f, ' done\n')
}


for(s in 1:50){
  # this is for each split, and saved into each split folder
  
  tensor_day <- map(matlist_day, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_aki, 'tensors_aki/day3_aki_trtensor_', dtwoption, '_split', s, '.mat'), 
           trtensor = tensor_day)

  cat('Day 3 ', dtwoption, ' trtensor ', s, ' split done\n')
}

rm(matlist_day)
rm(tensor_day)
gc()

# ========== day 4 ========= # 

matlist_day <- list()
for(f in 1:52){
  matlist_day[[f]] <- map(IDs$matchID_day4, function(x){feat[[f]][x, x]})
  cat('feat ', f, ' done\n')
}


for(s in 1:50){
  # this is for each split, and saved into each split folder
  
  tensor_day <- map(matlist_day, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_aki, 'tensors_aki/day4_aki_trtensor_', dtwoption, '_split', s, '.mat'), 
           trtensor = tensor_day)
  gc()
  cat('Day 4 ', dtwoption, ' trtensor ', s, ' split done\n')
}

rm(matlist_day)
rm(tensor_day)
gc()


# ========== day 5 ========= # 

matlist_day <- list()
for(f in 1:52){
  matlist_day[[f]] <- map(IDs$matchID_day5, function(x){feat[[f]][x, x]})
  cat('feat ', f, ' done\n')
}


for(s in 1:50){
  # this is for each split, and saved into each split folder
  
  tensor_day <- map(matlist_day, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_aki, 'tensors_aki/day5_aki_trtensor_', dtwoption, '_split', s, '.mat'), 
           trtensor = tensor_day)
  gc()
  cat('Day 5 ', dtwoption, ' trtensor ', s, ' split done\n')
}

rm(matlist_day)
rm(tensor_day)
gc()


# ========== day 6 ========= # 

matlist_day <- list()
for(f in 1:52){
  matlist_day[[f]] <- map(IDs$matchID_day6, function(x){feat[[f]][x, x]})
  cat('feat ', f, ' done\n')
}


for(s in 1:50){
  # this is for each split, and saved into each split folder
  
  tensor_day <- map(matlist_day, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_aki, 'tensors_aki/day6_aki_trtensor_', dtwoption, '_split', s, '.mat'), 
           trtensor = tensor_day)
  gc()
  cat('Day 6 ', dtwoption, ' trtensor ', s, ' split done\n')
}

rm(matlist_day)
rm(tensor_day)
gc()


# ========== day 7 ========= # 

matlist_day <- list()
for(f in 1:52){
  matlist_day[[f]] <- map(IDs$matchID_day7, function(x){feat[[f]][x, x]})
  cat('feat ', f, ' done\n')
}


for(s in 1:50){
  # this is for each split, and saved into each split folder
  
  tensor_day <- map(matlist_day, pluck(s)) %>% make_array
  writeMat(paste0(dataPath, splitPath_aki, 'tensors_aki/day7_aki_trtensor_', dtwoption, '_split', s, '.mat'), 
           trtensor = tensor_day)
  gc()
  cat('Day 7 ', dtwoption, ' trtensor ', s, ' split done\n')
}

rm(matlist_day)
rm(tensor_day)
gc()


