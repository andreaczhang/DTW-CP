# get projection <PART 2> (this time in R)
# 2. query (by index) the pivot distances

# first need to examine the outcomes 
library(magrittr)
library(purrr)
source('./utility/3-tensors.R')

dataPath <- 'data/path'
splitPath_aki <- 'splitdata-aki-rev/'
simMatPath_aki <- 'dtw-aki-proc/'


# for the trial purpose, take 1-4 days's first split 
outlist_day1 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day1.RData'))
outlist_day2 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day2.RData'))
outlist_day3 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day3.RData'))
outlist_day4 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day4.RData'))
outlist_day5 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day5.RData'))
outlist_day6 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day6.RData'))
outlist_day7 <- readRDS(paste0(dataPath, splitPath_aki, 'alloutcomes_aki_day7.RData'))



# start with day 1, tr and test
# training 

trID_day1_50splits <- map(outlist_day1, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day2_50splits <- map(outlist_day2, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day3_50splits <- map(outlist_day3, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day4_50splits <- map(outlist_day4, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day5_50splits <- map(outlist_day5, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day6_50splits <- map(outlist_day6, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day7_50splits <- map(outlist_day7, function(x){paste0('icustay_', x$trdf$icustay_id)})

# test
teID_day1_50splits <- map(outlist_day1, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day2_50splits <- map(outlist_day2, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day3_50splits <- map(outlist_day3, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day4_50splits <- map(outlist_day4, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day5_50splits <- map(outlist_day5, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day6_50splits <- map(outlist_day6, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day7_50splits <- map(outlist_day7, function(x){paste0('icustay_', x$tedf$icustay_id)})






pivot <- 1
dtwsimmat_name <- 'aki-sc05-feat'
### the name should change when we change the featmat type!


colList_pivot_day1 <- list()
colList_pivot_day2 <- list()
colList_pivot_day3 <- list()
colList_pivot_day4 <- list()
colList_pivot_day5 <- list()
colList_pivot_day6 <- list()
colList_pivot_day7 <- list()


for(f in 1:52){  
  feat <- readRDS(paste0(dataPath, simMatPath_aki, dtwsimmat_name, f, '.RData'))
  cat('Feat ', f, ' of ', dtwsimmat_name, ' loaded\n')
  
  # ======== query all splits, all days for this feature ========= # 
  colList_pvt_d1_50splits <- list()
  colList_pvt_d2_50splits <- list()
  colList_pvt_d3_50splits <- list()
  colList_pvt_d4_50splits <- list()
  colList_pvt_d5_50splits <- list()
  colList_pvt_d6_50splits <- list()
  colList_pvt_d7_50splits <- list()
  
  for(s in 1:50){
    colList_pvt_d1_50splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day1_50splits[[s]],
                                                       teIDvec = teID_day1_50splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d2_50splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day2_50splits[[s]],
                                                       teIDvec = teID_day2_50splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d3_50splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day3_50splits[[s]],
                                                       teIDvec = teID_day3_50splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d4_50splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day4_50splits[[s]],
                                                       teIDvec = teID_day4_50splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d5_50splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day5_50splits[[s]],
                                                       teIDvec = teID_day5_50splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d6_50splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day6_50splits[[s]],
                                                       teIDvec = teID_day6_50splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d7_50splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day7_50splits[[s]],
                                                       teIDvec = teID_day7_50splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    cat('split', s, ' done\n')
  }
  # save 10 splits (column) of this feature
  colList_pivot_day1[[f]] <- colList_pvt_d1_50splits
  colList_pivot_day2[[f]] <- colList_pvt_d2_50splits
  colList_pivot_day3[[f]] <- colList_pvt_d3_50splits
  colList_pivot_day4[[f]] <- colList_pvt_d4_50splits
  colList_pivot_day5[[f]] <- colList_pvt_d5_50splits
  colList_pivot_day6[[f]] <- colList_pvt_d6_50splits
  colList_pivot_day7[[f]] <- colList_pvt_d7_50splits
  
  cat('Feat ', f, ' done\n')
}


# ========== gather into matrices =========== # 
# colList_pivot_day1[[2]]$split1$TRcol %>% head

trteMat_d1_50splits <- list()
trteMat_d2_50splits <- list()
trteMat_d3_50splits <- list()
trteMat_d4_50splits <- list()
trteMat_d5_50splits <- list()
trteMat_d6_50splits <- list()
trteMat_d7_50splits <- list()

for(s in 1:50){
  trteMat_d1_50splits[[s]] <- map(colList_pivot_day1, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d2_50splits[[s]] <- map(colList_pivot_day2, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d3_50splits[[s]] <- map(colList_pivot_day3, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d4_50splits[[s]] <- map(colList_pivot_day4, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d5_50splits[[s]] <- map(colList_pivot_day5, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d6_50splits[[s]] <- map(colList_pivot_day6, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d7_50splits[[s]] <- map(colList_pivot_day7, function(x){x[[s]]}) %>% gatherColsTRTE
  
}

names(trteMat_d1_50splits) <- paste0('split', 1:50)
names(trteMat_d2_50splits) <- paste0('split', 1:50)
names(trteMat_d3_50splits) <- paste0('split', 1:50)
names(trteMat_d4_50splits) <- paste0('split', 1:50)
names(trteMat_d5_50splits) <- paste0('split', 1:50)
names(trteMat_d6_50splits) <- paste0('split', 1:50)
names(trteMat_d7_50splits) <- paste0('split', 1:50)



##### the name need to be changed once we change the distance type! 
dtwType <- 'sc05'
saveRDS(trteMat_d1_50splits, file = paste0(dataPath, splitPath_aki, 'day1-', dtwType, '-pivotMats.RData'))
saveRDS(trteMat_d2_50splits, file = paste0(dataPath, splitPath_aki, 'day2-', dtwType, '-pivotMats.RData'))
saveRDS(trteMat_d3_50splits, file = paste0(dataPath, splitPath_aki, 'day3-', dtwType, '-pivotMats.RData'))
saveRDS(trteMat_d4_50splits, file = paste0(dataPath, splitPath_aki, 'day4-', dtwType, '-pivotMats.RData'))
saveRDS(trteMat_d5_50splits, file = paste0(dataPath, splitPath_aki, 'day5-', dtwType, '-pivotMats.RData'))
saveRDS(trteMat_d6_50splits, file = paste0(dataPath, splitPath_aki, 'day6-', dtwType, '-pivotMats.RData'))
saveRDS(trteMat_d7_50splits, file = paste0(dataPath, splitPath_aki, 'day7-', dtwType, '-pivotMats.RData'))



