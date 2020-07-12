# get projection <PART 2> (this time in R)
# 2. query (by index) the pivot distances (on pink)

# first need to examine the outcomes 
library(magrittr)
library(purrr)
source('~/Documents/Work/Paper2/Utilities/helpers-rev-5postTensor.R')

dataPath <- '~/Documents/Data_backup/REVProject2/'
splitPath_sep <- 'splitdata-sepsis-rev/'
simMatPath_sep <- 'dtw-sep-proc/'


# for the trial purpose, take 1-4 days's first split 
outlist_day1 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day1.RData'))
outlist_day2 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day2.RData'))
outlist_day3 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day3.RData'))
outlist_day4 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day4.RData'))
outlist_day5 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day5.RData'))
outlist_day6 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day6.RData'))
outlist_day7 <- readRDS(paste0(dataPath, splitPath_sep, 'alloutcomes_sep_day7.RData'))



# start with day 1, tr and test
# training 

trID_day1_10splits <- map(outlist_day1, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day2_10splits <- map(outlist_day2, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day3_10splits <- map(outlist_day3, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day4_10splits <- map(outlist_day4, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day5_10splits <- map(outlist_day5, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day6_10splits <- map(outlist_day6, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day7_10splits <- map(outlist_day7, function(x){paste0('icustay_', x$trdf$icustay_id)})

# test
teID_day1_10splits <- map(outlist_day1, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day2_10splits <- map(outlist_day2, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day3_10splits <- map(outlist_day3, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day4_10splits <- map(outlist_day4, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day5_10splits <- map(outlist_day5, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day6_10splits <- map(outlist_day6, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day7_10splits <- map(outlist_day7, function(x){paste0('icustay_', x$tedf$icustay_id)})




# feat1 <- readRDS(paste0(dataPath, simMatPath_sep, dtwsimmat_name, 1, '.RData'))

# dim(feat1)  # 1827, therefore need to match first 

# f1d1s1 <- trteOneCol_eachDay(trIDvec = trID_day1_split1, 
#                              teIDvec = teID_day1_split1, 
#                              singleFeatMat = feat1, 
#                              pivotLoc = 2)

# double check: this is the pivot (2 in training set, 12 in overall)
# trID_day1_split1[2]
# which(names(feat1) == 'icustay_224092')
# 
# id_terows <- match(trID_day1_split1, names(feat1))
# trID_day1_split1 %>% head
# 
# names(feat1[id_terows]) %>% head


pivot <- 1
dtwsimmat_name <- 'sepfull-sc05-feat'
### the name should change when we change the featmat type!


colList_pivot_day1 <- list()
colList_pivot_day2 <- list()
colList_pivot_day3 <- list()
colList_pivot_day4 <- list()
colList_pivot_day5 <- list()
colList_pivot_day6 <- list()
colList_pivot_day7 <- list()


for(f in 1:52){  
  feat <- readRDS(paste0(dataPath, simMatPath_sep, dtwsimmat_name, f, '.RData'))
  cat('Feat ', f, ' of ', dtwsimmat_name, ' loaded\n')
  
  # ======== query all splits, all days for this feature ========= # 
  colList_pvt_d1_10splits <- list()
  colList_pvt_d2_10splits <- list()
  colList_pvt_d3_10splits <- list()
  colList_pvt_d4_10splits <- list()
  colList_pvt_d5_10splits <- list()
  colList_pvt_d6_10splits <- list()
  colList_pvt_d7_10splits <- list()
  
  for(s in 1:10){
    colList_pvt_d1_10splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day1_10splits[[s]],
                                                       teIDvec = teID_day1_10splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d2_10splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day2_10splits[[s]],
                                                       teIDvec = teID_day2_10splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d3_10splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day3_10splits[[s]],
                                                       teIDvec = teID_day3_10splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d4_10splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day4_10splits[[s]],
                                                       teIDvec = teID_day4_10splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d5_10splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day5_10splits[[s]],
                                                       teIDvec = teID_day5_10splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d6_10splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day6_10splits[[s]],
                                                       teIDvec = teID_day6_10splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
    colList_pvt_d7_10splits[[s]] <- trteOneCol_eachDay(trIDvec = trID_day7_10splits[[s]],
                                                       teIDvec = teID_day7_10splits[[s]],
                                                       singleFeatMat = feat,
                                                       pivotLoc = pivot)
    
  }
  # save 10 splits (column) of this feature
  colList_pivot_day1[[f]] <- colList_pvt_d1_10splits
  colList_pivot_day2[[f]] <- colList_pvt_d2_10splits
  colList_pivot_day3[[f]] <- colList_pvt_d3_10splits
  colList_pivot_day4[[f]] <- colList_pvt_d4_10splits
  colList_pivot_day5[[f]] <- colList_pvt_d5_10splits
  colList_pivot_day6[[f]] <- colList_pvt_d6_10splits
  colList_pivot_day7[[f]] <- colList_pvt_d7_10splits
  
  cat('Feat ', f, ' done\n')
}


# ========== gather into matrices =========== # 
# colList_pivot_day1[[2]]$split1$TRcol %>% head

trteMat_d1_10splits <- list()
trteMat_d2_10splits <- list()
trteMat_d3_10splits <- list()
trteMat_d4_10splits <- list()
trteMat_d5_10splits <- list()
trteMat_d6_10splits <- list()
trteMat_d7_10splits <- list()

for(s in 1:10){
  trteMat_d1_10splits[[s]] <- map(colList_pivot_day1, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d2_10splits[[s]] <- map(colList_pivot_day2, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d3_10splits[[s]] <- map(colList_pivot_day3, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d4_10splits[[s]] <- map(colList_pivot_day4, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d5_10splits[[s]] <- map(colList_pivot_day5, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d6_10splits[[s]] <- map(colList_pivot_day6, function(x){x[[s]]}) %>% gatherColsTRTE
  trteMat_d7_10splits[[s]] <- map(colList_pivot_day7, function(x){x[[s]]}) %>% gatherColsTRTE
  
}

names(trteMat_d1_10splits) <- paste0('split', 1:10)
names(trteMat_d2_10splits) <- paste0('split', 1:10)
names(trteMat_d3_10splits) <- paste0('split', 1:10)
names(trteMat_d4_10splits) <- paste0('split', 1:10)
names(trteMat_d5_10splits) <- paste0('split', 1:10)
names(trteMat_d6_10splits) <- paste0('split', 1:10)
names(trteMat_d7_10splits) <- paste0('split', 1:10)



##### the name need to be changed once we change the distance type! 
saveRDS(trteMat_d1_10splits, file = paste0(dataPath, splitPath_sep, 'day1-sc05-pivotMats.RData'))
saveRDS(trteMat_d2_10splits, file = paste0(dataPath, splitPath_sep, 'day2-sc05-pivotMats.RData'))
saveRDS(trteMat_d3_10splits, file = paste0(dataPath, splitPath_sep, 'day3-sc05-pivotMats.RData'))
saveRDS(trteMat_d4_10splits, file = paste0(dataPath, splitPath_sep, 'day4-sc05-pivotMats.RData'))
saveRDS(trteMat_d5_10splits, file = paste0(dataPath, splitPath_sep, 'day5-sc05-pivotMats.RData'))
saveRDS(trteMat_d6_10splits, file = paste0(dataPath, splitPath_sep, 'day6-sc05-pivotMats.RData'))
saveRDS(trteMat_d7_10splits, file = paste0(dataPath, splitPath_sep, 'day7-sc05-pivotMats.RData'))



