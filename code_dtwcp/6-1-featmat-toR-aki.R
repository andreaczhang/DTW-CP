# get projection <PART 1> (this time in R)
# 1. make the matlab files into Rdata compatible, which can be done on the pink
library(magrittr)
library(purrr)
library(R.matlab)
source('./Utilities/helpers-rev-5postTensor.R')

dataPath <- '~/Documents/Data_backup/REVProject2/featmat-aki/'


# feat_day1_split1_default <- readMat(paste0(dataPath, featMatPath, 'day1aki_featmats_default_split1.mat'))
# the structure is of this type 
# feat_day1_split1_default[[1]][[1]][[1]] %>% head
# feat_day1_split1_default[[1]][[2]][[1]] %>% head
# d1s1 <- featMat_matlabToR(feat_day1_split1_default)
# d1s1$comp5 %>% head

# read the TRIAL FEATMATS
# at a later point the split need to rotate too

fmList_thisday_default <- list()
fmList_thisday_itakura <- list()
fmList_thisday_sc05 <- list()

for(i in 1:7){
  # for each day, one list
  fmList_50splits_default <- list()
  fmList_50splits_itakura <- list()
  fmList_50splits_sc05 <- list()
  
  # each split 
  for(s in 1:50){
    fmList_50splits_default[[s]] <- readMat(paste0(dataPath, 'day', i, '_aki_featmats_default_split', s, '.mat'))
    fmList_50splits_itakura[[s]] <- readMat(paste0(dataPath, 'day', i, '_aki_featmats_itakura_split', s, '.mat'))
    fmList_50splits_sc05[[s]] <- readMat(paste0(dataPath, 'day', i, '_aki_featmats_sc05_split', s, '.mat'))
    
    cat('day ', i, ' split ', s, 'done\n')
  }
  names(fmList_50splits_default) <- paste0('split', 1:50)
  names(fmList_50splits_itakura) <- paste0('split', 1:50)
  names(fmList_50splits_sc05) <- paste0('split', 1:50)
  
  fmList_thisday_default[[i]] <- fmList_50splits_default
  fmList_thisday_itakura[[i]] <- fmList_50splits_itakura
  fmList_thisday_sc05[[i]] <- fmList_50splits_sc05
  
}


names(fmList_thisday_default) <- paste0('day', 1:7)
names(fmList_thisday_itakura) <- paste0('day', 1:7)
names(fmList_thisday_sc05) <- paste0('day', 1:7)

# fmList_thisday_default$day1$split1$featmats %>% length


fmList_R_50splits_default <- list()
fmList_R_50splits_itakura <- list()
fmList_R_50splits_sc05 <- list()

for(i in 1:7){
  fmList_R_50splits_default[[i]] <- map(fmList_thisday_default[[i]], featMat_matlabToR)
  fmList_R_50splits_itakura[[i]] <- map(fmList_thisday_itakura[[i]], featMat_matlabToR)
  fmList_R_50splits_sc05[[i]] <- map(fmList_thisday_sc05[[i]], featMat_matlabToR)
  
}

names(fmList_R_50splits_default) <- paste0('day', 1:7)
names(fmList_R_50splits_itakura) <- paste0('day', 1:7)
names(fmList_R_50splits_sc05) <- paste0('day', 1:7)

# fmList_R_50splits_default$day1$split1$comp10 %>% head
# save 
savePath <- '~/Documents/Data_backup/REVProject2/'
saveRDS(fmList_R_50splits_default, file = paste0(savePath, 'splitdata-aki-rev/fm-default-7d50splits.RData'))
saveRDS(fmList_R_50splits_itakura, file = paste0(savePath, 'splitdata-aki-rev/fm-itakura-7d50splits.RData'))
saveRDS(fmList_R_50splits_sc05, file = paste0(savePath, 'splitdata-aki-rev/fm-sc05-7d50splits.RData'))






