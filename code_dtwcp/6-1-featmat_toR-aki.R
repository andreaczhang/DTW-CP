# try to make normalised version 
# might not be any difference, but then we know for sure 

library(magrittr)
library(purrr)
library(R.matlab)
source('./utility/3-tensors.R')


dataPath <- 'data/path'

# first read the files 

fmList_thisday_default <- list()

for(i in 1:7){
  # for each day, one list
  fmList_50splits_default <- list()
  # each split 
  for(s in 1:50){
    fmList_50splits_default[[s]] <- readMat(paste0(dataPath, 'day', i, '_aki_featmats_default_split', s, '.mat'))
 
    cat('day ', i, ' split ', s, 'done\n')
  }
  names(fmList_50splits_default) <- paste0('split', 1:50)
  fmList_thisday_default[[i]] <- fmList_50splits_default
}

names(fmList_thisday_default) <- paste0('day', 1:7)


# put into R format 
fmList_R_50splits_default <- list()

for(i in 1:7){
  fmList_R_50splits_default[[i]] <- map(fmList_thisday_default[[i]], featMat_matlabToR)
 
}

names(fmList_R_50splits_default) <- paste0('day', 1:7)

fmList_R_50splits_default$day1$split50$comp28 %>% head

# ------ normalized ------ # 

# try <- getUnitComp(fmList_R_50splits_default$day1$split8$comp25)

fmList_norm <- list()
for(d in 1:7){
  fmList_norm_perday <- list()
  for(s in 1:50){
    fmList_norm_perday[[s]] <- map(fmList_R_50splits_default[[d]][[s]], getUnitComp)
    
  }
  names(fmList_norm_perday) <- paste0('split', 1:50)
  fmList_norm[[d]] <- fmList_norm_perday
}
names(fmList_norm) <- paste0('day', 1:7)



saveRDS(fmList_norm, file = paste0(dataPath, 'featmat-aki-norm.RData'))







