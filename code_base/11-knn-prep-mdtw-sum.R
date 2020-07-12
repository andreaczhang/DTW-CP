# prepare data for KNN classifier: multivariate, independent DTW
# for simplicity, add each layer together 
library(magrittr)
library(purrr)

dataPath <- 'data/path'

# ====== start with sep ====== # 

f_sep_each <- list()
for(i in 1:52){
  f_sep_each[[i]] <- readRDS(paste0(dataPath, 'dtw-sep-feats/sepfull-default-feat', i, '.RData'))
  cat('feautre', i, 'loaded\n')
}


# produce sums
fsum_sep <- f_sep_each[[1]]
for(i in 1:51){
  fsum_sep <- fsum_sep + f_sep_each[[i+1]]
  cat('feature', i, 'summed\n')
}

# fsum_sep[1:6, 1:6]
saveRDS(fsum_sep, file = paste0(dataPath, 'dtw-sep-feats/mdtw-sep.RData'))


# ======= AKI ======== # 


f_aki_each <- list()
for(i in 1:52){
  f_aki_each[[i]] <- readRDS(paste0(dataPath, 'AKI/REVISION-AKI/dtw-aki-proc/aki-default-feat', i, '.RData'))
  cat('feautre', i, 'loaded\n')
}


# produce sums
fsum_aki <- f_aki_each[[1]]
for(i in 1:51){
  fsum_aki <- fsum_aki + f_aki_each[[i+1]]
  cat('feature', i, 'summed\n')
}
# fsum_aki[1:6, 1:6]
# fsum_aki %>% dim
saveRDS(fsum_aki, file = paste0(dataPath, 'dtw-aki-proc/mdtw-aki.RData'))








