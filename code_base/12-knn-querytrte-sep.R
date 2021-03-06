# query training and test data for sep data 
library(magrittr)
library(purrr)
source('./utility/b1-knn.R')

dataPath <- '~/Documents/Data/Project2/'


fsum_sep <- readRDS(paste0(dataPath, 'dtw-sep-feats/mdtw-sep.RData'))
out_sep_d1 <-  readRDS(paste0(dataPath, 'alloutcomes_sep_day1.RData'))
out_sep_d2 <-  readRDS(paste0(dataPath, 'alloutcomes_sep_day2.RData'))
out_sep_d3 <-  readRDS(paste0(dataPath, 'alloutcomes_sep_day3.RData'))
out_sep_d4 <-  readRDS(paste0(dataPath, 'alloutcomes_sep_day4.RData'))
out_sep_d5 <-  readRDS(paste0(dataPath, 'alloutcomes_sep_day5.RData'))
out_sep_d6 <-  readRDS(paste0(dataPath, 'alloutcomes_sep_day6.RData'))
out_sep_d7 <-  readRDS(paste0(dataPath, 'alloutcomes_sep_day7.RData'))


trID_day1 <- map(out_sep_d1, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day2 <- map(out_sep_d2, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day3 <- map(out_sep_d3, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day4 <- map(out_sep_d4, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day5 <- map(out_sep_d5, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day6 <- map(out_sep_d6, function(x){paste0('icustay_', x$trdf$icustay_id)})
trID_day7 <- map(out_sep_d7, function(x){paste0('icustay_', x$trdf$icustay_id)})

teID_day1 <- map(out_sep_d1, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day2 <- map(out_sep_d2, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day3 <- map(out_sep_d3, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day4 <- map(out_sep_d4, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day5 <- map(out_sep_d5, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day6 <- map(out_sep_d6, function(x){paste0('icustay_', x$tedf$icustay_id)})
teID_day7 <- map(out_sep_d7, function(x){paste0('icustay_', x$tedf$icustay_id)})






# ===== query for the whole list ==== # 
distvec_sep_d1 <- list()
distvec_sep_d2 <- list()
distvec_sep_d3 <- list()
distvec_sep_d4 <- list()
distvec_sep_d5 <- list()
distvec_sep_d6 <- list()
distvec_sep_d7 <- list()


for(s in 1:10){
  distvec_sep_d1[[s]] <- query_distvec_alltest(dtwmat = fsum_sep, trIDvec = trID_day1[[s]], teID = teID_day1[[s]])
  distvec_sep_d2[[s]] <- query_distvec_alltest(dtwmat = fsum_sep, trIDvec = trID_day2[[s]], teID = teID_day2[[s]])
  distvec_sep_d3[[s]] <- query_distvec_alltest(dtwmat = fsum_sep, trIDvec = trID_day3[[s]], teID = teID_day3[[s]])
  distvec_sep_d4[[s]] <- query_distvec_alltest(dtwmat = fsum_sep, trIDvec = trID_day4[[s]], teID = teID_day4[[s]])
  distvec_sep_d5[[s]] <- query_distvec_alltest(dtwmat = fsum_sep, trIDvec = trID_day5[[s]], teID = teID_day5[[s]])
  distvec_sep_d6[[s]] <- query_distvec_alltest(dtwmat = fsum_sep, trIDvec = trID_day6[[s]], teID = teID_day6[[s]])
  distvec_sep_d7[[s]] <- query_distvec_alltest(dtwmat = fsum_sep, trIDvec = trID_day7[[s]], teID = teID_day7[[s]])
  cat('split', s, 'done\n')
}

names(distvec_sep_d1) <- paste0('split', 1:10)
names(distvec_sep_d2) <- paste0('split', 1:10)
names(distvec_sep_d3) <- paste0('split', 1:10)
names(distvec_sep_d4) <- paste0('split', 1:10)
names(distvec_sep_d5) <- paste0('split', 1:10)
names(distvec_sep_d6) <- paste0('split', 1:10)
names(distvec_sep_d7) <- paste0('split', 1:10)

# put together 
disvec_sep <- list(distvec_sep_d1 = distvec_sep_d1, 
                   distvec_sep_d2 = distvec_sep_d2, 
                   distvec_sep_d3 = distvec_sep_d3, 
                   distvec_sep_d4 = distvec_sep_d4, 
                   distvec_sep_d5 = distvec_sep_d5, 
                   distvec_sep_d6 = distvec_sep_d6, 
                   distvec_sep_d7 = distvec_sep_d7)
saveRDS(disvec_sep, file = paste0(dataPath, 'knn_disvecs_sep.RData'))






