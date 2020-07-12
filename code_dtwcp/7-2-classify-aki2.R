library(magrittr)
library(purrr)
library(tictoc)
library(caret)
library(kernlab)
library(randomForest)
library(Metrics)
library(pROC)
library(Metrics)  
library(PRROC)  

source('~/Documents/Work/Paper2/Utilities/helpers-rev-5postTensor.R')
source('~/Documents/Work/Paper2/Utilities/helpers-rev-6classify.R')

dataPath <- '~/Documents/Data_backup/REVProject2/'
resultPath <- '~/Documents/Data_backup/REVProject2/'
fmList_norm <- readRDS(file = paste0(dataPath, 'splitdata-aki-rev/featmat-aki-norm.RData'))



split <- 1  #### this changes 

# ======= load featmat ======== # 

featlist <- list()
for(f in 1:52){  
  featlist[[f]] <- readRDS(paste0(dataPath, 'dtw-aki-proc/aki-default-feat', f, '.RData'))
  cat('Feat ', f, ' of 52 loaded\n')
}



# ======== load outcomes ===== # 

outlist <- map(1:7, function(x){readRDS(paste0(dataPath, 'splitdata-aki-rev/alloutcomes_aki_day', x, '.RData'))})

trID_alldays_50splits <- list()
teID_alldays_50splits <- list()
for(d in 1:7){
  trID_alldays_50splits[[d]] <- map(outlist[[d]], function(x){paste0('icustay_', x$trdf$icustay_id)})
  teID_alldays_50splits[[d]] <- map(outlist[[d]], function(x){paste0('icustay_', x$tedf$icustay_id)})
}





set.seed(1)
# use the normalised version 
fm_R <- fmList_norm


# ======= specify which split to use ====== #

S <- split  # this parameter is set at the beginning 


for(d in 1:7){
  for(C in 1:29){
    
    # ------ 1. select pivot ----- # 
    P <- 10 # number of pivots to sample
    n <- length(trID_alldays_50splits[[d]][[S]])
    
    # sample the pivot 
    pivots <- sample(1:n, size = P, replace = F)
    
    slices_pivots <- list()
    
    for(i in 1:P){
      colList_pvt <- list()
      for(f in 1:52){
        # change the pivot everytime 
        colList_pvt[[f]] <- trteOneCol_eachDay(trIDvec = trID_alldays_50splits[[d]][[S]],
                                               teIDvec = teID_alldays_50splits[[d]][[S]],
                                               singleFeatMat = featlist[[f]],
                                               pivotLoc = pivots[i])
      }
      slices_pivots[[i]] <- gatherColsTRTE(colList_pvt)
      cat('Pivot extraction of sample', i, 'from day', d, 'split', S, ' done\n')
    }
    
    
    # ========= SVM linear ========= # 
    svmL_res <- list()
    svmL_auc_vec <- c()
    for(i in 1:P){
      svmL_auc_vec[i] <- trainPred_svmL_onepivot(Xtr = slices_pivots[[i]]$trMat %*% fm_R[[d]][[S]][[C]],
                                                 Ytr = outlist[[d]][[S]]$trdf$death)
      cat('svmL for pivot', i, 'done\n')
    }
    piv_chosen <- which.max(svmL_auc_vec)
    svmL_res_best <- trainPred_svmL(Xtr = slices_pivots[[piv_chosen]]$trMat %*% fm_R[[d]][[S]][[C]],
                                    Ytr = outlist[[d]][[S]]$trdf$death,
                                    Xte = slices_pivots[[piv_chosen]]$teMat %*% fm_R[[d]][[S]][[C]],
                                    Yte = outlist[[d]][[S]]$tedf$death)

    saveRDS(svmL_res_best, file = paste0(resultPath, 'aki-new-d', d, '-Sp', S, '-comp', C, 'default-svmL.RData'))


    
    # ========= svm R ========= # 
    
    svmR_auc_vec <- c()
    for(i in 1:P){
      svmR_auc_vec[i] <- trainPred_svmR_onepivot(Xtr = slices_pivots[[i]]$trMat %*% fm_R[[d]][[S]][[C]], 
                                                 Ytr = outlist[[d]][[S]]$trdf$death)
      cat('svmR for pivot', i, 'done\n')
      
    }
    piv_chosen <- which.max(svmR_auc_vec)
    svmR_res_best <- trainPred_svmR(Xtr = slices_pivots[[piv_chosen]]$trMat %*% fm_R[[d]][[S]][[C]],
                                    Ytr = outlist[[d]][[S]]$trdf$death, 
                                    Xte = slices_pivots[[piv_chosen]]$teMat %*% fm_R[[d]][[S]][[C]],
                                    Yte = outlist[[d]][[S]]$tedf$death)
    
    saveRDS(svmR_res_best, file = paste0(resultPath, 'aki-new-d', d, '-Sp', S, '-comp', C, 'default-svmR.RData'))
    
    
    gc()
    
  }
}


