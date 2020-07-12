library(magrittr)
library(purrr)
library(R.matlab)
library(tictoc)
library(caret)
library(caretEnsemble)
library(Metrics)
library(pROC)
library(Metrics)  
library(PRROC)  
#library(glmnet)
source('~/Documents/PhdProjects/Project-Paper2/Utilities/helpers-rev-5postTensor.R')
source('~/Documents/PhdProjects/Project-Paper2/Utilities/helpers-rev-6classify.R')

dataPath <- '~/Documents/Data/Project2/Ripoll2014/REVISION-RIPOLL/'
resultPath <- '~/Documents/Data/Project2/AllResults/temp2/'
fmList_norm <- readRDS(file = paste0(dataPath, 'featmat-sep-norm.RData'))

# ======= load featmat ======== # 

featlist <- list()
for(f in 1:52){  
  featlist[[f]] <- readRDS(paste0(dataPath, 'dtw-sep-feats/sepfull-default-feat', f, '.RData'))
  cat('Feat ', f, ' of 52 loaded\n')
}



# ======== load outcomes ===== # 

outlist <- map(1:7, function(x){readRDS(paste0(dataPath, 'alloutcomes_sep_day', x, '.RData'))})

trID_alldays_10splits <- list()
teID_alldays_10splits <- list()
for(d in 1:7){
  trID_alldays_10splits[[d]] <- map(outlist[[d]], function(x){paste0('icustay_', x$trdf$icustay_id)})
  teID_alldays_10splits[[d]] <- map(outlist[[d]], function(x){paste0('icustay_', x$tedf$icustay_id)})
}





set.seed(1)
# use the normalised version 
fm_R <- fmList_norm

# ======= specify which split to use ====== # 
for(d in 1:7){
  for(S in 1:10){
    res_eachSplit <- list()
    
    for(C in 1:29){
      # ------ 1. select pivot ----- # 
      P <- 10 # number of pivots to sample
      n <- length(trID_alldays_10splits[[d]][[S]])
      
      # sample the pivot 
      pivots <- sample(1:n, size = P, replace = F)
      
      slices_pivots <- list()
      for(i in 1:P){
        colList_pvt <- list()
        for(f in 1:52){
          # change the pivot everytime 
          colList_pvt[[f]] <- trteOneCol_eachDay(trIDvec = trID_alldays_10splits[[d]][[S]],
                                                 teIDvec = teID_alldays_10splits[[d]][[S]],
                                                 singleFeatMat = featlist[[f]],
                                                 pivotLoc = pivots[i])
        }
        slices_pivots[[i]] <- gatherColsTRTE(colList_pvt)
        # cat('Pivot extraction of sample', i, 'from day', d, 'split', S, ' done\n')
      }
      
      # ----- 2. do predictions for 10 pivots, and select the best one ---- # 
      
      glm_cv_auc_5piv <- c()
      for(i in 1:P){
        glm_cv_auc_5piv[i] <- baseGLM_TRcv(Xtr = slices_pivots[[i]]$trMat %*% fm_R[[d]][[S]][[C]],
                                           Ytr = outlist[[d]][[S]]$trdf$death,
                                           nfold = 5)
        # cat('pivot', i, 'done\n')
      }
      piv_chosen <- which.max(glm_cv_auc_5piv)
      
      glm_res_test <- baseGLM(Xtr = slices_pivots[[piv_chosen]]$trMat %*% fm_R[[d]][[S]][[C]],
                              Ytr = outlist[[d]][[S]]$trdf$death, 
                              Xte = slices_pivots[[piv_chosen]]$teMat %*% fm_R[[d]][[S]][[C]],
                              Yte = outlist[[d]][[S]]$tedf$death)
      
      cat('cv glm d',d, 's', S, 'C', C, 'done\n' )
      res_eachSplit[[C]] <- glm_res_test
    }# end for all components
    names(res_eachSplit) <- paste0('comp', 2:30)
    
    saveRDS(res_eachSplit, file = paste0(resultPath, 'sep-new-d', d, '-Sp', S, 'default-LR.RData'))
    # res_eachSplit[[C]] <- res_eachComp
  }
}# end for day


