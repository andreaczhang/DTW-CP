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
library(glmnet)

source('~/Documents/PhdProjects/Project-Paper2/Utilities/helpers-rev-5postTensor.R')
source('~/Documents/PhdProjects/Project-Paper2/Utilities/helpers-rev-6classify.R')
# source('~/Documents/PhdProjects/Project-Paper2/Utilities/helpers-rev-8interpret.R')


dataPath <- '~/Documents/Data/Project2/Ripoll2014/REVISION-RIPOLL/'
simMatPath_sep <- 'dtw-sep-feats/'

# load the features 
dtwsimmat_name <- 'sepfull-default-feat'
featlist <- list()
for(f in 1:52){  
  featlist[[f]] <- readRDS(paste0(dataPath, simMatPath_sep, dtwsimmat_name, f, '.RData'))
  cat('Feat ', f, ' of ', dtwsimmat_name, ' loaded\n')
}




# ======== load outcomes ===== # 

outlist <- map(1:7, function(x){readRDS(paste0(dataPath, 'alloutcomes_sep_day', x, '.RData'))})

trID_alldays_10splits <- list()
teID_alldays_10splits <- list()
for(d in 1:7){
  trID_alldays_10splits[[d]] <- map(outlist[[d]], function(x){paste0('icustay_', x$trdf$icustay_id)})
  teID_alldays_10splits[[d]] <- map(outlist[[d]], function(x){paste0('icustay_', x$tedf$icustay_id)})
}




# ======= specify which split to use ====== # 



S <- 1

featmat_alldays <- map(1:7, function(x){
  readMat(paste0(dataPath, 'featmats-default-sep/day', x, 'sep_featmats_default_split', S,'.mat'))
})

cat('featmat loaded\n')

fm_R <- map(featmat_alldays, featMat_matlabToR)
C <- 29


# ======== start running, see how it goes ====== #
# res_alldays_thissplit <- list()

for(d in 3:7){
  
  # ------ 1. select pivot ----- # 
  n <- length(trID_alldays_10splits[[d]][[S]])
  pivots <- seq(1, n, by = 1)
  
  slices_pivots <- list()
  for(i in 1:n){
    colList_pvt <- list()
    for(f in 1:52){
      # change the pivot everytime 
      colList_pvt[[f]] <- trteOneCol_eachDay(trIDvec = trID_alldays_10splits[[d]][[S]],
                                             teIDvec = teID_alldays_10splits[[d]][[S]],
                                             singleFeatMat = featlist[[f]],
                                             pivotLoc = pivots[i])
    }
    slices_pivots[[i]] <- gatherColsTRTE(colList_pvt)
    cat('Pivot extraction', i, 'day', d, 'split', S, ' done\n')
  }
  
  
  # ----- 2. do predictions ---- # 
  # need to specify what is ytr and yte
  # and compMat
  project_res <- list()
  project_res_glmnet <- list()
  noproject_res <- list()
  
  for(i in 1:n){
    project_res[[i]] <- runLearners_percomp(Xtr = slices_pivots[[i]]$trMat %*% fm_R[[d]][[C]],
                                            Xte = slices_pivots[[i]]$teMat %*% fm_R[[d]][[C]], 
                                            Ytr = outlist[[d]][[S]]$trdf$death, 
                                            Yte = outlist[[d]][[S]]$tedf$death, 
                                            bootarg = F)
    
    project_res_glmnet[[i]] <- predictLearner_Lasso(xtr = slices_pivots[[i]]$trMat %*% fm_R[[d]][[C]], 
                                                    xte = slices_pivots[[i]]$teMat %*% fm_R[[d]][[C]], 
                                                    ytr = outlist[[d]][[S]]$trdf$death, 
                                                    yte = outlist[[d]][[S]]$tedf$death)
    # for curiosity, test without projection 
    noproject_res[[i]] <- predictLearner_Lasso(xtr = slices_pivots[[i]]$trMat, 
                                               xte = slices_pivots[[i]]$teMat, 
                                               ytr = outlist[[d]][[S]]$trdf$death, 
                                               yte = outlist[[d]][[S]]$tedf$death)
    
    cat('pivot prediction', i, ' day ', d, 'split', S, 'done\n')
  }
  
  
  
  # projectLR_coefs <- map_df(projectLR_res, pluck(1))
  glm_auc <- map_dbl(project_res, function(x){x$glm$auc})
  glm_auprc <- map_dbl(project_res, function(x){x$glm$auprc})
  glm_accu <- map_dbl(project_res, function(x){x$glm$accu})
  svm_auc <- map_dbl(project_res, function(x){x$svm$auc})
  svm_auprc <- map_dbl(project_res, function(x){x$svm$auprc})
  svm_accu <- map_dbl(project_res, function(x){x$svm$accu})
  
  glmnet_auc <- map_dbl(project_res_glmnet, function(x){x$auc})
  glmnet_auprc <- map_dbl(project_res_glmnet, function(x){x$auprc})
  glmnet_accu <- map_dbl(project_res_glmnet, function(x){x$accu})
  
  noprojglmnet_auc <- map_dbl(noproject_res, function(x){x$auc})
  noprojglmnet_auprc <- map_dbl(noproject_res, function(x){x$auprc})
  noprojglmnet_accu <- map_dbl(noproject_res, function(x){x$accu})
  
  res_alldays_thissplit <- data.frame(glm_auc, 
                                      glm_auprc, 
                                      glm_accu,
                                      glmnet_auc, 
                                      glmnet_auprc, 
                                      glmnet_accu,
                                      noprojglmnet_auc, 
                                      noprojglmnet_auprc, 
                                      noprojglmnet_accu,
                                      svm_auc, 
                                      svm_auprc, 
                                      svm_accu)
  cat('pivot result for day', d, 'split', S, 'done\n')
  saveRDS(res_alldays_thissplit, file = paste0('~/Documents/Data/Project2/AllResults/Ours/sep_allpiv_comp30_d',
                                               d, '_s', S, '.Rdata'))
}# end of one split







