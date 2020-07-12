library(magrittr)
library(purrr)
library(data.table)
library(dplyr)
library(mltools)
library(tictoc)
library(caret)
library(keras)
library(pROC)
library(Metrics)  
library(PRROC)    

# ------ model and data specification ------ # 
UNIT <- 64
# 128

MODEL <- 'lstm1'
# lstm2, bilstm1, bilstm2

day <- 1

# --------------------------------- # 



source('./utility/b3-lstm.R')
dataPathAKI <- 'data/path'
savePathAKI <- 'result/path'



basedata_aki <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day', day, '_50splits.RData'))


# need to limit the maximum number of ge.x

if(day == 1){
  ge_list <- paste0('ge', c(12, 18))
}else if(day == 2){
  ge_list <- paste0('ge', c(12, 18, 24))
}else{
  ge_list <- paste0('ge', c(12, 18, 24))
}






set.seed(1)
tic()

for(s in 1:50){
  xtr_list <- basedata_aki[[s]]$basedata_tr_xlist
  ytr_list <- basedata_aki[[s]]$basedata_tr_ylist
  xte_list <- basedata_aki[[s]]$basedata_te_xlist
  yte_list <- basedata_aki[[s]]$basedata_te_ylist
  
  
  for(g in 1:length(ge_list)){
    model_result <- runLSTM_model(XTR = xtr_list[[paste0('X_', ge_list[g])]], 
                                  YTR = ytr_list[[ge_list[g]]][['death']], 
                                  XTE = xte_list[[paste0('X_', ge_list[g])]], 
                                  YTE = yte_list[[ge_list[g]]][['death']], 
                                  nUnit = UNIT, 
                                  modelName = MODEL)
    
    saveRDS(model_result, file = paste0(savePathAKI, 
                                        'd', day, 
                                        '_sp_', s, 
                                        '_', ge_list[g], 
                                        '_unit', UNIT, 
                                        '_', MODEL, '.RData'))
    cat('Model result for AKI data', 'd',day, 'sp', s, ge_list[g], 'unit', UNIT, MODEL, ' done\n')
  }
  gc()
}

# time difference 
t <- toc()
dt <- t$toc - t$tic
saveRDS(dt, file = paste0(savePathAKI, 'telps_day', day, 'unit', UNIT, '_', MODEL, '.RData'))






