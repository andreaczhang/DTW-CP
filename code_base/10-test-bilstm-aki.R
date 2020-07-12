# try run the bidirectional lstm 
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


nUnit <- 32
# nUnit <- 64
# nUnit <- 128

p <- 10
t <- 24

m1 <- createmodel_bilstm(nUnit = 32, dataDim = p, timeSteps = t)
m2 <- createmodel_bilstm2(nUnit = 32, dataDim = p, timeSteps = t)
m3 <- createmodel_lstm1(nUnit = 32, dataDim = p, timeSteps = t)
m4 <- createmodel_lstm2(nUnit = 32, dataDim = p, timeSteps = t)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

# try using one dataset's one split, check if the model run 
dataPathAKI <- '~/Documents/Data/Project2/AKI/'

basedata_aki_day1 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day1_50splits.RData'))
basedata_aki_day2 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day2_50splits.RData'))
basedata_aki_day3 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day3_50splits.RData'))
basedata_aki_day4 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day4_50splits.RData'))
basedata_aki_day5 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day5_50splits.RData'))
basedata_aki_day6 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day6_50splits.RData'))
basedata_aki_day7 <- readRDS(file = paste0(dataPathAKI, 'REVISION-AKI/data_baseline_aki/day7_50splits.RData'))

# XTR <- basedata_aki_day1$split1$basedata_tr_xlist$X_ge18
# YTR <- basedata_aki_day1$split1$basedata_tr_ylist$ge18$death
# XTE <- basedata_aki_day1$split1$basedata_te_xlist$X_ge18
# YTE <- basedata_aki_day1$split1$basedata_te_ylist$ge18$death


# in total 8, within each split 
# but there are some that are 0 so we don't need to run them 
map_df(basedata_aki_day1$split1$basedata_tr_xlist, dim)  # stop at 18
map_df(basedata_aki_day2$split1$basedata_tr_xlist, dim)  # stop at 42
map_df(basedata_aki_day3$split1$basedata_tr_xlist, dim)
map_df(basedata_aki_day4$split1$basedata_tr_xlist, dim)
map_df(basedata_aki_day5$split1$basedata_tr_xlist, dim)
map_df(basedata_aki_day6$split1$basedata_tr_xlist, dim)
map_df(basedata_aki_day7$split1$basedata_tr_xlist, dim)




ntime <- dim(XTR)[2]
nfeature <- dim(XTR)[3]
lstmmodel <- createmodel_bilstm(nUnit = nUnit, dataDim = nfeature, timeSteps = ntime)
summary(lstmmodel)

lstmmodel %>% fit(XTR, YTR,
                  batch_size = 32,
                  epochs = 20,
                  validation_split = 0.3,
                  callbacks = list(callback_early_stopping(monitor = 'val_loss',
                                                           patience = 5)))

lstmpredictions <- predLSTMBoot(lstmobj = lstmmodel, Xtest = XTE, Ytest = YTE, bootArg = 100)


gc()







