# this is to provide the overall info for server version 
# ----- load packages and helpers ------ # 

library(tictoc)
library(magrittr)
library(purrr)
library(proxy)
library(dtw)


source('./utility/2-dtw.R')

# ----- load data ------ # 
dataPath <- 'path/to/data/'

#### iterate over all 52 features 
#### here is using heartrate as an example 
FEAT <- 'heartrate'

# ===== first AKI ====== # 
d_aki <- readRDS(file = paste0(dataPath, 'AKI/processed7day_aki.RData'))


dtwdist_aki_default <- dtw_wrap(patientList = d_aki, feature = FEAT, constraint = 'default')
dtwdist_aki_itakura <- dtw_wrap(patientList = d_aki, feature = FEAT, constraint = 'itakura')
dtwdist_aki_sakoechiba05 <- dtw_wrap(patientList = d_aki, feature = FEAT, constraint = 'sakoechiba05')



# ===== then sepsis ===== #
d_sepsis <- readRDS(file = paste0(dataPath, 'Ripoll2014/processed2weeks_ripoll.RData'))


dtwdist_sep_default <- dtw_wrap(patientList = d_sepsis, feature = FEAT, constraint = 'default')
dtwdist_sep_itakura <- dtw_wrap(patientList = d_sepsis, feature = FEAT, constraint = 'itakura')
dtwdist_sep_sakoechiba05 <- dtw_wrap(patientList = d_sepsis, feature = FEAT, constraint = 'sakoechiba05')





