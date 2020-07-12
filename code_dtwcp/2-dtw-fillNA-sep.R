# this is the file to be automated, to fill the NAs 

dataPath <- 'path/to/data/'
dir_sep <- 'dtw-sep/'
# processed directory 
dirproc_sep <- 'dtw-sep-proc/'

source('utility/2-dtw.R')


runtime_default <- c()
runtime_itakura <- c()

for(i in 1:52){##### this is to restart from where it went wrong
  cat('Loading default data for feature ', i, '\n')
  default <- readRDS(paste0(dataPath, dir_sep, 'sep-default-feat', i, '.RData'))
  
  # check if the names are consistent
  
  # processing the not normalised first
  # should also save the separated mat for default too
  saveRDS(default$matSim, file = paste0(dataPath, dirproc_sep, 'sep-default-feat', i, '.RData'))
  rm(default)
  # record run time into the vector
  runtime_default[i] <- default$telapsed
  # runtime_itakura[i] <- itakura$telapsed
  
}
saveRDS(runtime_default, file = paste0(dataPath, dirproc_sep, 'runtime_default.RData'))
saveRDS(runtime_itakura, file = paste0(dataPath, dirproc_sep, 'runtime_itakura.RData'))








# ============ sc03 ========= # 
runtime_sc03 <- c()


for(i in 1:52){
  cat('Loading default data for feature ', i, '\n')
  default <- readRDS(paste0(dataPath, dir_sep, 'sep-default-feat', i, '.RData'))

  cat('Loading sc03 data for feature ', i, '\n')
  sc03 <- readRDS(paste0(dataPath, dir_sep, 'sep-sc03-feat', i, '.RData'))
  # check if the names are consistent
  if(all.equal(colnames(default$matSim),
               colnames(sc03$matSim))!= T ){
    stop('Patient names do not match\n')
  }

  # here only need to save the processed, since we already saved the default
  sc03_full <- dtw_fillNA(default= default$matSim, queryDF = sc03$matSim)
  saveRDS(sc03_full, file = paste0(dataPath, dirproc_sep, 'sepfull-sc03-feat', i, '.RData'))
  cat('Filled sc03 (normalised) for feature ', i, '\n')

  sc03_full_norm <- dtw_fillNA(defaultDF = default$matSim_norm, queryDF = sc03$matSim_norm)
  saveRDS(sc03_full_norm, file = paste0(dataPath, dirproc_sep, 'sepfull-norm-sc03-feat', i, '.RData'))
  cat('Filled sc03 (unnormalised) for feature ', i, '\n')

  # record run time
  runtime_sc03[i] <- sc03$telapsed

}

saveRDS(runtime_sc03, file = paste0(dataPath, dirproc_sep, 'runtime_sc03.RData'))




# ======= sc05 ======== #
runtime_sc05 <- c()
for(i in 1:52){
  cat('Loading default data for feature ', i, '\n')
  default <- readRDS(paste0(dataPath, dir_sep, 'sep-default-feat', i, '.RData'))

  cat('Loading sc05 data for feature ', i, '\n')
  sc05 <- readRDS(paste0(dataPath, dir_sep, 'sep-sc05-feat', i, '.RData'))
  # check if the names are consistent
  if(all.equal(colnames(default$matSim),
               colnames(sc05$matSim))!= T ){
    stop('Patient names do not match\n')
  }

  # here only need to save the processed, since we already saved the default
  sc05_full <- dtw_fillNA(default= default$matSim, queryDF = sc05$matSim)
  saveRDS(sc05_full, file = paste0(dataPath, dirproc_sep, 'sepfull-sc05-feat', i, '.RData'))
  cat('Filled sc05 (normalised) for feature ', i, '\n')

  sc05_full_norm <- dtw_fillNA(defaultDF = default$matSim_norm, queryDF = sc05$matSim_norm)
  saveRDS(sc05_full_norm, file = paste0(dataPath, dirproc_sep, 'sepfull-norm-sc05-feat', i, '.RData'))
  cat('Filled sc05 (unnormalised) for feature ', i, '\n')

  # record run time
  runtime_sc05[i] <- sc05$telapsed

}

saveRDS(runtime_sc05, file = paste0(dataPath, dirproc_sep, 'runtime_sc05.RData'))



