# this is the file to be automated, to fill the NAs 

dataPath <- 'path/to/data/'
dir_aki <- 'dtw-aki/'
# processed directory 
dirproc_aki <- 'dtw-aki-proc/'

source('utility/2-dtw.R')



runtime_default <- c()
runtime_itakura <- c()


for(i in 1:52){##### this is to restart from where it went wrong
  cat('Loading default data for feature ', i, '\n')
  default <- readRDS(paste0(dataPath, dir_aki, 'aki-default-feat', i, '.RData'))

  # check if the names are consistent

  # processing the not normalised first
  # should also save the akiarated mat for default too
  saveRDS(default$matSim, file = paste0(dataPath, dirproc_aki, 'aki-default-feat', i, '.RData'))
  rm(default)
  # record run time into the vector
  runtime_default[i] <- default$telapsed
  # runtime_itakura[i] <- itakura$telapsed

}
saveRDS(runtime_default, file = paste0(dataPath, dirproc_aki, 'runtime_default.RData'))
saveRDS(runtime_itakura, file = paste0(dataPath, dirproc_aki, 'runtime_itakura.RData'))




# # ======= sc05 ======== #
runtime_sc05 <- c()
runtime_itakura <- c()

for(i in 1:52){
  cat('Loading default data for feature ', i, '\n')
  default <- readRDS(paste0(dataPath, dir_aki, 'aki-default-feat', i, '.RData'))

  cat('Loading sc05 data for feature ', i, '\n')
  sc05 <- readRDS(paste0(dataPath, dir_aki, 'aki-sc05-feat', i, '.RData'))
  
  # check if the names are consistent
  if(all.equal(colnames(default$matSim),
               colnames(sc05$matSim))!= T ){
    stop('Patient names do not match\n')
  }

  # here only need to save the processed, since we already saved the default
  sc05_full <- dtw_fillNA(default= default$matSim, queryDF = sc05$matSim)
  saveRDS(sc05_full, file = paste0(dataPath, dirproc_aki, 'aki-sc05-feat', i, '.RData'))
  runtime_sc05[i] <- sc05$telapsed
  
  # remove everytime (here only sc05)
  rm(sc05_full)
  rm(sc05)
  
  cat('Loading itakura data for feature ', i, '\n')
  itakura <- readRDS(paste0(dataPath, dir_aki, 'aki-itakura-feat', i, '.RData'))
  
  itakura_full <- dtw_fillNA(default= default$matSim, queryDF = itakura$matSim)
  saveRDS(itakura_full, file = paste0(dataPath, dirproc_aki, 'aki-itakura-feat', i, '.RData'))
  cat('Filled itakura for feature ', i, '\n')
  # record run time
  runtime_itakura[i] <- itakura$telapsed
  
  # remove (sc05 and also default)
  rm(itakura_full)
  rm(itakura)
  rm(default)
  gc()
  
}


saveRDS(runtime_itakura, file = paste0(dataPath, dirproc_aki, 'runtime_itakura.RData'))
saveRDS(runtime_sc05, file = paste0(dataPath, dirproc_aki, 'runtime_sc05.RData'))



