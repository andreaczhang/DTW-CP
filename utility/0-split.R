# ------------- used for sepsis cohort ----------- #
trteIDgenerator_sepsis <- function(outcometable){
  wholeIndex <- seq(1, nrow(outcometable))
  trainIndex <- createDataPartition(outcometable$death, p = .7, list = FALSE)
  trainID <- wholeIndex[ trainIndex]
  testID  <- wholeIndex[-trainIndex]
  return(list(trainID = trainID, 
              testID = testID))
}



trteIDgenerator_aki <- function(outcometable){
  ##### originally randomSetGenerator2
  # get the ID based on outcome in its own training or test set 
  
  indexTotal <- 1:nrow(outcometable) # this is fixed 
  index_dead <- which(outcometable$death ==1)  # 189
  index_alive <- which(outcometable$death ==0) # 463
  # then draw NON inclusive indices
  # to make sure, draw (0.7*500) alive, draw (0.3*500) dead in total
  
  randindexDead_total <- sample(index_dead, size = 150, replace = F)  # 150
  randindexAlive_total <- sample(index_alive, size = 350, replace = F) # 350
  
  # then split: first 0.7 of both sets go to training, 0.3 of both sets go to test
  
  randindexDead_tr <- randindexDead_total[1:105]
  randindexAlive_tr <- randindexAlive_total[1:245]
  
  randindexDead_te <- randindexDead_total[106:150]  # 150
  randindexAlive_te <- randindexAlive_total[246:350] # 355
  
  # put dead and alive together for tr and te set respectively
  randindex_tr <- c(randindexDead_tr, randindexAlive_tr) # %>% sort(decreasing = F)
  randindex_te <- c(randindexDead_te, randindexAlive_te) # %>% sort(decreasing = F)
  
  return(list(randindex_tr = randindex_tr, 
              randindex_te = randindex_te))
}


# ======= match the random sampled id to original icustay id ==== #
matchID_eachsplit <- function(trainID, testID, dinfoDF){
  # the dinfoDF is for each window (day)
  # IDs here are not icustayID! it is the row inside dinfoDF
  trdf <- dinfoDF[trainID, ] %>% select(icustay_id, dischtime_hours, death)
  tedf <- dinfoDF[testID, ] %>% select(icustay_id, dischtime_hours, death)
  return(list(trdf = trdf, 
              tedf = tedf))
}
