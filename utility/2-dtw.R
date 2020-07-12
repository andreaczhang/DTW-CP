
dtw_default_run <- function(ref, query){
  tryCatch(
    expr = {
      # this is the expression that might not run
      dtw_default <- dtw(ref, query)
      dist_default <- dtw_default$distance
      dist_default_norm <- dtw_default$normalizedDistance
      distvec <- cbind(dist_default, dist_default_norm)
      
      return(distvec = distvec)
    },
    error = function(e){
      # message('Caught an error!')
      return(1)
    }  
  )    
}



dtw_itakura_run <- function(ref, query){
  tryCatch(
    expr = {
      # this is the expression that might not run
      dtw_itakura <- dtw(ref, query, 
                         window.type = 'itakura')
      dist_itakura <- dtw_itakura$distance
      dist_itakura_norm <- dtw_itakura$normalizedDistance
      distvec <- cbind(dist_itakura, dist_itakura_norm)
      
      return(distvec = distvec)
    },
    error = function(e){
      # message('Caught an error!')
      return(1)
    }  
  )    
}



dtw_sakoechiba_run <- function(ref, query, prop_window){
  tryCatch(
    expr = {
      # this is the expression that might not run
      dtw_sakoechiba <- dtw(ref, query, 
                            step = symmetricP0, 
                            window.type = 'sakoechiba', 
                            window.size = prop_window*(max(length(ref), length(query))))
      dist_sakoechiba <- dtw_sakoechiba$distance
      dist_sakoechiba_norm <- dtw_sakoechiba$normalizedDistance
      distvec <- cbind(dist_sakoechiba, dist_sakoechiba_norm)
      
      return(distvec = distvec)
    },
    error = function(e){
      # message('Caught an error!')
      return(1)
    }  
  )    
}


# ------- these two are for converting error message (1) into NA --------- # 

dtw_itakura_run_wrap <- function(ref, query){
  itakura <- dtw_itakura_run(ref = ref, query = query)
  if(length(itakura) == 1){
    # this is error
    res <- c(NA, NA)
  }else{
    res <- itakura[1, ]
  }
  return(res)
}

dtw_sakoechiba_run_wrap <- function(ref, query, prop_window){
  sakoechiba <- dtw_sakoechiba_run(ref = ref, query = query, prop_window = prop_window)
  if(length(sakoechiba) == 1){
    # this is error
    res <- c(NA, NA)
  }else{
    res <- sakoechiba[1, ]
  }
  return(res)
}




# ============= big dtw function ===========  # 

dtw_wrap <- function(patientList, feature, constraint){
  
  # placeholder
  matDim <- length(patientList)  
  patientID <- names(patientList)
  matSim <- data.frame(matrix(rep(0, matDim * matDim), 
                              ncol = matDim, nrow = matDim, byrow = T), 
                       row.names = patientID)
  matSim_norm <- matSim
  colnames(matSim) <- patientID  
  colnames(matSim_norm) <- patientID  
  
  # time 
  tic()
  
  for (i in 1:matDim){
    # ------- default --------- #
    # (this one should take the longest)
    if(constraint == 'default'){
      matSim[, i] <- map_dbl(patientList, function(x){
        dtw_default_run(ref = patientList[[i]][[feature]], 
                        query = x[[feature]])[ ,1]})  # result is unnormalised
      
      matSim_norm[, i] <- map_dbl(patientList, function(x){
        dtw_default_run(ref = patientList[[i]][[feature]], 
                        query = x[[feature]])[ ,2]})  # result is normalised
      
      # note that only with default, the result is unwrapped: hence [, 1]. 
      # the rest might have error hence produced non-0 exit, therefore
      # I wrote a wrapper.
    }else if(constraint == 'itakura'){
      # ------ itakura ------ # 
      
      
      matSim[, i] <- map_dbl(patientList, function(x){
        dtw_itakura_run_wrap(ref = patientList[[i]][[feature]], 
                             query = x[[feature]])[1]})  
      
      matSim_norm[, i] <- map_dbl(patientList, function(x){
        dtw_itakura_run_wrap(ref = patientList[[i]][[feature]], 
                             query = x[[feature]])[2]})  
      
      
      
    }else if(constraint == 'sakoechiba03'){
      # --------- sakoechiba 0.3 ---------- #
      matSim[, i] <- map_dbl(patientList, function(x){
        dtw_sakoechiba_run_wrap(ref = patientList[[i]][[feature]], 
                                query = x[[feature]], 
                                prop_window = 0.3)[1]}) 
      
      matSim_norm[, i] <- map_dbl(patientList, function(x){
        dtw_sakoechiba_run_wrap(ref = patientList[[i]][[feature]], 
                                query = x[[feature]], 
                                prop_window = 0.3)[2]})  
      
      
    }else if(constraint == 'sakoechiba05'){
      # --------- sakoechiba 0.5 ---------- #
      matSim[, i] <- map_dbl(patientList, function(x){
        dtw_sakoechiba_run_wrap(ref = patientList[[i]][[feature]], 
                                query = x[[feature]], 
                                prop_window = 0.5)[1]})  
      
      matSim_norm[, i] <- map_dbl(patientList, function(x){
        dtw_sakoechiba_run_wrap(ref = patientList[[i]][[feature]], 
                                query = x[[feature]], 
                                prop_window = 0.5)[2]})  
      
    }else{
      stop('Need to set a constraint!')
    }
    # print the details of this operation
    cat(constraint, ' p ', i, ' out of ', matDim, ' complete\n')
  }
  tend <- toc()
  telapsed <- tend$toc - tend$tic
  
  return(list(matSim = matSim, 
              matSim_norm = matSim_norm, 
              telapsed = telapsed))
}




dtw_fillNA <- function(defaultDF, queryDF){
  
  # create placeholder, it is the same as the one with many NA
  placeholderDF <- queryDF
  # get row and col number together 
  id_NA <- which(is.na(placeholderDF), arr.ind = T)
  if(length(id_NA) >0){
    # this means there are NAs in the constrained dtw
    # fill using the default table
    placeholderDF[id_NA] <- defaultDF[id_NA]
    
  }else{
    # there are no NA
    placeholderDF <- placeholderDF
  }
  
  # double check if it's correct 
  deviation_original <- sum(defaultDF - queryDF, na.rm = T)
  deviation_new <- sum(defaultDF - placeholderDF)
  if(deviation_original != deviation_new){
    stop('Deviation unequal, check\n')
  }
  return(placeholderDF)
}









