
make_array <- function(simMat_list){
  tarray <- array(
    data = unlist(simMat_list), 
    dim = c(dim(simMat_list[[1]]), length(simMat_list))  
  )
  return(tarray = tarray)
}



# convert the feature matrix to R data
featMat_matlabToR <- function(dotmat_object){
  featMats_R <- map(dotmat_object[[1]], function(x){x[[1]]})
  nc <- map_dbl(featMats_R, ncol)
  names(featMats_R) <- paste0('comp', nc)
  return(featMats_R)
}



# == query the pivot column per feature == # 

trteOneCol_eachDay <- function(trIDvec, teIDvec, singleFeatMat, pivotLoc){
  # check if the pivot lcoation is outside range 
  if(pivotLoc > length(trIDvec)){
    stop('Column index exceeding bound, from 1 to the maximum of length(trIDvec)!')
  }
  
  # since we are dealing with the large 
  matchID_trrows <- match(trIDvec, names(singleFeatMat))
  matchID_terows <- match(teIDvec, names(singleFeatMat))
  
  # the pivot is always one of the training patients!
  # ie, for day1 it is between 1 and 158
  # therefore it is advisable to know the limit before specifying 
  TRcol <- singleFeatMat[matchID_trrows, pivotLoc]
  TEcol <- singleFeatMat[matchID_terows, pivotLoc]
  return(list(TRcol = TRcol, 
              TEcol = TEcol))
}



# gather into matrix
gatherColsTRTE <- function(collist){
  trMat <- map(collist, function(x){x$TRcol}) %>% do.call(cbind, .)
  teMat <- map(collist, function(x){x$TEcol}) %>% do.call(cbind, .)
  return(list(trMat = trMat, 
              teMat = teMat))
}


# === put projection, response together for one split ==== # 

getProj_response_onesplit <- function(outlist, pivotMatlist, fmList){
  # response first
  ytr <- outlist$trdf$death
  yte <- outlist$tedf$death
  
  # get projections 
  Xtrlist <- map(fmList, function(x){pivotMatlist$trMat %*% x})
  Xtelist <- map(fmList, function(x){pivotMatlist$teMat %*% x})
  
  return(list(Xtrlist = Xtrlist, 
              Xtelist = Xtelist, 
              ytr = ytr, 
              yte = yte))
}
