seqStdFeature <- function(originalSeq, grpMean, grpSD){
  
  # check how many NA
  # if all NA, replace by 0, a singleton
  if(sum(is.na(originalSeq)) == length(originalSeq)){
    newSeq <- 0
  }else{
    # remove NA
    seqnoNA <- originalSeq[is.na(originalSeq) != 1]
    # remove mean and divide by sd
    newSeq <- (seqnoNA - grpMean)/grpSD
  }
  
  return(newSeq = newSeq)
  
}


# for each patient, all features
seqStdPatient <- function(patientDF, grpMeans, grpSDs){
  
  m <- patientDF[, 3:ncol(patientDF)]
  mLst <- as.list(m) # convert each column to a list
  
  seqs <- list()
  
  for (i in 1:ncol(m)){
    seqs[[i]] <- seqStdFeature(originalSeq = mLst[[i]], 
                               grpMean = grpMeans[i], 
                               grpSD = grpMeans[i])
    
  }
  names(seqs) <- colnames(m)
  return(seqs = seqs)
  
}

