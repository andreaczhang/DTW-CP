
seqStdFeatureKeepNA <- function(originalSeq, grpMean, grpSD){
  
  # check how many NA
  # if all NA, return the original seq
  if(sum(is.na(originalSeq)) == length(originalSeq)){
    newSeq <- originalSeq
  }else{
    # remove mean and divide by sd
    newSeq <- (originalSeq - grpMean)/grpSD
  }
  return(newSeq = newSeq)
  
}


# -------------- 3. standardize by patient ----------------- # 

seqStdPatientKeepNA <- function(patientDF, grpMeans, grpSDs){
  
  m <- patientDF[, 3:ncol(patientDF)]
  mLst <- as.list(m) # convert each column to a list
  
  seqs <- list()
  
  for (i in 1:ncol(m)){
    seqs[[i]] <- seqStdFeatureKeepNA(originalSeq = mLst[[i]], 
                                     grpMean = grpMeans[i], 
                                     grpSD = grpMeans[i])
    
  }
  names(seqs) <- colnames(m)
  mdf <- do.call(cbind, seqs)
  processedPatientDF <- data.frame(hr = patientDF$hr, mdf) 
  return(processedPatientDF = processedPatientDF)
  
}




# -------- convert to integers, extreme values fall into -4 and 4
Clipper <- function(x, lower, upper) {
  ifelse(x <= lower,  lower, ifelse(x >= upper, upper, x))
}


discretizeBySeq <- function(sequence){
  # round to nearest integer
  intSeq <- round(sequence, 0)
  # clip into -4 to 4
  intSeqClip <- Clipper(intSeq, lower = -4, upper = 4)
  # convert NA to 9
  intSeqClip[is.na(intSeqClip)] <- 9
  return(intSeqClip)
}


discretize <- function(patientDFstd){
  # do not convert hr
  discretizedDF <- map(patientDFstd[2:ncol(patientDFstd)], 
                       function(x){discretizeBySeq(x)}) %>% data.frame
  
  return(discretizedDF)
}

# ========= generate ID to attach to one hot matrix ========= # 

generateID <- function(patientHours){
  eachP <- list()
  for(i in 1:length(patientHours)){
    eachP[[i]] <- rep(names(patientHours[i]), patientHours[i]) 
  }
  names(eachP) <- names(patientHours)
  
  IDs <- unlist(eachP) 
  names(IDs) <- NULL
  
  return(list(IDs = IDs, eachP = eachP))
}


# make one hot matrix for the patient list, dim should be nT*nP by nf
makeOneHot <- function(patientList){
  
  discretizedIndividuals <- map(patientList, function(i){discretize(i)})
  # 2. merge into a large dataframe, then data table 
  pDF <- rbindlist(discretizedIndividuals) %>% data.frame
  pDFf <- data.frame(apply(pDF, 2, as.character))  # has to be a dataframe 
  dt <- data.table(pDFf)  # transform into datatable 
  
  # 3. get dummy, then drop the NA
  pdummy <- one_hot(dt)
  ind9s <- 1-(grepl(pattern = '_9', x = names(pdummy)))  
  pdummyNoNA <- data.frame(pdummy)[, c(which(ind9s == 1))]
  
  return(pdummyNoNA = pdummyNoNA)
}


# ========== make one hot matrix into list ========= # 
discretePlist <- function(matrixWithID){
  uniqueIDs <- unique(matrixWithID$id)
  
  sepList <- list()
  for(i in 1:length(uniqueIDs)){
    sepList[[i]] <- matrixWithID[which(matrixWithID$id == uniqueIDs[i]), 2:ncol(matrixWithID)]
  }
  names(sepList) <- uniqueIDs
  return(sepList = sepList)
}



# ======== major function: from standardized, individual patient list to individual one hot list ======= # 

onehotPList <- function(processedList){
  # remove the first 24h (before admission)
  postadmin <- map(processedList, function(df){df[25:nrow(df), ]})
  # discretize
  discretizedIndividuals <- map(postadmin, function(i){discretize(i)})
  # get hours
  patientHRs <- map_dbl(discretizedIndividuals, function(x){x %>% nrow}) 
  # merge into a large dataframe, then data table 
  pDF <- rbindlist(discretizedIndividuals) %>% data.frame
  pDFf <- data.frame(apply(pDF, 2, as.character))  # has to be a dataframe 
  dt <- data.table(pDFf)  # transform into datatable 
  
  # get dummy, then drop the NA
  pdummy <- one_hot(dt)
  ind9s <- 1-(grepl(pattern = '_9', x = names(pdummy)))  
  pdummyNoNA <- data.frame(pdummy)[, c(which(ind9s == 1))]
  
  # append the icustayID to the matrix (this is where the previous one hot stops)
  IDday <- generateID(patientHRs)
  pdummyWithID <- data.frame(id = IDday$IDs, pdummyNoNA)
  # separate into each list 
  finalList <- discretePlist(pdummyWithID)
  
  return(list(patientHRs = patientHRs, 
              finalList = finalList))
}




# =========== separate one hot list according to training and test id before ========== # 

##### sepsis cohort 
##### returns labels 
traintestLabel <- function(onehotList, trainID, testID){
  n <- length(onehotList)
  allID <- names(onehotList)
  trainID <- paste0('icustay_', trainID)
  testID <- paste0('icustay_', testID)
  labels <- c()
  for(i in 1:n){
    if(allID[i] %in% trainID){
      labels[i] <- 'Train'
    }else if(allID[i] %in% testID){
      labels[i] <- 'Test'
    }else{
      labels[i] <- NULL
    }
  }
  return(labels = labels)
}

##### similar, for MICU cohort 
##### returns IDs
matchTrTeIDs <- function(wholeListIDs, trteIDs){
  wholeID <- c()
  for(i in 1:length(trteIDs)){
    wholeID[i] <- which(wholeListIDs == trteIDs[i])
  }
  return(wholeID = wholeID)
}



# =========== inclusion table, default is from 4, by 4 ======== # 

generateInclusionTab <- function(onehotList, maxwindow){
  hours <- map_dbl(onehotList, nrow)
  inclusionTab <- data.frame(hr = hours, 
                             icustay_id = as.numeric(substring(names(hours), 9)))
  
  # greater or equal to hours
  # I think now it's better to use 6
  for(i in seq(6, maxwindow, by=6)){
    ge <- paste0('ge', i)
    inclusionTab[[ge]] <- ifelse(inclusionTab$hr>= i, 1, 0)
    
  }  
  return(inclusionTab = inclusionTab)
}



# ======== combine inclusion table with outcomes ======== # 

mergeOutcomes <- function(inclusiontable, outcometable){
  
  # generate order index 
  inclusiontable$order <- 1:nrow(inclusiontable)
  mergedTab <- merge(x = outcometable, y = inclusiontable, by = 'icustay_id')
  # at this stage the order is linked with outcome, but we need it to match inclusion table 
  
  mergedTab2 <- mergedTab[order(mergedTab$order), ]
  return(mergedTab2 = mergedTab2)
}


# ========= extract time series (one hot list) and outcome based on inclusion ======= # 

matchList <- function(onehotList, mergedoutTab){
  # check if the orders match 
  if(all.equal(paste0('icustay_', mergedoutTab$icustay_id), 
               names(onehotList)) == F){
    cat('wrong order, match data before proceeding\n')
  }else{
    # extract all ge columns
    genames <- names(mergedoutTab)[grepl(pattern = 'ge', x = colnames(mergedoutTab))]
    listTab <- list()
    listonehotList <- list()
    
    for(i in 1:length(genames)){
      listTab[[i]] <- mergedoutTab[which(mergedoutTab[[genames[i]]] == 1), ]
      listonehotList[[i]] <- onehotList[which(mergedoutTab[[genames[i]]] == 1)]
    }
  }
  names(listTab) <- genames
  names(listonehotList) <- genames
  return(list(listTab = listTab, 
              listonehotList = listonehotList))
  
}



longMat <- function(singleOnehotList, window){
  # take necessary window
  matList <- map(singleOnehotList, function(x){x[1:window, ]})
  longMat <- rbindlist(matList) %>% data.frame
  return(longMat = longMat)
}




# ======= put into tensor ====== #
intoArray <- function(processedMat, nPatient, nTime, nFeat){
  if(nPatient * nTime * nFeat != prod(dim(processedMat))){
    stop('Wrong dimension, check and try again')
  }
  # make into 3d array: first nTime rows correspond to one patient 
  mArray <- array(t(processedMat), dim = c(nFeat, nTime, nPatient))
  # change coordinate: patient by time by features 
  mArray2 <- aperm(mArray, perm = c(3, 2, 1)) 
  return(resArray = mArray2)
}



arrayFromList <- function(singleOnehotList, window){
  # take appropriate hours, make into long matrix
  longmat <- longMat(singleOnehotList = singleOnehotList, window = window)
  # put back into array 
  parray <- intoArray(processedMat = longmat, 
                      nPatient = length(singleOnehotList), 
                      nTime = window, 
                      nFeat = ncol(longmat))
  return(parray = parray)
}

XList <- function(tsList){
  
  windows <- as.numeric(substring(names(tsList), 3))
  Xlist <- list()
  for(i in 1:length(windows)){
    Xlist[[i]] <- arrayFromList(tsList[[i]], window = windows[i])
  }
  names(Xlist) <- paste0('X_ge', windows)
  return(Xlist = Xlist)
}







baselineDataPrepare <- function(outcome_tr, outcome_te, wholedata, maxwindow){
  # 1. match the subject id 
  pID <- as.numeric(substring(names(wholedata), 9))
  
  index_tr <- matchTrTeIDs(wholeListIDs = pID, trteIDs = outcome_tr$icustay_id)
  index_te <- matchTrTeIDs(wholeListIDs = pID, trteIDs = outcome_te$icustay_id)
  
  # 2. take the corresponding ts 
  rawdata_tr <- wholedata[index_tr]
  rawdata_te <- wholedata[index_te]
  
  # 3. combine together, standardize and make onehot 
  rawdata <- c(rawdata_tr, rawdata_te)
  
  groupMeans <- apply(rbindlist(rawdata)[, 3:54], 2, function(x){mean(x, na.rm = T)})
  groupSds <- apply(rbindlist(rawdata)[, 3:54], 2, function(x){sd(x, na.rm = T)})
  
  data_processed <- map(rawdata, function(x){seqStdPatientKeepNA(patientDF = x, 
                                                                 grpMeans = groupMeans, 
                                                                 grpSDs = groupSds)})
  data_onehot <- onehotPList(processedList = data_processed)
  
  # 4. separate train and test 
  data_onehot_tr <- data_onehot$finalList[1:length(rawdata_tr)]
  data_onehot_te <- data_onehot$finalList[(length(rawdata_tr)+1):(length(rawdata_tr)+length(rawdata_te))]
  
  # 5.1. match the outcomes: train
  inclusiontable_tr <- generateInclusionTab(data_onehot_tr, maxwindow = maxwindow)
  # conbine with outcomes 
  inclusiontableOUT_tr <- mergeOutcomes(inclusiontable_tr, outcome_tr)
  
  # match the time series list with outcomes 
  dataall_tr <- matchList(onehotList = data_onehot_tr, mergedoutTab = inclusiontableOUT_tr)
  
  # 5.2. match the outcomes:test data
  inclusiontable_te <- generateInclusionTab(data_onehot_te, maxwindow = maxwindow)
  inclusiontableOUT_te <- mergeOutcomes(inclusiontable_te, outcome_te)
  dataall_te <- matchList(onehotList = data_onehot_te, mergedoutTab = inclusiontableOUT_te)
  
  
  
  # 6. segment into separate lists 
  basedata_tr_xlist <- XList(tsList = dataall_tr$listonehotList)
  # training list, Y
  basedata_tr_ylist <- dataall_tr$listTab  
  # test list, X
  basedata_te_xlist <- XList(tsList = dataall_te$listonehotList)
  # test list, Y
  basedata_te_ylist <- dataall_te$listTab
  
  return(list(basedata_tr_xlist = basedata_tr_xlist, 
              basedata_tr_ylist = basedata_tr_ylist, 
              basedata_te_xlist = basedata_te_xlist, 
              basedata_te_ylist = basedata_te_ylist))
}





baselineDataCreator24 <- function(outcomefile_tr, outcomefile_te, wholelist){
  
  ytr_split <- read.csv(file = outcomefile_tr)
  yte_split <- read.csv(file = outcomefile_te)
  # query from corresponding 
  basedata <- baselineDataPrepare(outcome_tr = ytr_split,
                                  outcome_te = yte_split, 
                                  wholedata = wholelist, 
                                  maxwindow = 24)
  return(basedata = basedata)
}





