# ====== find the relevant columns and rows from big dtw matrix ===== # 

query_distvec_each <- function(dtwmat, trIDvec, teID){
  # one by one, easier to debug
  # find the row id for this subject
  whichrow_te <- which(rownames(dtwmat) == teID) 
  # take all columns (that belong to his training conterpart)
  distvec <- t(dtwmat[trIDvec][whichrow_te, ])
  return(distvec)
}


query_distvec_alltest <- function(dtwmat, trIDvec, teIDvec){
  distlist <- list()
  for(i in 1:length(teIDvec)){
    distlist[[i]] <- query_distvec_each(dtwmat = dtwmat, 
                                        trIDvec = trIDvec, 
                                        teID = teIDvec[i])
  }
  names(distlist) <- teIDvec
  return(distlist)
}



knn_checkID <- function(trdf, tedf, distvec_list){
  # 1. if the names of dist is the same as yte 
  bin_cond1 <- all.equal(names(distvec_list), paste0('icustay_', tedf$icustay_id))
  
  # 2. if the rownames in each test list is the same as training 
  bin_matchTR <- map_lgl(distvec_list, 
                         function(x){all.equal(rownames(x), paste0('icustay_', trdf$icustay_id))})
  bin_cond2 <- all(bin_matchTR == T)
  
  if(any(c(bin_cond1, bin_cond2) == F)){
    stop('names do not match, check\n')
  }else{
    return(T)
  }
}


# ============ KNN ============ # 


knn_onecase_breaktie <- function(distvec, label, k){
  compareDF <- data.frame(d = distvec, y = label)
  # sort from small to big distance
  compareDF_sorted <- compareDF[order(compareDF$d), ]
  # get k top 
  top_k_cl <- compareDF_sorted[1:k, ]$y
  
  # get prob 
  classes <- unique(label)
  probs <- map_dbl(1:length(classes), function(x){
    length(which(top_k_cl == classes[[x]]))/k
  })
  
  
  # break tie: in our use case there is only two class
  if(sum(probs == max(probs)) == 1){
    # condition 1: prob_win is maximum and only one: no tie 
    class_win <- classes[which.max(probs)]
    prob_win <- probs[which.max(probs)]
  }else{
    # condition 2: prob_win is maximum, and exists at least one other class that ties 
    # plus 1, this breaks the tie
    top_kplus1_cl <- compareDF_sorted[1:(k+1), ]$y
    probs_kplus1 <- map_dbl(1:length(classes), function(x){
      length(which(top_kplus1_cl == classes[[x]]))/(k+1)
    })
    class_win <- classes[which.max(probs_kplus1)]
    prob_win <- probs_kplus1[which.max(probs_kplus1)]
  }
  
  
  return(list(class_win = class_win, 
              prob_win = prob_win))
}



knn_everytest <- function(distvecList, trainlabel, k){
  res <- list()
  for(i in 1:length(distvecList)){
    res[[i]] <- knn_onecase_breaktie(distvec = distvecList[[i]], 
                                     label = trainlabel, k = k)
  }
  # arrange the result a bit 
  classes <- map(res, pluck(1))  %>% do.call(c, .)
  probs <- map_dbl(res, pluck(2)) 
  return(list(classes = classes, 
              probs = probs))
}





knn_everytest_wrap <- function(distvecList, trdf, tedf, k){
  
  # check IDs
  is_IDmatch <- knn_checkID(trdf = trdf, 
                            tedf = tedf, 
                            distvec_list = distvecList)
  
  if(is_IDmatch == F){
    stop('ID do not match\n')
  }else{
    # convert to numeric vector
    Xvec_num <- map(distvecList, as.numeric)
    knn_outputs <- knn_everytest(distvecList = Xvec_num, 
                                 trainlabel = trdf$death, 
                                 k = k)
    
    # need to transform the probability of the negative class into 1-p
    knn_outputsDF <- data.frame(cl = knn_outputs$classes, 
                                p_eachcl = knn_outputs$probs)
    
    knn_outputsDF$p_bothcl <- ifelse(knn_outputsDF$cl == 1, 
                                     knn_outputsDF$p_eachcl, 
                                     1-knn_outputsDF$p_eachcl)
    # compute the performance
    knn_metrics <- measures(actualLabel = tedf$death, 
                            predictedLabel = knn_outputsDF$cl,
                            predictedP = knn_outputsDF$p_bothcl)
    
    
  }
  return(knn_metrics)
  
}


