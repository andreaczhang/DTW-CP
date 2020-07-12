measures <- function(actualLabel, predictedLabel, predictedP){
  auc <- Metrics::auc(actual = actualLabel, 
                      predicted = predictedP)
  # precision recall curve
  pr_obj <- pr.curve(scores.class0 = predictedP[actualLabel == 1],  # actual positive (tp+fn)
                     scores.class1 = predictedP[actualLabel == 0],  # actual negative (tn+fp)
                     curve = T)
  auprc <- pr_obj$auc.integral
  # these 3 are using predicted labels (0, 1)
  accu <- Metrics::accuracy(actual = actualLabel, 
                            predicted = predictedLabel)
  return(list(auc = auc,
              auprc = auprc , 
              accu = accu)) 
  
}



createmodel_bilstm <- function(nUnit, dataDim, timeSteps){
  # keep the unit fixed 
  # one bilstm layer
  model <- keras_model_sequential()
  model %>% 
    bidirectional(layer_lstm(units = nUnit),  
                  input_shape = c(timeSteps, dataDim))%>% 
    layer_dense(units = 1, activation = 'sigmoid')
  
  model %>% 
    compile(
      loss = 'binary_crossentropy',
      optimizer = 'rmsprop',
      metrics = c('accuracy')
    )
  return (model = model)
}




createmodel_lstm1 <- function(nUnit, dataDim, timeSteps){
  # keep the unit fixed 
  # one lstm layers 
  model <- keras_model_sequential()
  model %>% 
    layer_lstm(units = nUnit, 
               input_shape = c(timeSteps, dataDim), 
               activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'sigmoid') 
  
  model %>% 
    compile(
      loss = 'binary_crossentropy',
      optimizer = 'rmsprop',
      metrics = c('accuracy')
    )
  return (model = model)
}



createmodel_lstm2 <- function(nUnit, dataDim, timeSteps){
  # keep the unit fixed 
  # two lstm layers 
  # when stacking more than 1 lstm layer, return sequence from the first 
  model <- keras_model_sequential()
  model %>% 
    layer_lstm(units = nUnit, 
               return_sequences = TRUE, 
               input_shape = c(timeSteps, dataDim), 
               activation = 'relu') %>% 
    layer_lstm(units = nUnit, activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'sigmoid') 
  
  model %>% 
    compile(
      loss = 'binary_crossentropy',
      optimizer = 'rmsprop',
      metrics = c('accuracy')
    )
  return (model = model)
}



# ======= predict using the FITTED model objects ====== # 
predlstm <- function(lstmobj, Xtest, Ytest){
  
  yPredictPositiveProb <- predict_proba(lstmobj, Xtest) 
  yPredictLab <- predict_classes(lstmobj, Xtest)
  
  measure_obj <- measures(actualLabel = Ytest, 
                          predictedLabel = yPredictLab, 
                          predictedP = yPredictPositiveProb)
  
  return(list(yPredictLab = yPredictLab, 
              yPredictPositiveProb = yPredictPositiveProb, 
              measure_obj = measure_obj))
}


predlstm_debug <- function(lstmobj, Xtest, Ytest){
  tryCatch(
    expr = {
      # this is when it goes alright 
      yPredictPositiveProb <- predict_proba(lstmobj, Xtest) 
      yPredictLab <- predict_classes(lstmobj, Xtest)
      
      measure_obj <- measures(actualLabel = Ytest, 
                              predictedLabel = yPredictLab, 
                              predictedP = yPredictPositiveProb)
      
      return(list(yPredictLab = yPredictLab, 
                  yPredictPositiveProb = yPredictPositiveProb, 
                  measure_obj = measure_obj))
    },
    
    error = function(e){
      # message('Caught an error!')
      return(list(lstmobj = lstmobj, 
                  Xtest = Xtest, 
                  Ytest = Ytest))
    }  
  )    # end of try catch  
  
}

predLSTMBoot_debug <- function(lstmobj, Xtest, Ytest, bootArg){
  cat('LSTM classification with bootstrap (debug mode) \n')
  
  # create boot samples 
  Bsize <- bootArg
  vecauc <- vecauprc <- vecaccu <- rep(0, Bsize) 
  errorholder <- list()
  
  # ----- guarantee we have at least one positive ----- # 
  id_y_positive <- which(Ytest == 1)
  id_y_negative <- which(Ytest == 0)
  
  # this is a vector of 100 elements, to guarantee at least one positive case
  minimum_pos_vec <- sapply(seq(1:Bsize), function(s){ 
    sample(id_y_positive, size = 1, replace = T)})
  
  minimum_neg_vec <- sapply(seq(1:Bsize), function(s){ 
    sample(id_y_negative, size = 1, replace = T)})
  
  # the rest just sample from the whole indices
  others_indmat <- sapply(seq(1:Bsize), function(s){ 
    sample(1:length(Ytest), size = length(Ytest)-2, replace = T)})
  
  # bind these two together 
  indMat <- rbind(minimum_pos_vec, minimum_neg_vec, others_indmat)
  
  for (i in 1:Bsize){
    XBoot <- Xtest[indMat[, i], , ]  # 3d array
    yBoot <- Ytest[indMat[, i]]
    
    # this is run as usual 
    res <- predlstm_debug(lstmobj = lstmobj, Xtest = XBoot, yBoot)
    
    if('lstmobj' %in% names(res)){
      # this means there is an error
      errorholder[[i]] <- res
      vecauc[i] <- NA
      vecauprc[i] <- NA
      vecaccu[i] <- NA
    }else{
      # this means it goes ok 
      errorholder[[i]] <- NULL
      vecauc[i] <- res$measure_obj$auc 
      vecauprc[i] <- res$measure_obj$auprc
      vecaccu[i] <- res$measure_obj$accu
    }
  } 
  return(list(errorholder = errorholder, 
              vecauc = vecauc, 
              vecauprc = vecauprc, 
              vecaccu = vecaccu))
} # end of function 





predLSTMBoot <- function(lstmobj, Xtest, Ytest, bootArg){
  
  cat('LSTM classification with bootstrap\n')
  
  # create boot samples 
  Bsize <- bootArg
  vecauc <- vecauprc <- vecaccu <- rep(0, Bsize) 
  
  # ----- guarantee we have at least one positive ----- # 
  id_y_positive <- which(Ytest == 1)
  id_y_negative <- which(Ytest == 0)
  
  # this is a vector of 100 elements, to guarantee at least one positive case
  minimum_pos_vec <- sapply(seq(1:Bsize), function(s){ 
    sample(id_y_positive, size = 1, replace = T)})
  
  minimum_neg_vec <- sapply(seq(1:Bsize), function(s){ 
    sample(id_y_negative, size = 1, replace = T)})
  
  # the rest just sample from the whole indices
  others_indmat <- sapply(seq(1:Bsize), function(s){ 
    sample(1:length(Ytest), size = length(Ytest)-2, replace = T)})
  
  # bind these two together 
  indMat <- rbind(minimum_pos_vec, minimum_neg_vec, others_indmat)
  
  for (i in 1:Bsize){
    
    XBoot <- Xtest[indMat[, i], , ]  # 3d array
    yBoot <- Ytest[indMat[, i]]
    
    res <- predlstm(lstmobj = lstmobj, Xtest = XBoot, yBoot)      ##
    
    vecauc[i] <- res$measure_obj$auc 
    vecauprc[i] <- res$measure_obj$auprc
    vecaccu[i] <- res$measure_obj$accu
    # cat(paste0('B sample ', i, ' done\n'))
  }
  
  return(list(vecauc = vecauc, 
              vecauprc = vecauprc, 
              vecaccu = vecaccu))
}




runLSTM_model <- function(XTR, YTR, XTE, YTE, nUnit, modelName, isDebug = NULL){
  # npatient <- dim(XTR)[1]
  ntime <- dim(XTR)[2]
  nfeature <- dim(XTR)[3]
  
  # train the model 
  if(modelName == 'lstm1'){
    lstmmodel <- createmodel_lstm1(nUnit = nUnit, dataDim = nfeature, timeSteps = ntime)
    
  }else if(modelName == 'lstm2'){
    lstmmodel <- createmodel_lstm2(nUnit = nUnit, dataDim = nfeature, timeSteps = ntime)
    
  }else if(modelName == 'bilstm1'){
    lstmmodel <- createmodel_bilstm(nUnit = nUnit, dataDim = nfeature, timeSteps = ntime)
    
  }else{
    stop('Model is not implemented, please specify which: lstm1, lstm2, bilstm1')
  }
  
  # then fit the model 
  lstmmodel %>% fit(XTR, YTR, 
                    batch_size = 32,
                    epochs = 20, 
                    validation_split = 0.2, 
                    callbacks = list(callback_early_stopping(monitor = 'val_loss', 
                                                             patience = 5)))
  
  cat('LSTM model: ', modelName, ' fitting complete, start prediction\n')
  if(isDebug == T){
    lstmpredictions <- predLSTMBoot_debug(lstmobj = lstmmodel, Xtest = XTE, Ytest = YTE, bootArg = 100)
  }else{
    lstmpredictions <- predLSTMBoot(lstmobj = lstmmodel, Xtest = XTE, Ytest = YTE, bootArg = 100)
  }
  return(lstmpredictions = lstmpredictions)
}


