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


baseGLM <- function(Xtr, Ytr, Xte, Yte){
  trdf <- data.frame(X = Xtr, Y = Ytr)
  tedf <- data.frame(X = Xte, Y = Yte)
  
  mod <- glm(Y ~., family = 'binomial', data = trdf)
  positiveProb <- predict(mod, newdata = tedf[, 1:ncol(tedf)], type = 'response')
  yPredictLab <- ifelse(positiveProb >= 0.5, 1, 0)
  # aucval <- Metrics::auc(actual = Yte, predicted = predmod)
  measure_obj <- measures(actualLabel = Yte, 
                          predictedLabel = yPredictLab, 
                          predictedP = positiveProb)
  res <- data.frame(auc = measure_obj$auc, 
                    auprc = measure_obj$auprc, 
                    accu = measure_obj$accu)
  return(res = res)
}







trainPred_svmL <- function(Xtr, Xte, Ytr, Yte){
  # first try random forest 
  dataTR <- data.frame(X = Xtr, Y = factor(Ytr))
  dataTE <- data.frame(X = Xte, Y = factor(Yte))
  
  ctrl <- trainControl(method = "cv",     # Cross-validation
                       number = 5,      # 5 folds
                       classProbs = TRUE,                  # For AUC
                       summaryFunction = twoClassSummary)  # For AUC
  
  svmL_model <- train(make.names(Y) ~ .,
                      data = dataTR, 
                      method = "svmLinear",
                      metric = "ROC",
                      trControl = ctrl, 
                      preProcess = c("center","scale"), 
                      tuneGrid = expand.grid(C = seq(0.01, 2, length = 10)))
  
  # predict on test set
  prob_svmL <- predict(object = svmL_model, 
                       newdata = dataTE[, 1:(ncol(dataTE)-1)],
                       type = "prob")
  
  positiveProb <- prob_svmL$X1
  label <- ifelse(positiveProb >= 0.5, 1, 0)
  
  measure_obj <- measures(actualLabel = dataTE$Y, 
                          predictedLabel = label, 
                          predictedP = positiveProb)
  
  res <- data.frame(auc = measure_obj$auc, 
                    auprc = measure_obj$auprc, 
                    accu = measure_obj$accu)
  
  return(res = res)
}



trainPred_svmR <- function(Xtr, Xte, Ytr, Yte){
  # first try random forest 
  dataTR <- data.frame(X = Xtr, Y = factor(Ytr))
  dataTE <- data.frame(X = Xte, Y = factor(Yte))
  
  ctrl <- trainControl(method = "cv",     
                       number = 5,      
                       classProbs = TRUE,                  
                       summaryFunction = twoClassSummary) 
  
  svmR_model <- train(make.names(Y) ~ .,
                      data = dataTR, 
                      method = "svmRadial",
                      metric = "ROC",
                      trControl = ctrl, 
                      preProcess = c("center","scale"), 
                      tuneLength = 10)
  
  # predict on test set
  prob_svmR <- predict(object = svmR_model, 
                       newdata = dataTE[, 1:(ncol(dataTE)-1)],
                       type = "prob")
  
  positiveProb <- prob_svmR$X1
  label <- ifelse(positiveProb >= 0.5, 1, 0)
  
  measure_obj <- measures(actualLabel = dataTE$Y, 
                          predictedLabel = label, 
                          predictedP = positiveProb)
  
  res <- data.frame(auc = measure_obj$auc, 
                    auprc = measure_obj$auprc, 
                    accu = measure_obj$accu)
  
  return(res = res)
}





getCVfolds <- function(XTR, YTR, nfold){
  fold_id_list <- createFolds(YTR, k = nfold, list = T, returnTrain = FALSE)
  
  # first create the datasets
  xtrlist <- list()
  ytrlist <- list()
  xtelist <- list()
  ytelist <- list()
  
  for(k in 1:nfold){
    xtrlist[[k]] <- XTR[-fold_id_list[[k]],] # df
    xtelist[[k]] <- XTR[fold_id_list[[k]], ]
    ytrlist[[k]] <- YTR[-fold_id_list[[k]]] # vector
    ytelist[[k]] <- YTR[fold_id_list[[k]]]
  }
  return(list(xtrlist = xtrlist, 
              xtelist = xtelist, 
              ytrlist = ytrlist, 
              ytelist = ytelist))
}



# =========== GLM specific ============== #

baseGLM_TRcv <- function(Xtr, Ytr, nfold){
  TR_cvfolds <- getCVfolds(XTR = Xtr, YTR = Ytr, nfold = nfold)
  auc_vec_cv <- c()
  for(k in 1:nfold){
    auc_vec_cv[k] <- baseGLM_cv_auc_single(Xtr = TR_cvfolds$xtrlist[[k]], 
                                           Ytr = TR_cvfolds$ytrlist[[k]], 
                                           Xte = TR_cvfolds$xtelist[[k]], 
                                           Yte = TR_cvfolds$ytelist[[k]])
  }
  mean_auc_cv <- mean(auc_vec_cv)
  
  return(mean_auc_cv)
}


baseGLM_cv_auc_single <- function(Xtr, Ytr, Xte, Yte){
  trdf <- data.frame(X = Xtr, Y = Ytr)
  tedf <- data.frame(X = Xte, Y = Yte)
  
  mod <- glm(Y ~., family = 'binomial', data = trdf)
  positiveProb <- predict(mod, newdata = tedf[, 1:ncol(tedf)], type = 'response')
  auc <- Metrics::auc(actual = Yte, predicted = positiveProb)
  return(auc)
}



trainPred_svmL_onepivot <- function(Xtr, Ytr){
  
  trainIndex <- createDataPartition(Ytr, p = .7, list = FALSE)
  # for Xtr alone
  xtr_holdout <- Xtr[trainIndex,]
  ytr_holdout <- Ytr[trainIndex]
  xte_holdout <- Xtr[-trainIndex,]
  yte_holdout <- Ytr[-trainIndex]
  
  dataTR <- data.frame(X = xtr_holdout, Y = factor(ytr_holdout))
  dataTE <- data.frame(X = xte_holdout, Y = factor(yte_holdout))
  
  # train on the tr
  ctrl <- trainControl(method = "cv",     # Cross-validation
                       number = 5,      # 5 folds
                       classProbs = TRUE,                  # For AUC
                       summaryFunction = twoClassSummary)  # For AUC
  
  svmL_model <- train(make.names(Y) ~ .,
                      data = dataTR, 
                      method = "svmLinear",
                      metric = "ROC",
                      trControl = ctrl, 
                      preProcess = c("center","scale"), 
                      tuneGrid = expand.grid(C = seq(0.01, 2, length = 10)))
  
  # predict on test set
  prob_svmL <- predict(object = svmL_model, 
                       newdata = dataTE[, 1:(ncol(dataTE)-1)],
                       type = "prob")
  
  positiveProb <- prob_svmL$X1
  label <- ifelse(positiveProb >= 0.5, 1, 0)
  
  auc <- Metrics::auc(actual = dataTE$Y, predicted = positiveProb)
  return(auc)
}








trainPred_svmR_onepivot <- function(Xtr, Ytr){
  trainIndex <- createDataPartition(Ytr, p = .7, list = FALSE)
  # for Xtr alone
  xtr_holdout <- Xtr[trainIndex,]
  ytr_holdout <- Ytr[trainIndex]
  xte_holdout <- Xtr[-trainIndex,]
  yte_holdout <- Ytr[-trainIndex]
  
  dataTR <- data.frame(X = xtr_holdout, Y = factor(ytr_holdout))
  dataTE <- data.frame(X = xte_holdout, Y = factor(yte_holdout))
  
  ctrl <- trainControl(method = "cv",     # Cross-validation
                       number = 5,      # 5 folds
                       classProbs = TRUE,                  # For AUC
                       summaryFunction = twoClassSummary)  # For AUC
  
  svmR_model <- train(make.names(Y) ~ .,
                      data = dataTR, 
                      method = "svmRadial",
                      metric = "ROC",
                      trControl = ctrl, 
                      preProcess = c("center","scale"), 
                      tuneLength = 10)
  
  # predict on test set
  prob_svmR <- predict(object = svmR_model, 
                       newdata = dataTE[, 1:(ncol(dataTE)-1)],
                       type = "prob")
  
  positiveProb <- prob_svmR$X1
  label <- ifelse(positiveProb >= 0.5, 1, 0)
  
  auc <- Metrics::auc(actual = dataTE$Y, predicted = positiveProb)
  return(auc)
}




trainManyLearners <- function(Xtr, Ytr){
  
  dataTR <- data.frame(X = Xtr, Y = factor(Ytr))
  Xtrain <- dataTR[, c(1:(ncol(dataTR)-1))]
  ytrain <- dataTR[, ncol(dataTR)]
  
  # ---------------- set model config ------------- # 
  # control 2 gives class label: need to specify class prob, twoclass summary
  # and metric = ROC for the training 
  # suitable for svm.
  control <- trainControl(method = 'cv', 
                          number = 5, 
                          savePredictions = 'final', 
                          classProbs = T,
                          summaryFunction = twoClassSummary,
                          index = createResample(ytrain, 5))
  
  # ------------- model list ---------- # 
  cat(paste0('Starting training for ', ncol(Xtr), ' components\n'))
  
  # 2. use roc 
  modelList <- caretList(Xtrain, 
                         make.names(ytrain), 
                         trControl = control,   # note that if use two class sum, need to use roc
                         methodList = c('glm','svmLinear'), 
                         tuneList = NULL, 
                         preProcess = c('center', 'scale'), 
                         metric = 'ROC')
  
  cat(paste0('Model fitted \n'))
  
  return(list(modelList = modelList))
}



# ========== predict a single learner ========== # 
predictLearner <- function(trainedSingleModel, X, Y){
  
  pred_obj <- predict.train(trainedSingleModel, newdata = X, type = 'prob')
  positiveProb <- pred_obj[, which(colnames(pred_obj) == 'X1')]
  yPredictLab <- ifelse(positiveProb>=0.5, 1, 0)
  
  measure_obj <- measures(actualLabel = Y, 
                          predictedLabel = yPredictLab, 
                          predictedP = positiveProb)
  return(measure_obj = measure_obj)
}






# ========== predict a single learner, bootstrap ========== # 
testSingleLearner <- function(trainedModel, Xte, yte){
  dataTE <- data.frame(X = Xte, Y = factor(yte))
  Xtest <- dataTE[, c(1:(ncol(dataTE)-1))]
  ytest <- dataTE[, ncol(dataTE)]
  
  measure_obj <- predictLearner(trainedSingleModel = trainedModel, 
                                X = Xtest, 
                                Y = ytest)
  res <- data.frame(auc = measure_obj$auc, 
                    auprc = measure_obj$auprc, 
                    accu = measure_obj$accu)
  
  return(res = res)
  
}


testSingleLearnerBoot <- function(trainedModel, Xte, yte, bootArgs){
  dataTE <- data.frame(X = Xte, Y = factor(yte))
  Xtest <- dataTE[, c(1:(ncol(dataTE)-1))]
  ytest <- dataTE[, ncol(dataTE)]
  
  # create boot samples 
  Bsize <- bootArgs
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
    dataTEBoot <- dataTE[indMat[, i], ] 
    
    measure_obj <- predictLearner(trainedSingleModel = trainedModel, 
                                  X = dataTEBoot[, c(1:(ncol(dataTE)-1))], 
                                  Y = dataTEBoot[, ncol(dataTE)])
    
    vecauc[i] <- measure_obj$auc 
    vecauprc[i] <- measure_obj$auprc
    vecaccu[i] <- measure_obj$accu
  }
  
  return(list(vecauc = vecauc, 
              vecauprc = vecauprc, 
              vecaccu = vecaccu
  ))
}






testAllLearners <- function(trainedModels, Xte, yte, bootArgs){
  
  if(bootArgs == F){
    glm <- testSingleLearner(trainedModel = trainedModels$modelList$glm, 
                             Xte = Xte, 
                             yte = yte)
    cat('(test) glm done\n')
    
    svm <- testSingleLearner(trainedModel = trainedModels$modelList$svmLinear, 
                             Xte = Xte, 
                             yte = yte)
    cat('(test) svm done\n')
    
  }else{
    glm <- testSingleLearnerBoot(trainedModel = trainedModels$modelList$glm, 
                                 Xte = Xte, 
                                 yte = yte, 
                                 bootArgs = bootArgs)
    cat('(test) glm done\n')
    
    svm <- testSingleLearnerBoot(trainedModel = trainedModels$modelList$svmLinear, 
                                 Xte = Xte, 
                                 yte = yte, 
                                 bootArgs = bootArgs)
    cat('(test) svm done\n')
    
    
  }
  return(list(glm = glm, 
              svm = svm))
}




runLearners_percomp <- function(Xtr, Xte, Ytr, Yte, bootarg){
  
  trainedLearners <- trainManyLearners(Xtr = Xtr, Ytr = factor(Ytr))
  # use test all leaners _2 for only glm and svm 
  testLearners <- testAllLearners(trainedModels = trainedLearners, 
                                  Xte = Xte, 
                                  yte = Yte, 
                                  bootArgs = bootarg)
  
  return(testLearners = testLearners)
}




# ===== put all components together ==== # 

runLearners_allcomp <- function(Xtr_allcomp, Xte_allcomp, Ytr, Yte){
  mat_auc_list <- list()
  mat_auprc_list <- list()
  mat_accu_list <- list()
  
  for(i in 1:length(Xtr_allcomp)){
    reslist <- runLearners_percomp(Xtr = Xtr_allcomp[[i]], 
                                   Xte = Xte_allcomp[[i]], 
                                   Ytr = Ytr, 
                                   Yte = Yte, 
                                   bootarg = 100)
    mat_auc_list[[i]] <- map(reslist, function(x){x$vecauc})
    mat_auprc_list[[i]] <- map(reslist, function(x){x$vecauprc})
    mat_accu_list[[i]] <- map(reslist, function(x){x$vecaccu})
    
  }
  auc_glm <- map(mat_auc_list, function(x){x$glm}) %>% do.call(cbind, .)
  auprc_glm <- map(mat_auprc_list, function(x){x$glm}) %>% do.call(cbind, .)
  accu_glm <- map(mat_accu_list, function(x){x$glm}) %>% do.call(cbind, .)
  
  auc_svm <- map(mat_auc_list, function(x){x$svm}) %>% do.call(cbind, .)
  auprc_svm <- map(mat_auprc_list, function(x){x$svm}) %>% do.call(cbind, .)
  accu_svm <- map(mat_accu_list, function(x){x$svm}) %>% do.call(cbind, .)
  
  # name them 
  names(auc_glm) <- names(Xtr_allcomp)
  names(auprc_glm) <- names(Xtr_allcomp)
  names(accu_glm) <- names(Xtr_allcomp)
  
  names(auc_svm) <- names(Xtr_allcomp)
  names(auprc_svm) <- names(Xtr_allcomp)
  names(accu_svm) <- names(Xtr_allcomp)
  
  
  return(list(auc_glm = auc_glm, 
              auc_svm = auc_svm, 
              auprc_glm = auprc_glm, 
              auprc_svm = auprc_svm,
              accu_glm = accu_glm, 
              accu_svm = accu_svm))
}







