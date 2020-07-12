# examine some performance for AKI,
# focus on knn1 and knn5, why the performance differ so much 
library(Metrics)
source('./Utilities/helpers-rev-6knn.R')
source('./Utilities/helpers-rev-6classify.R')
dataPath <- '~/Documents/Data/Project2/'

out_aki_d7 <-  readRDS(paste0(dataPath, 'AKI/REVISION-AKI/alloutcomes_aki_day7.RData'))
disvec_aki <- readRDS(file = paste0(dataPath, 'AKI/REVISION-AKI/knn_disvecs_aki.RData'))


s <- 3

knnres_aki_d7_eachs_k1 <- knn_everytest_wrap(distvecList = disvec_aki[[7]][[s]], 
                                              trdf = out_aki_d7[[s]]$trdf, 
                                              tedf = out_aki_d7[[s]]$tedf, 
                                              k = 1)
# accuracy 0.88
knnres_aki_d7_eachs_k5 <- knn_everytest_wrap(distvecList = disvec_aki[[7]][[s]], 
                                             trdf = out_aki_d7[[s]]$trdf, 
                                             tedf = out_aki_d7[[s]]$tedf, 
                                             k = 5)
# accuracy 0.87

# indeed, the auprc differ much 
# figure out by examining the probability 

Xvec_num <- map(disvec_aki[[7]][[s]], as.numeric)
knn_outputs_k1 <- knn_everytest(distvecList = Xvec_num, 
                             trainlabel = out_aki_d7[[s]]$trdf$death, 
                             k = 1)
knn_outputs_k5 <- knn_everytest(distvecList = Xvec_num, 
                                trainlabel = out_aki_d7[[s]]$trdf$death, 
                                k = 5)

table(knn_outputs_k1$classes, knn_outputs_k5$classes)
# only 7 unequal

table(out_aki_d7[[s]]$tedf$death, knn_outputs_k1$classes)
table(out_aki_d7[[s]]$tedf$death, knn_outputs_k5$classes)


# need to transform the probability of the negative class into 1-p
knn_outputsDF_k1 <- data.frame(cl = knn_outputs_k1$classes, 
                            p_eachcl = knn_outputs_k1$probs)
knn_outputsDF_k5 <- data.frame(cl = knn_outputs_k5$classes, 
                               p_eachcl = knn_outputs_k5$probs)



knn_outputsDF_k1$p_bothcl <- ifelse(knn_outputsDF_k1$cl == 1, 
                                 knn_outputsDF_k1$p_eachcl, 
                                 1-knn_outputsDF_k1$p_eachcl)

knn_outputsDF_k5$p_bothcl <- ifelse(knn_outputsDF_k5$cl == 1, 
                                    knn_outputsDF_k5$p_eachcl, 
                                    1-knn_outputsDF_k5$p_eachcl)


plot(knn_outputsDF_k1$p_bothcl)
lines(knn_outputsDF_k5$p_bothcl, type = 'l', col = 'red')






pr_k1 <- Metrics::precision(actual = out_aki_d7[[s]]$tedf$death, 
                  predicted = knn_outputsDF_k1$cl)
re_k1 <- Metrics::recall(actual = out_aki_d7[[s]]$tedf$death, 
                            predicted = knn_outputsDF_k1$cl)

pr_k5 <- Metrics::precision(actual = out_aki_d7[[s]]$tedf$death, 
                            predicted = knn_outputsDF_k5$cl)
re_k5 <- Metrics::recall(actual = out_aki_d7[[s]]$tedf$death, 
                         predicted = knn_outputsDF_k5$cl)

Metrics::precision()
?Metrics::precision

table(out_aki_d7[[s]]$tedf$death, knn_outputsDF_k1$cl)
table(out_aki_d7[[s]]$tedf$death, knn_outputsDF_k5$cl)

try3 <- ifelse(knn_outputsDF_k1$p_bothcl[out_aki_d7[[s]]$tedf$death == 1] >0.5, 0.9, 0.1)
try4 <- ifelse(knn_outputsDF_k1$p_bothcl[out_aki_d7[[s]]$tedf$death == 0] >0.5, 0.9, 0.1)

pr_obj_k1 <- pr.curve(scores.class0 = try3,  # actual positive (tp+fn)
                      scores.class1 = try4,  # actual negative (tn+fp)
                      curve = T)
pr_obj_k1$auc.integral
pr_obj_k1$curve %>% plot


pr_obj_k5 <- pr.curve(scores.class0 = knn_outputsDF_k5$p_bothcl[out_aki_d7[[s]]$tedf$death == 1],  # actual positive (tp+fn)
                      scores.class1 = knn_outputsDF_k5$p_bothcl[out_aki_d7[[s]]$tedf$death == 0],  # actual negative (tn+fp)
                      curve = T)
pr_obj_k5$auc.integral

pr_obj_k5$curve %>% plot

tryp1 <- ifelse(knn_outputsDF_k5$p_bothcl[out_aki_d7[[s]]$tedf$death == 1] >0.5, 1, 0)
tryp2 <- ifelse(knn_outputsDF_k5$p_bothcl[out_aki_d7[[s]]$tedf$death == 0] >0.5, 1, 0)

# hard coding 

pr_obj_k52 <- pr.curve(tryp1,  # actual positive (tp+fn)
                      tryp2,  # actual negative (tn+fp)
                      curve = T)
pr_obj_k52$auc.integral



