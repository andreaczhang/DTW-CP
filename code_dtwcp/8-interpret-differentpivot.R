# get projection <PART 2> (this time in R)
# 2. query (by index) the pivot distances (on pink)

# first need to examine the outcomes 
library(magrittr)
library(purrr)
library(corrplot)
source('~/Documents/PhdProjects/Project-Paper2/Utilities/helpers-rev-5postTensor.R')
source('~/Documents/PhdProjects/Project-Paper2/Utilities/helpers-rev-6classify.R')

dataPath <- '~/Documents/Data/Project2/Ripoll2014/REVISION-RIPOLL/'
simMatPath_sep <- 'dtw-sep-feats/'


# for the trial purpose, take 1-4 days's first split 
outlist_day2 <- readRDS(paste0(dataPath, 'alloutcomes_sep_day2.RData'))


# training 
trID_day2_10splits <- map(outlist_day2, function(x){paste0('icustay_', x$trdf$icustay_id)})

# test
teID_day2_10splits <- map(outlist_day2, function(x){paste0('icustay_', x$tedf$icustay_id)})



# load the features 

dtwsimmat_name <- 'sepfull-default-feat'
featlist <- list()
for(f in 1:52){  
  featlist[[f]] <- readRDS(paste0(dataPath, simMatPath_sep, dtwsimmat_name, f, '.RData'))
  cat('Feat ', f, ' of ', dtwsimmat_name, ' loaded\n')
}
  






# ======== query a few splis, for day3 ========= # 

# set.seed(1)

# randomly select 50 sets
n <- length(trID_day2_10splits$split2)
# pivots <- sample(1:length(trID_day3_10splits$split2), size = 100, replace = F)
pivots <- seq(1, n, by = 1)


slices_pivots <- list()
for(i in 1:n){
  colList_pvt_split2 <- list()
  for(f in 1:52){
    # change the pivot everytime 
    colList_pvt_split2[[f]] <- trteOneCol_eachDay(trIDvec = trID_day2_10splits$split2,
                                                     teIDvec = teID_day2_10splits$split2,
                                                     singleFeatMat = featlist[[f]],
                                                     pivotLoc = pivots[i])
  }
  slices_pivots[[i]] <- gatherColsTRTE(colList_pvt_split2)
  cat('Pivot ', i, ' done\n')
}

names(slices_pivots) <- paste0('pivot', 1:n)
# slices_pivots_d3
# put into matrix 
# trte <- gatherColsTRTE(colList_pvt_d3_split2)
# cor(trte$trMat) %>% corrplot





#slices_pivots_d3[[1]]$teMat %>% head
# ========== try prediction using the raw features ========= # 
# record the coefficients too 

# simple preediction test out 
Ytr <- factor(outlist_day2$split2$trdf$death)
Yte <- factor(outlist_day2$split2$tedf$death)


simpleLR_res <- list()
for(i in 1:n){
  simpleLR_res[[i]] <- simpleLR(xtr = slices_pivots[[i]]$trMat, 
                                ytr = Ytr, 
                                xte = slices_pivots[[i]]$teMat, 
                                yte = Yte)
  cat('pivot ', i, 'done\n')
}


simpleLR_coefs <- map_df(simpleLR_res, pluck(1))
simpleLR_aucs <- map_dbl(simpleLR_res, function(x){x$res$auc})
simpleLR_auprc <- map_dbl(simpleLR_res, function(x){x$res$auprc})
simpleLR_accu <- map_dbl(simpleLR_res, function(x){x$res$accu})

simpleLR_coefs %>% boxplot
simpleLR_aucs %>% boxplot
simpleLR_aucs %>% sd
simpleLR_aucs %>% summary




# ----- produce projections too ------ # 
featmat_d4 <- readMat('~/Documents/Data/Project2/Ripoll2014/REVISION-RIPOLL/featmats-default-sep/day2sep_featmats_default_split2.mat')
fm_d4 <- featMat_matlabToR(featmat_d4)


projectLR_res <- list()

cM30 <- fm_d4$comp30
cM25 <- fm_d4$comp25

for(i in 1:n){
  projectLR_res[[i]] <- projectLR(xtr = slices_pivots[[i]]$trMat,
                                ytr = Ytr,
                                xte = slices_pivots[[i]]$teMat,
                                yte = Yte,
                                compMat = cM30)

  cat('pivot ', i, 'done\n')
}



# projectLR_coefs <- map_df(projectLR_res, pluck(1))
projectLR_aucs <- map_dbl(projectLR_res, function(x){x$res$auc})
projectLR_auprc <- map_dbl(projectLR_res, function(x){x$res$auprc})
projectLR_accu <- map_dbl(projectLR_res, function(x){x$res$accu})


projectLR_coefs %>% boxplot
projectLR_aucs %>% boxplot
projectLR_aucs %>% sd
projectLR_aucs %>% summary



# simpleLR_aucs
# simpleLR_auprc %>% boxplot
# projectLR_auprc %>% boxplot
# simpleLR_accu %>% boxplot
# projectLR_accu %>% boxplot



projectLR_aucs %>% plot(type = 'b')
simpleLR_aucs %>% lines(col = 'red')

(projectLR_aucs - simpleLR_aucs) %>% hist

id_projectbetter <- which(projectLR_aucs - simpleLR_aucs > 0.01)
id_projectworse <- which(projectLR_aucs - simpleLR_aucs < -0.01)

barplot(as.numeric(simpleLR_coefs[id_projectbetter, ]), col = 'red', pch= 20, ylim = c(-50, 50))






par(mfrow = c(1, 1))
cM25[, 1] %>% barplot
cM25[, 2] %>% barplot
cM25[, 3] %>% barplot
cM25[, 4] %>% barplot
cM25[, 5] %>% barplot

cM25[, 6] %>% barplot
cM25[, 7] %>% barplot
cM25[, 8] %>% barplot
cM25[, 9] %>% barplot
cM25[, 10] %>% barplot


proj <- slices_pivots_d3[[1]]$trMat %*% fm_d3$comp15
i <- 41
slices_pivots_d3[[1]]$trMat[i, ] %>% barplot
proj[i, ] %>% barplot(ylim = c(-600, 600))

fm_d3$comp15[, 1]
slices_pivots_d3[[1]]$trMat %>% boxplot
which(slices_pivots_d3[[1]]$trMat[, 52]>150) # 29

slices_pivots_d3[[1]]$trMat[28, ] %>% barplot

plot(slices_pivots_d3[[41]]$trMat[, c(52)], col = Ytr, pch = 20)




simpleLR_aucs[29]
projectLR_aucs[29]
# ================ #
# compar these two 

plot(simpleLR_aucs, type = 'b')
lines(projectLR_aucs, col = 'red')

summary(simpleLR_aucs)
summary(projectLR_aucs)

diff_aucs <- projectLR_aucs - simpleLR_aucs
which(diff_aucs < -0.02)
diff_aucs %>% hist


# examine outliers first from the original 
which(simpleLR_aucs >= 0.94)
# 142 212 429 521 549
which(simpleLR_aucs <= 0.85)
# 164, 196, 289, 357, 426, 490

simpleLR_coefs %>% boxplot
barplot(as.numeric(simpleLR_coefs[41, ]), col = 'red', pch= 20, ylim = c(-50, 50))
barplot(as.numeric(simpleLR_coefs[196, ]), col = 'red', pch= 20, ylim = c(-50, 50))
barplot(as.numeric(simpleLR_coefs[289, ]), col = 'red', pch= 20, ylim = c(-50, 50))
barplot(as.numeric(simpleLR_coefs[357, ]), col = 'red', pch= 20, ylim = c(-50, 50))
barplot(as.numeric(simpleLR_coefs[426, ]), col = 'red', pch= 20, ylim = c(-50, 50))

barplot(as.numeric(simpleLR_coefs[142, ]), col = 'red', pch= 20, ylim = c(-50, 50))
barplot(as.numeric(simpleLR_coefs[429, ]), col = 'red', pch= 20, ylim = c(-50, 50))
barplot(as.numeric(simpleLR_coefs[521, ]), col = 'red', pch= 20, ylim = c(-50, 50))
barplot(as.numeric(simpleLR_coefs[549, ]), col = 'red', pch= 20, ylim = c(-50, 50))
barplot(as.numeric(simpleLR_coefs[212, ]), col = 'red', pch= 20, ylim = c(-50, 50))

simpleLR_coefs$X.23 %>% hist
which(abs(simpleLR_coefs$X.23-median(simpleLR_coefs$X.23))<= 1)
# 7 107 117 118 132 145 186 192 204 279 281 356 380 400 414 416 451 473 500 537 556

barplot(as.numeric(simpleLR_coefs[7, ]), col = 'red', pch= 20, ylim = c(-50, 50))


simpleLR_coefs[4, 23]





