# in this way, compute the score over 52 features 
library(magrittr)
library(purrr)
library(data.table)
library(dplyr)
library(mltools)
library(tictoc)
library(caret)
library(keras)
library(pROC)
library(Metrics)  
library(PRROC)    
library(FastKNN)

# use the available pivot slice's each column as input (no projection)

dataPath <- '~/Documents/Data/Project2/Ripoll2014/REVISION-RIPOLL/'

# test run the pivotmats of day3 

d3_pivots_default <- readRDS(paste0(dataPath, 'day3-default-pivotMats.RData'))
d3_outcomes <- readRDS(paste0(dataPath, 'alloutcomes_sep_day3.RData'))


Xtr <- d3_pivots_default$split1$trMat
Xte <- d3_pivots_default$split1$teMat
Ytr <- factor(d3_outcomes$split1$trdf$death)
Yte <- factor(d3_outcomes$split1$tedf$death)




Xtr1 <- Xtr[, 1]
Xte1 <- Xte[, 1]

Xtr1 %>% head
Xte1 %>% head


feat1 <- data.frame(Xtr1, Ytr)
feat1[, 3] <- abs(feat1[, 1] - Xte1[1])


# try the interpreation her e

X <- Xtr[, c(1, 2)]
X %>% head

par(mfrow = c(1, 1))
plot(X, col = Ytr, pch = 20)
points(Xte[, c(1, 2)], col = Yte)








