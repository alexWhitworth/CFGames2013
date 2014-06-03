## Author:    Alex Whitworth
## Date:      December, 2013
## Desc:      Principle component and factor analysis for CF Games Data. 
          # 1. PCA to determine number of factors.
          # 2A. Factor analysis on each model
          # 2B. AIC for each model
          # 3. Model averaging
          # 4. Save output
#---------------------------------------------------------
# load data / packages / functions
#---------------------------------------------------------
library(xtable)
library(GPArotation)
setwd("C:/Users/crossfit_al1985/Documents/UCLA/213C - Applied Multivariate Analysis/2013-12 Project")
load("./RData/imputed_covariances.Rdata")
#---------------------------------------------------------
# source("./Rdata/00_functions.R")
#---------------------------------------------------------

# 1. PCA of all data sets for male and female atheletes
#---------------------------------------------------------
# 10 event scores
pca_m10 <- eigen(cor_raw_m10) ## first 3 - 59.2%; 4- 69.6% (4)
pca_f10 <- eigen(cor_raw_f10) ## first 3 - 64.2%; 4- 73.6% (3)
# LM imputed scores
  # raw_men_lm[, c(3:14)]
pca_mLM <- eigen(cor_imp_lm_m) ## first 3 - 62.9%; 4- 71.9% (use 4) 
pca_fLM <- eigen(cor_imp_lm_f) ## first 3 - 65.7%; 4- 73.9% (use 3) 
# KNN imputed scores
  # raw_men_knn[, c(1:4,12,5:11)] 
pca_mKNN <- eigen(cor_imp_knn_m) ## first 3 - 55.7%; 4- 64.8% (use 4) 
pca_fKNN <- eigen(cor_imp_knn_f) ## first 3 - 60.7%; 4- 69.7% (use 4) 

#### How many factors (2-4)? ####
  ## 2 ~ 50% variance
  ## 3 ~ 60-65% variance
  ## 4 ~ 70% variance

# 2A. Factor Analysis of all data sets for male and female atheletes
#---------------------------------------------------------
# rotation options: "none", "varimax", "oblimin"
fa4_m10 <- factanal(covmat= cor_raw_m10, factors= 3, rotation= "oblimin")
fa4_f10 <- factanal(covmat= cor_raw_f10, factors= 2, rotation= "oblimin")

fa4_mLM <- factanal(covmat= cor_imp_lm_m, factors= 3, rotation= "oblimin")
fa4_fLM <- factanal(covmat= cor_imp_lm_f, factors= 2, rotation= "oblimin")

fa4_mKNN <- factanal(covmat= cor_imp_knn_m, factors= 3, rotation= "oblimin")
fa4_fKNN <- factanal(covmat= cor_imp_knn_m, factors= 2, rotation= "oblimin")

# 2B. AIC for each model
#---------------------------------------------------------
aic <- list() # [[1]] is mens scores, [[2]] is females
# male model AIC
aic[1][1] <- -2 * log(fa4_m10$criteria[1]) + 2 * (12*13/2 - 2)
aic[[1]][2] <- -2 * log(fa4_mLM$criteria[1]) + 2 * (12*13/2)
aic[[1]][3] <- -2 * log(fa4_mKNN$criteria[1]) + 2 * (12*13/2)
# female model AIC
aic[2][1] <- -2 * log(fa4_f10$criteria[1]) + 2 * (12*13/2 - 2)
aic[[2]][2] <- -2 * log(fa4_fLM$criteria[1]) + 2 * (12*13/2)
aic[[2]][3] <- -2 * log(fa4_fKNN$criteria[1]) + 2 * (12*13/2)

# 3. Model Averaging
#---------------------------------------------------------
# weights
w.func <- function(j) {
  w <- vector()
  for (i in 1:3) {
      w[i] <- exp(-1/2*aic[[j]][i]) / (exp(-1/2*aic[[j]][1]) + exp(-1/2*aic[[j]][2]) + exp(-1/2*aic[[j]][3]))
    }
  return(w)
}
w.m <- w.func(j= 1)
w.f <- w.func(j= 2)

# scores
m1 <- as.matrix(round(rbind(fa4_m10$loadings[, c(1:3)], rep(0,3), rep(0,3)), 3))
m2 <- as.matrix(round(fa4_mLM$loadings[,c(1:3)], 3))
m3 <- as.matrix(round(fa4_mKNN$loadings[c(1:4, 12,5:11), c(1:3)], 3))

f1 <- as.matrix(round(rbind(fa4_f10$loadings[,c(1:2)], rep(0,2), rep(0,2)), 3))
f2 <- as.matrix(round(fa4_fLM$loadings[,c(1:2)], 3))
f3 <- as.matrix(round(fa4_fKNN$loadings[c(1:4, 12,5:11), c(1:2)], 3))

# model averages --- currently this doesn't  make sense as factors are diffent
# define
m.avg <- matrix(nrow= 12, ncol= 3, dimnames=dimnames(m3))
f.avg <- matrix(nrow= 12, ncol= 2, dimnames=dimnames(f3))
# compute averages - male
m.avg[, 1] <- round(m1[, 1] * w.m[1] + m2[, 3] * w.m[2] + m3[, 2] * w.m[3], 3)
m.avg[, 2] <- round(m1[, 2] * w.m[1] + m2[, 2] * w.m[2] - m3[, 1] * w.m[3], 3)
m.avg[, 3] <- round(m1[, 3] * w.m[1] + m2[, 1] * w.m[2] + m3[, 3] * w.m[3], 3)
# compute averages - female
f.avg[, 1] <- round(f1[, 1] * w.f[1] + f2[, 2] * w.f[2] + f3[, 1] * w.f[3] , 3)
f.avg[, 2] <- round(f1[, 2] * w.f[1] + f2[, 1] * w.f[2] + f3[, 2] * w.f[3] , 3)


m.avg.unique <- round(w.m[1] * c(fa4_m10$uniquenesses,1,1) + 
                      w.m[2] * fa4_mLM$uniquenesses + 
                      w.m[3] * fa4_mKNN$uniquenesses[c(1:4, 12, 5:11)], 3)
names(m.avg.unique) <- dimnames(m3)[[1]]

f.avg.unique <- round(w.f[1] * c(fa4_f10$uniquenesses,1,1) + 
                      w.f[2] * fa4_fLM$uniquenesses + 
                      w.f[3] * fa4_fKNN$uniquenesses[c(1:4, 12, 5:11)], 3)
names(f.avg.unique) <- dimnames(f3)[[1]]

# 4. Save
#---------------------------------------------------------
save.image("./RData/final_data.Rdata")
#---------------------------------------------------------