## Author:    Alex Whitworth
## Date:      December, 2013
## Desc:      Summary statistics and missing value imputation of the CF Games data
        # 1. Summary statistics (raw and std)
        # 2. Predict events 11 (Cinco 1) and 12 (Cinco 2) - via LM and KNN(k= 5)
        # 3. Create correlation matrices for men and women
        # 4. Save data
#---------------------------------------------------------
# load data / packages / functions
#---------------------------------------------------------
library(xtable)
library(imputation)
setwd("C:/Users/crossfit_al1985/Documents/UCLA/213C - Applied Multivariate Analysis/2013-12 Project")
load("./RData/std_cf_games.Rdata")
#---------------------------------------------------------
source("./Rdata/00_functions.R")
#---------------------------------------------------------

# 1. Summary statistics tabulations
#---------------------------------------------------------
raw_m2 <- raw_m2[raw_m2$rank_id <= 44, ] # drop 2 athletes with WD (Mikko S, Michael M., Brandon S.)
raw_f2 <- raw_f2[raw_f2$rank_id < 44, ] # drop 1 athletes with WD (Heather G.)

# create tabulations
    # (avg2, st.dev2, min, max) are in minutes for timed events, lb's for C&J, and raw sec for sprint
    # for standardized tabulations, values are in standard deviations
m_raw_tab <- tab_games(raw_m2[, -c(1,6,14)]) 
f_raw_tab <- tab_games(raw_f2[, -c(1,6,14)]) 
m_std_tab <- tab_games(std_m[, -c(1:3)], std=T)[, c(1,4:5)]
f_std_tab <- tab_games(std_f[, -c(1:3)], std=T)[, c(1,4:5)]

# output summary statistics for LaTeX
xtable(cbind(m_raw_tab, m_std_tab[, c(2:3)]))
xtable(cbind(f_raw_tab, f_std_tab[, c(2:3)]))

#---------------------------------------------------------
# 2. predict Cinco 1 & 2 for missing values
#---------------------------------------------------------
## Imputation method = LM
#---------------------------------------------------------
f1 <- cinco_1 ~ pool + row_1 + row_2 + burden_run + zigzag_time + legless + naughty_nancy + c_and_j + X2007 + sprint_chipper
f2 <- cinco_2 ~ pool + row_1 + row_2 + burden_run + zigzag_time + legless + naughty_nancy + c_and_j + X2007 + sprint_chipper
# men - cinco 1
lm1.m <- step(lm(f1, raw_m2), direction= "backward")
# men - cinco 2
lm2.m <- step(lm(f2, raw_m2), direction= "backward")
# women - cinco 1
lm1.f <- step(lm(f1, raw_f2), direction= "backward")
# women - cinco 2
lm2.f <- step(lm(f2, raw_f2), direction= "backward")
#-----------------------
# predict
#-----------------------
m_cinco1 <- round(c(raw_m2$cinco_1[1:30],
              predict(lm1.m, newdata= raw_m2[c(31:44),])), 1)
m_cinco2 <- round(c(raw_m2$cinco_2[1:30],
              predict(lm2.m, newdata= raw_m2[c(31:44),])), 1)
f_cinco1 <- round(c(raw_f2$cinco_1[1:30],
              predict(lm1.f, newdata= raw_f2[c(31:43),])), 1)
f_cinco2 <- round(c(raw_f2$cinco_2[1:30],
              predict(lm2.f, newdata= raw_f2[c(31:43),])), 1)
# combine actual and predicted scores - men and women
raw_men_lm <- cbind(raw_m2[, c(1,14,2:5,15,7:11)], m_cinco1, m_cinco2)
raw_fem_lm <- cbind(raw_f2[, c(1,14,2:5,15,7:11)], f_cinco1, f_cinco2)

#---------------------------------------------------------
## Imputation method = K Nearest Neighbor
#---------------------------------------------------------
men_knn <- raw_m2[,c(2:5, 7:13, 15)]
fem_knn <- raw_f2[,c(2:5, 7:13, 15)]
raw_men_knn <- kNNImpute(men_knn, k= 5)[[1]]
raw_fem_knn <- kNNImpute(fem_knn, k= 5)[[1]]
################ Note: ################
# kNNImpute() is not accurately imputing scores for the last 4 athletes for men and female.
# I have done so manually with the below code:
#######################################
source("./Rdata/02A_knn_impute_errors.R")
#---------------------------------------------------------
# 3. create Covariance (corr) matrices for PCA / Factor analysis 
#---------------------------------------------------------
## Corr / Cov for raw data - 10 events (exc. Cinco 1,2)
raw_m2_data <- raw_m2[, c(2:5, 15, 7:11)]
raw_f2_data <- raw_f2[, c(2:5, 15, 7:11)]
cor_raw_m10 <- cor(raw_m2_data)
cor_raw_f10 <- cor(raw_f2_data)
## Corr / Cov for data imputed using LM (all 12 events)
cor_imp_lm_m <- cor(raw_men_lm[, c(3:14)])
cor_imp_lm_f <- cor(raw_fem_lm[, c(3:14)])
## Corr / Cov for data imputed using KNN (all 12 events)
cor_imp_knn_m <- cor(raw_men_knn)
cor_imp_knn_f <- cor(raw_fem_knn)

## clean up
library(gdata) # needed for keep()
keep(cor_raw_m10, cor_raw_f10, cor_imp_lm_m, cor_imp_lm_f, cor_imp_knn_m, cor_imp_knn_f, 
     raw_m2_data, raw_f2_data, raw_men_lm, raw_fem_lm, raw_men_knn, raw_fem_knn,
     sure= T)
#---------------------------------------------------------
# 4. Save ouput
#---------------------------------------------------------
save.image("./RData/imputed_covariances.Rdata")