## Author:    Alex Whitworth
## Date:      December, 2013
## Desc:      Compute factor scores for each athlete. Create plots.
#---------------------------------------------------------
# load data / packages / functions
#---------------------------------------------------------
library(xtable)
library(gdata)
library(ggplot2)
setwd("C:/Users/crossfit_al1985/Documents/UCLA/213C - Applied Multivariate Analysis/2013-12 Project")
#---------------------------------------------------------
load("./RData/final_data.Rdata")
keep(raw_men_knn, raw_fem_knn, m.avg, f.avg, sure= T)
load("./RData/std_cf_games.Rdata")
# source("./RData/00_functions.R")

std_m <- std_m[std_m$rank_id <= 44, ]
std_f <- std_f[std_f$rank_id < 44, ]

# 01. Create Standardized data 
#---------------------------------------------------------
m.mu <- apply(raw_m2, 2, mean, na.rm= T)
f.mu <- apply(raw_m2, 2, mean, na.rm= T)
m.sd <- apply(raw_m2, 2, sd, na.rm= T)
f.sd <- apply(raw_f2, 2, sd, na.rm= T)

std_m$cinco_1[31:44] <- (raw_men_knn$cinco_1[31:44] - m.mu[12]) / m.sd[12]
std_m$cinco_2[31:44] <- (raw_men_knn$cinco_2[31:44] - m.mu[13]) / m.sd[13]
std_f$cinco_1[31:43] <- (raw_fem_knn$cinco_1[31:43] - m.mu[12]) / m.sd[12]
std_f$cinco_2[31:43] <- (raw_fem_knn$cinco_2[31:43] - m.mu[13]) / m.sd[13]

# 02. Factor Scores
#---------------------------------------------------------
std_m <- std_m[, c(1:7, 15, 8:14)]
std_m$c_and_j <- std_m$c_and_j * -1
std_f <- std_f[, c(1:7, 15, 8:14)]
std_f$c_and_j <- std_f$c_and_j * -1

m.fact.scores <- as.data.frame(
  matrix(data= NA, nrow= 44, ncol= 3,
         dimnames= list(ath[ath$female == 0 & ath$rank_id <= 44, 4],
                        c("Factor 1", "Factor 2", "Factor 3"))))

m.fact.scores[, 1] <- as.matrix(std_m[, c(4:15)]) %*% as.vector(m.avg[, 1])
m.fact.scores[, 2] <- as.matrix(std_m[, c(4:15)]) %*% as.vector(-m.avg[, 2])
m.fact.scores[, 3] <- as.matrix(std_m[, c(4:15)]) %*% as.vector(m.avg[, 3])

f.fact.scores <- as.data.frame(
  matrix(data= NA, nrow= 43, ncol= 2,
         dimnames= list(ath[ath$female == 1 & ath$rank_id < 44, 4],
                        c("Factor 1", "Factor 2"))))

f.fact.scores[, 1] <- as.matrix(std_f[, c(4:15)]) %*% as.vector(f.avg[, 1])
f.fact.scores[, 2] <- as.matrix(std_f[, c(4:15)]) %*% as.vector(f.avg[, 2])

# 03. plotting
#---------------------------------------------------------
# women
# build labels
w.names <- paste(c(1:43), c(rownames(f.fact.scores)[1:10], rep("",33)), sep= ". ")

png("./written report/figures/fem_fact_scores.png", width= 600, height= 500, units= "px")
ggplot(f.fact.scores, aes(x=f.fact.scores[,1], y= f.fact.scores[,2])) + geom_point(colour= "red") + 
  geom_text(aes(label= w.names), 
            angle= 15, size= 5, hjust= -.1, vjust= 0.05) +
  labs(title= "Athlete Factor Scores \n Female Competitors",
       x= "Factor 1", y= "Factor 2") + 
  theme(axis.title= element_text(size= 14, face= "bold"),
        axis.text= element_text(size= 12),
        title= element_text(size= 16, face= "bold.italic"))
dev.off()
# men
# build labels
m.names <- paste(c(1:44), c(rownames(m.fact.scores)[1:10], rep("",34)), sep= ". ")

png("./written report/figures/men_fact_scores_1_2.png", width= 600, height= 500, units= "px")
ggplot(m.fact.scores, aes(x=m.fact.scores[,1], y= m.fact.scores[,2])) + geom_point(colour= "red") + 
  geom_text(aes(label= m.names), 
            angle= 15, size= 5, hjust= -.1, vjust= 0.1) +
  labs(title= "Athlete Factor Scores \n Male Competitors",
       x= "Factor 1", y= "Factor 2") + 
  theme(axis.title= element_text(size= 14, face= "bold"),
        axis.text= element_text(size= 12),
        title= element_text(size= 16, face= "bold.italic"))
dev.off()

png("./written report/figures/men_fact_scores_1_3.png", width= 600, height= 500, units= "px")
ggplot(m.fact.scores, aes(x=m.fact.scores[,1], y= m.fact.scores[,3])) + geom_point(colour= "red") + 
  geom_text(aes(label= m.names), 
            angle= 15, size= 5, hjust= -.1, vjust= 0.1) +
  labs(title= "Athlete Factor Scores \n Male Competitors",
       x= "Factor 1", y= "Factor 3") + 
  theme(axis.title= element_text(size= 14, face= "bold"),
        axis.text= element_text(size= 12),
        title= element_text(size= 16, face= "bold.italic"))
dev.off()

png("./written report/figures/men_fact_scores_2_3.png", width= 600, height= 500, units= "px")
ggplot(m.fact.scores, aes(x=m.fact.scores[,2], y= m.fact.scores[,3])) + geom_point(size= 3, colour= "red") + 
  geom_text(aes(label= m.names), 
            angle= 15, size= 5, hjust= .8, vjust= -0.2) +
  labs(title= "Athlete Factor Scores \n Male Competitors",
       x= "Factor 2", y= "Factor 3") + 
  theme(axis.title= element_text(size= 14, face= "bold"),
        axis.text= element_text(size= 12),
        title= element_text(size= 16, face= "bold.italic"))
dev.off()

# 04. Save
#---------------------------------------------------------
save.image("./Rdata/factor_scores.Rdata")