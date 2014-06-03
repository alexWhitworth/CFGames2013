## Author:    Alex Whitworth
## Date:      December, 2013
## Desc:      Initial data munging of 2013 CrossFit Games data
#---------------------------------------------------------

# load data / packages / functions
#---------------------------------------------------------
setwd("C:/Users/crossfit_al1985/Documents/UCLA/213C - Applied Multivariate Analysis/2013-12 Project")
library(plyr)
library(reshape2)
library(stringr)
library(lubridate)

ath    <- read.csv("./games_competitors.csv", header= T)
scores <- read.csv("./games_events.csv", header= T)

source("./Rdata/00_functions.R")
#---------------------------------------------------------
# munging phase 1
#---------------------------------------------------------

# 1. reshape scores var to wide and tidy data for ease in step 2
#----------------------
scores <- melt(scores, id=c("rank_id", "female", "data_type"), variable.name= "event", 
                value.name= "score")
scores <- dcast(scores, value.var= "score", ... ~ data_type)

# 2. clean 3 factors that represent scores --> numeric
#----------------------
# points
scores$points <- ifelse(scores$place %in% c("CUT", "WD"), NA, scores$points)
scores$points <- as.integer(str_replace_all(scores$points, " pts", "")) # make points score into numeric

# place - save ties
scores$place <- ifelse(scores$place %in% c("CUT", "WD"), NA, scores$place)
scores$place <- str_replace_all(scores$place, "[stndrh~]", "") # remove all extraneous place characters except ties.
# extract tie scores to their own variable
scores$tie <- ifelse(str_extract(scores$place, "T"), 1, 0)
scores$tie <- ifelse(is.na(scores$tie), 0, scores$tie)
scores$place <- as.integer(str_replace_all(scores$place, "T", "")) # now remove ties from scores$place

# time - note: different metrics (time, place, lb's)
scores$time <- ifelse(scores$time == "", NA, scores$time) # place has already been munged

# 3. recast to wide data
#----------------------
ties <- scores[, c(1,2,3,7)] # save ties

# raw scores for males and females
raw_m <- dcast(scores[, -c(4:5)], value.var= "time", formula= rank_id ~ event, subset= .(female == 0))
raw_f <- dcast(scores[, -c(4:5)], value.var= "time", formula= rank_id ~ event, subset= .(female == 1))

# placing for males and females 
# place_m <- dcast(scores[, -c(5:6)], value.var= "place", formula= rank_id ~ event, subset= .(female == 0))
# place_f <- dcast(scores[, -c(5:6)], value.var= "place", formula= rank_id ~ event, subset= .(female == 1))

#---------------------------------------------------------
# munging phase 2 - raw scores --> numeric and time
  # note: need to source "00_functions.R" 
#---------------------------------------------------------
raw_m2 <- cf_convert(raw_m)
raw_m2 <- merge(raw_m2, ath[ath$female == 0, ], by= "rank_id")
raw_m2$female <- NULL; raw_m2$COMPETITOR <- NULL
raw_f2 <- cf_convert(raw_f)
raw_f2 <- merge(raw_f2, ath[ath$female == 1, ], by= "rank_id")
raw_f2$female <- NULL; raw_f2$COMPETITOR <- NULL

#---------------------------------------------------------
#### update zig zag to raw scores
# zig_m <- ath[ath$female == 0, c(1,5)] # extract raw zig-zag scores
# zig_f <- ath[ath$female == 1, c(1,5)]
# # merge
# raw_m2 <- merge(raw_m2, zig_m, by="rank_id")
# raw_f2 <- merge(raw_f2, zig_f, by="rank_id")
# 
# 
# raw_m2 <- cbind(raw_m2[, c(1:5)], raw_m2[, 14], raw_m2[, c(6:13)])
# raw_f2 <- cbind(raw_f2[, c(1:5)], raw_f2[, 14], raw_f2[, c(6:13)])
# names(raw_f2)[6] <- "zigzag_raw"
#---------------------------------------------------------

## check correlations
cor(raw_m2[,-c(1)], use="pairwise.complete.obs")
cor(raw_f2[,-c(1)], use="pairwise.complete.obs")

## create standardized scores
std_m <- as.data.frame(cbind(raw_m2[,c(1,14,6)],apply(raw_m2[,-c(1,6,14)], 2, scale_norm, useNA=F)))
std_f <- as.data.frame(cbind(raw_f2[,c(1,14,6)],apply(raw_f2[,-c(1,6,14)], 2, scale_norm, useNA=F)))
# invert C&J scors to match rest
std_m$c_and_j <- std_m$c_and_j * -1
std_f$c_and_j <- std_f$c_and_j * -1

## check mean deviation rankings
cbind(std_m[,1], apply(std_m[, -c(1:2)], 1, sum, na.rm=T))
cbind(std_f[,1], apply(std_f[, -c(1:2)], 1, sum, na.rm=T))

# excluding zigzag sprint
cbind(std_m[,1], apply(std_m[, -c(1:2,7)], 1, sum, na.rm=T))
cbind(std_f[,1], apply(std_f[, -c(1:2,7)], 1, sum, na.rm=T))

#---------------------------------------------------------
# clean up intermediate dataset
rm(raw_m, raw_f, scores, cf_convert, scale_norm, zig_m, zig_f)
# save data
#---------------------------------------------------------
save.image("./Rdata/std_CF_games.Rdata")
#---------------------------------------------------------

# useful functions
#---------------------------------------------------------
# tapply(vector, list(x1, x2...), function, ...)