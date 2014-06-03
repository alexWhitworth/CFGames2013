## Author:    Alex Whitworth
## Date:      Fall Quarter 2013
## Desc:      Functions needed for MGMT 213 Crossfit Games analysis
#------------------------------------------------------------------


scale_norm <- function(vec, scale.factor= 1, useNA=T) {
  # normalize a vector
  # useNA - defaults to not remove NA values. To remove, useNA= F
  vec2 <- (vec - mean(vec, na.rm=ifelse(useNA == F, T, F))) / (scale.factor * sd(vec, na.rm=ifelse(useNA == F, T, F)))
  return(vec2)
}

cf_convert <- function(cf_scores) {
  # This function takes an input of 2013 Crossfit Games scores. It cleans the raw character scores to
  # numeric scores: durations (for timed events), place for zigzag sprint, and pounds for C&J.  
  # note: rank_id = column 1; zigzag = column 6; C&J = column 9
  #-----------------------------------------------------------
  
  ## 1. extract numeric value from clean and jerk. 
  # If completed jerk, add 5 pounds to score (1/2 increment),
  # otherwise truncate: athlete couldn't get clean at next weight
  c_j <- as.numeric(str_replace_all(cf_scores$clean_and_jerk, " lb", ""))
  c_and_j <- ifelse(as.integer(c_j * 10) %% 10 == 2, trunc(c_j)+ 5, trunc(c_j)) 
  ## 2. zig zag raw scores are just places. 
  zigzag <- as.integer(str_replace_all(cf_scores$zigzag_sprint, " pt", ""))
  ## 3. extract durations for timed events
  cf_2 <- as.data.frame(apply(cf_scores[,-c(1,6,9)], 2, function(x) as.duration(ms(x)))) 
  cf_2$row_2 <- cf_2$row_2 + 3600 # add 1 hour to row2 time, data input error
  ## 4. recombine and return
  cf_scores2 <- cbind(cf_scores[,1], cf_2[,1:4], zigzag, cf_2[, 5:6], c_and_j, cf_2[,7:10])
  names(cf_scores2)[1] <- "rank_id"
  return(cf_scores2)
}

tab_games <- function(cf, std=F) {
  # DESC: basic tabulations of the CF games data
  # inputs: matrix of CF games data
  # STD = T if scores are standardized, =F if raw scores
  
  # internals
  ath.cnt <- function(x) sum(!is.na(x)) # needed for accurate count (given NA's)
  adj_fun <- function(x, fun) {
    a <- apply(x, 2, fun, na.rm=T)
    if (std == F) {
      a2 <- c(a[1:6]/60, a[7], a[8:11]/60, a[12])
    } else {
      a2 <- a
    }
    return(a2)
  }
  # tabulate
  ath <- apply(cf, 2, ath.cnt)
  avg <- round(apply(cf, 2, mean, na.rm= T), 2)
  avg2 <- round(
    if (std == F) {
      a2 <- c(avg[1:6]/60, avg[7], avg[8:11]/60, avg[12])
    } else {
      a2 <- avg
    }
    , 2)
  st.dev <- round(apply(cf, 2, sd, na.rm= T), 2)
  st.dev2 <- round(
    if (std == F) {
      a2 <- c(st.dev[1:6]/60, st.dev[7], st.dev[8:11]/60, st.dev[12])
    } else {
      a2 <- st.dev
    }
    , 2)
  min_ <- round(adj_fun(cf, min), 2)
  max_ <- round(adj_fun(cf, max), 2)
  # combine and return
  tab_g <- cbind(ath, avg2, st.dev2, min_, max_)
  return(tab_g)
}