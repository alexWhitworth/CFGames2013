impute.fn <- function(scores, distances, raw_dist) {
  knn.values <- scores[c(as.integer(names(distances)))]
  knn.weights <- 1 - (distances / max(raw_dist))
  weighted.mean(knn.values, knn.weights)
}

# men - impute errors
#---------------------------------------------------------
x.dist <- as.matrix(dist(raw_m2[,c(2:5, 7:13, 15)]))
dist_44 <- x.dist[44, c(1:30)][order(x.dist[44, c(1:30)])]
dist_43 <- x.dist[43, c(1:30)][order(x.dist[43, c(1:30)])]
dist_42 <- x.dist[42, c(1:30)][order(x.dist[42, c(1:30)])]
dist_41 <- x.dist[41, c(1:30)][order(x.dist[41, c(1:30)])]

# inpute cinco 1
raw_men_knn$cinco_1[41] <- impute.fn(raw_m2$cinco_1, dist_41[1:5], dist_41)
raw_men_knn$cinco_1[42] <- impute.fn(raw_m2$cinco_1, dist_42[1:5], dist_42)
raw_men_knn$cinco_1[43] <- impute.fn(raw_m2$cinco_1, dist_43[1:5], dist_43)
raw_men_knn$cinco_1[44] <- impute.fn(raw_m2$cinco_1, dist_44[1:5], dist_44)
# impute cinco 2
raw_men_knn$cinco_2[41] <- impute.fn(raw_m2$cinco_2, dist_41[1:5], dist_41)
raw_men_knn$cinco_2[42] <- impute.fn(raw_m2$cinco_2, dist_42[1:5], dist_42)
raw_men_knn$cinco_2[43] <- impute.fn(raw_m2$cinco_2, dist_43[1:5], dist_43)
raw_men_knn$cinco_2[44] <- impute.fn(raw_m2$cinco_2, dist_44[1:5], dist_44)

# round scores to match
raw_men_knn$cinco_1 <- round(raw_men_knn$cinco_1, 1)
raw_men_knn$cinco_2 <- round(raw_men_knn$cinco_2, 1)

# women - impute errors
#---------------------------------------------------------
x.dist <- as.matrix(dist(raw_f2[,c(2:5, 7:13, 15)]))
dist_43 <- x.dist[43, c(1:30)][order(x.dist[43, c(1:30)])]
dist_42 <- x.dist[42, c(1:30)][order(x.dist[42, c(1:30)])]
dist_41 <- x.dist[41, c(1:30)][order(x.dist[41, c(1:30)])]
dist_40 <- x.dist[40, c(1:30)][order(x.dist[40, c(1:30)])]

# inpute cinco 1
raw_fem_knn$cinco_1[40] <- impute.fn(raw_f2$cinco_1, dist_40[1:5], dist_40)
raw_fem_knn$cinco_1[41] <- impute.fn(raw_f2$cinco_1, dist_41[1:5], dist_41)
raw_fem_knn$cinco_1[42] <- impute.fn(raw_f2$cinco_1, dist_42[1:5], dist_42)
raw_fem_knn$cinco_1[43] <- impute.fn(raw_f2$cinco_1, dist_43[1:5], dist_43)
# impute cinco 2
raw_fem_knn$cinco_2[40] <- impute.fn(raw_f2$cinco_2, dist_40[1:5], dist_40)
raw_fem_knn$cinco_2[41] <- impute.fn(raw_f2$cinco_2, dist_41[1:5], dist_41)
raw_fem_knn$cinco_2[42] <- impute.fn(raw_f2$cinco_2, dist_42[1:5], dist_42)
raw_fem_knn$cinco_2[43] <- impute.fn(raw_f2$cinco_2, dist_43[1:5], dist_43)

# round scores to match
raw_fem_knn$cinco_1 <- round(raw_fem_knn$cinco_1, 1)
raw_fem_knn$cinco_2 <- round(raw_fem_knn$cinco_2, 1)