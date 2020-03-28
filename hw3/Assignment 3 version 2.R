# *------------------------------------------------------------------
# Unsupervised Machine Learning - Homework 3
# *----------------------------------------------------------------

# Clear cache
rm(list = ls())

library('dummies')
library('ramify')
library('readxl')

#setwd("/Users/diego/Documents/GitHub/Unsupervised-ML/hw3")
df_sample <- read_xlsx("/Users/gabrielaszini/Documents/Unsupervised ML/Assignment 3/df_sample.xlsx")

# Leaving out the weights
df_sample <- df_sample[,!(names(df_sample) %in% c('W_FSTUWT'))]
# Convert all variable types to numeric
df_sample <- as.data.frame(apply(df_sample, 2, as.numeric))
# Scaling
df_sample <- as.data.frame(apply(df_sample, 2,
                                 function(x) (x - min(x)) / (max(x) - min(x))))

p = length(df_sample)
k = 2
centers = as.matrix(replicate(p, runif(k)))


better_kmeans <- function(dat, centers, eps, it_max){
  # Computes kmeans clusters on dat (dataframe)
  # with initial centers given by centers
  k <- dim(centers)[1]
  n <- length(dat[[1]])
  dist <- c()
  for (i in (1:k)){
    dist <- cbind(dist, rowSums((dat-centers[i,])^2))
  }
  groups <- argmin(dist, rows=TRUE)
  conv <- 100000
  iters <- 1
  while (conv > eps && iters <= it_max){
    # Shift initial center to the mean of the the observations in group
    centers_old = centers
    dums <- dummy(groups)
    for (i in (1:k)){
      centers <- rbind(centers, colSums(dat * dums[,i])/sum(dums[,i]) )
    }
    groups_old = groups 
    dist <- c()
    for (i in (1:k)){
      dist <- cbind(dist, rowSums((dat-centers[i,])^2))
    }
    groups <- argmin(dist, rows=TRUE)
    conv <- sum(abs(groups-groups_old))
    # Update variables
    print(iters)
    print(conv)
    iters <- iters+1
    
  }
  return(list(centers, cbind(dat,groups)))
}

res_pack <- kmeans(df_sample, centers, iter.max=10, algorithm="MacQueen")
res <- better_kmeans(df_sample, centers, 0.00001, 10)
dat1 <- res[[2]]

res[[1]]
res_pack$centers