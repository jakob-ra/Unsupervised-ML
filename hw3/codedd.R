library('dummies')
library('ramify')
library('readxl')

setwd("/Users/diego/Documents/GitHub/Unsupervised-ML/hw3")
df_sample <- read_xlsx("df_sample.xlsx")

# Leaving out the weights
df_sample <- df_sample[,!(names(df_sample) %in% c('W_FSTUWT'))]
# Convert all variable types to numeric
df_sample <- as.data.frame(apply(df_sample, 2, as.numeric))
# Scaling
df_sample <- as.data.frame(apply(df_sample, 2,
                     function(x) (x - min(x)) / (max(x) - min(x))))

better_kmeans <- function(dat, centers, eps){
  # Computes kmeans clusters on dat (dataframe)
  # with initial centers given by centers
  k <- dim(centers)[1]
  n <- length(dat[[1]])
  groups_old <- sample(1:k, n, replace=TRUE)
  conv <- 100000
  centers_old <- centers
  iters <- 1
  while (conv > eps && iters < 10){
    dist <- c()
    # Compute the distance to each center
    for (i in (1:k)){
      dist <- cbind(dist, rowSums((dat-centers_old[i,])^2))
    }
    # Assign groups
    groups <- argmin(dist, rows=TRUE)
    dums <- dummy(groups)
    # Compute new centers
    new_centers <- c()
    for (i in (1:k)){
      new_centers <- rbind(new_centers, colSums(dat * dums[,i])/sum(dums[,i]) )
    }
    # Check if any single switch decrease the objective
    for (i in (1:n)){
      for (j in (1:k)){
        temp_groups <- groups
        temp_groups[i] <- j
        temp_dums <- dummy(groups)
        temp_centers <- c()
        for (i in (1:k)){
          temp_centers <- rbind(temp_centers, colSums(dat * temp_dums[,i])/sum(temp_dums[,i]) )
        }
        temp_dist <- c()
        for (i in (1:k)){
          temp_dist <- cbind(dist, rowSums((dat-temp_centers[i,])^2))
        }
        if (sum(dist) > sum(temp_dist)){
          groups <- temp_groups
          new_centers <- temp_centers
        }
      }
    }
    # Test convergence
    # By change in groups assignments
    conv <- sum(abs(groups-groups_old))
    # Update variables
    groups_old <- groups
    centers_old <- new_centers
    iters <- iters+1
  }
  return(list(centers, cbind(dat,groups)))
}

rand_centers <- function(dat,k){
  # Returns centers based on assinging randomly
  # the data to k groups and averaging them
  p <- length(dat)
  n <- length(dat[[1]])
  groups <- sample(1:k, n, replace=TRUE)
  dums <- dummy(groups)
  centers <- c()
  for (i in (1:k)){
    centers <- rbind(centers, colSums(dat * dums[,i])/sum(dums[,i]) )
  }
  return(centers)
}

centers <- rand_centers(df_sample,2)
res_pack <- kmeans(df_sample, centers, iter.max=10)
res <- better_kmeans(df_sample, centers, 0.00001)
dat1 <- res[[2]]

res[[1]]
res_pack$centers
