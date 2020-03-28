library('dummies')
library('ramify')
library('readxl')
library('flexclust')

setwd("/Users/diego/Documents/GitHub/Unsupervised-ML/hw3")
df_sample <- read_xlsx("df_sample.xlsx")

# Leaving out the weights
df_sample <- df_sample[,!(names(df_sample) %in% c('ST004D01T','BSMJ','W_FSTUWT','CNT','SCIEEFF'))]
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
  conv <- 100000
  centers_old <- centers
  iters <- 1
  while (conv > eps && iters < 26){
    dist <- c()
    # Compute the distance to each center
    for (i in (1:k)){
      dist <- cbind(dist, sqrt(rowSums((dat-t(replicate(n,centers_old[i,])))^2)))
    }
    # Assign groups
    groups <- argmin(dist, rows=TRUE)
    dums <- dummy(groups)
    # Compute new centers
    new_centers <- c()
    for (i in (1:k)){
      new_centers <- rbind(new_centers, colSums(dat * dums[,i])/sum(dums[,i]) )
    }
    # Check convergence
    conv <- sqrt(sum((new_centers-centers_old)^2))
    # Update variables
    centers_old <- new_centers
    iters <- iters+1
  }
  return(list(new_centers, cbind(dat,groups)))
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

centers <- rand_centers(df_sample,4)
res_pack <- kcca(df_sample, centers)
res <- better_kmeans(df_sample, centers, 0.001)
dat1 <- res[[2]]

res[[1]]
res_pack@centers







