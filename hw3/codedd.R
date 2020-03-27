library('dummies')
library('ramify')
library('readxl')
setwd("/Users/diego/Documents/GitHub/Unsupervised-ML/hw3")
df_sample <- read_xlsx("df_sample.xlsx")
df_sample <- df_sample[,!(names(df_sample) %in% c('W_FSTUWT'))]

df_sample <- as.data.frame(apply(df_sample, 2, as.numeric))  # Convert all variable types to numeric

df_sample <- as.data.frame(apply(df_sample, 2,
                     function(x) (x - min(x)) / (max(x) - min(x))))


kmeans <- function(dat, k ,eps){
  p <- length(dat)
  n <- length(dat[[1]])
  groups <- sample(1:k, n, replace=TRUE)
  dums <- dummy(groups)
  centers <- c()
  for (i in (1:k)){
    centers <- rbind(centers, colSums(dat * dums[,i])/sum(dums[,i]) )
  }
  conv <- 100000
  while (conv > eps){
    dist <- c()
    for (i in (1:k)){
      dist <- cbind(dist, rowSums((dat-centers[i,])^2))
    }
    groups <- argmin(dist, rows=TRUE)
    dums <- dummy(groups)
    new_centers <- c()
    for (i in (1:k)){
      new_centers <- rbind(new_centers, colSums(dat * dums[,i])/sum(dums[,i]) )
    }
    conv <- sum(rowSums((centers-new_centers)^2))
    centers <- new_centers
  }
  return(list(centers, cbind(dat,groups)))
}


res <- kmeans(df_sample, 4, 1)
dat1 <- res[[2]]



