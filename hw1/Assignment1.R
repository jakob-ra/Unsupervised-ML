# *------------------------------------------------------------------
# Unsupervised Machine Learning - Homework 1
# *----------------------------------------------------------------

# Clear cache
rm(list = ls())
library(PMA)

# Load the data
load('/Users/gabrielaszini/Documents/Unsupervised\ ML/Assignment\ 1/FIFA2017_NL.RData')

fifa = subset(fifa, select = -name)
fifa = subset(fifa, select = -Position)
fifa = subset(fifa, select = -eur_value)
fifa = subset(fifa, select = -eur_wage)
fifa = subset(fifa, select = -eur_release_clause)
fifa = subset(fifa, select = -club)
fifa = as.data.frame(fifa)
########################################### Normal PCA #################################################

# PCA from package
fifa_pca_cor = prcomp(fifa, scale=TRUE)
fifa_pca_cor
summary(fifa_pca_cor)
plot(fifa_pca_cor, type="l")
biplot(fifa_pca_cor)

############################################ Sparse PCA ################################################

#Setting data to be in matrix form and scaling columns
fifa = as.matrix(fifa)
fifa = scale(fifa, center = TRUE, scale = TRUE)

#package results for 1 principal component
sparse_PCA_package_1 = SPC(fifa, sumabs = 2, K=1, center = FALSE, trace = FALSE)
sparse_PCA_package_1$d
sparse_PCA_package_1$v
sparse_PCA_package_1$u

# Defining plus function to be used in algorithm for sparse PCA
plusfunct <- function(x) {
  result = matrix(,ncol=1, nrow=length(x))
  for(i in 1:length(x)){
  if (x[i] > 0) {
    result[i] <- x[i]
  }
  else {
    result[i] <- 0
  }
  }
  return(result)
}

# Defining function for our sparse PCA for 1 principal component
sparse_PCA_1_vector = function(c_2, i_max, v, X, seq_lambda_2){
for(i in 1:i_max){
  u = X%*%v/norm(X%*%v, type="2")
  v_check = vector(mode = "list", length = length(seq_lambda_2))
  lambda_2_check = matrix(,ncol=2, nrow= length(seq_lambda_2))
  k = 1
  for (j in seq_lambda_2){
    S = sign(t(X)%*%u)*plusfunct((abs(t(X)%*%u)-j))
    # get the L2 norm
    v_check[[k]] = S/(sqrt(sum(S^2)))
    lambda_2_check[k,1] = k
    lambda_2_check[k,2] = norm(v_check[[k]], type="1")
    k = k+1
  }
  lambda_2_check = lambda_2_check[lambda_2_check[,2]<= c_2,]
  chosen_k = lambda_2_check[1,1]
  v = v_check[[chosen_k]]
}
  return(list(u,v))
}

# Setting chosen parameters
c_2 = 2
i_max = 150
v = svd(fifa)$v[,1] #initialize v
X = fifa
seq_lambda_2 = seq(0,35, 0.01)

# Calling function
sparse_PCA_our_1 = sparse_PCA_1_vector(c_2, i_max, v, X, seq_lambda_2)
sparse_PCA_our_1[[2]]

# Package results for 2 principal components
sparse_PCA_package_2 = SPC(fifa, sumabs = 2, K=2, center = FALSE, trace = FALSE)
sparse_PCA_package_2$d
sparse_PCA_package_2$v
sparse_PCA_package_2$u

# Function for obtaining more than one principal component (namely K)
sparse_PCA_more_vec = function(K,c_2, i_max, v, X, seq_lambda_2){
  u_res = vector(mode = "list", length = K)
  v_res = vector(mode = "list", length = K)
  sigma = vector(mode = "list", length = K)
  R_res = vector(mode = "list", length = K+1)
  R_res[[1]] = X
  R = X
  for(vec in 1:K){
    result = sparse_PCA_1_vector(c_2, i_max, v, R, seq_lambda_2)
    u_res[[vec]] = result[[1]]
    v_res[[vec]] = result[[2]]
    sigma[[vec]] = t(u_res[[vec]])%*%R%*%v_res[[vec]]
    use_u = as.matrix(u_res[[vec]])
    use_v = as.matrix(v_res[[vec]])
    use_sigma = drop(as.double(sigma[[vec]]))
    R = R - use_sigma*(use_u%*%t(use_v))
    R_res[[vec+1]] = R
    v = svd(R)$v[,1]
  }
  return(list(u_res, v_res, sigma, R_res))
}

# Setting chosen parameters
K=2
c_2 = 2
i_max = 150
v = svd(fifa)$v[,1]
X = fifa
seq_lambda_2 = seq(0,40, 0.01)

# Calling function
sparse_PCA_our_2 = sparse_PCA_more_vec(K,c_2, i_max, v, X, seq_lambda_2)

# We now use the obtained updated X (second element of R list) after first iteration
# In the built-in package to obtain the first principal component of this updated X,
# that is, the second principal component of original data
sparse_PCA_2_check = SPC(sparse_PCA_our_2[[4]][[2]], sumabs = 2, K=1, center = FALSE, trace = FALSE)


