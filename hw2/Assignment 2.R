# *------------------------------------------------------------------
# Unsupervised Machine Learning - Homework 2
# *----------------------------------------------------------------

# Clear cache
rm(list = ls())
library(smacof)

# Load the data
load('/Users/gabrielaszini/Documents/Unsupervised ML/Assignment 2/basket.RData')

# Here the values indicate co-purchase, so we will do the same transformation as in the paper
# No entries are 0, so do not need to worry about that
# create empty matrix of dissimilarities
 size = dim(basket)[1]
 dissimilarities= matrix(, nrow = size, ncol = size)

 for(i in 1:size){
   for(j in 1:size){
     dissimilarities[i,j] = log((basket[i,i]*basket[j,j])/(basket[j,i]*basket[i,j])) #did not put minus sign such that 
   }
 }
 
 # Compare dissimilarities with the ones obtained by sim2diss() using method = "counts"
 # TO DO
 
 euclideandist=function(X){
   store = X%*%t(X)
   alpha = diag(store)
   euclideandistsq = alpha%*%t(as.matrix(replicate(dim(store)[1],1))) + as.matrix(replicate(dim(store)[1],1))%*%t(alpha) - 2*store
   euclideandist = sqrt(euclideandistsq)
   return(euclideandist)
 }
 DZ = euclideandist(X)
 
 # SMACOF function 
 # should be general in that it accepts as input a general matrix nxn of dissimilarities and nxp matrix of initial coordinates
 # ATTENTION : initial_coordinates must be nxp
 smacov_ours = function(dissimilarities, initial_coordinates, eps){
   # iteration counter 
   k = 0
   Z = initial_coordinates
   D_Z = euclideandist(Z)
   dissimilarities_D_Z = dissimilarities*D_Z
   raw_stress_k = sum(dissimilarities[lower.tri(dissimilarities, diag = FALSE)]^2) + sum(D_Z[lower.tri(D_Z, diag = FALSE)]^2) - 2*sum(dissimilarities_D_Z[lower.tri(dissimilarities_D_Z, diag = FALSE)])
   # from raw stress get the normalized stress
   norm_stress_k = raw_stress_k/(sum(dissimilarities[lower.tri(dissimilarities, diag = FALSE)]^2))
   print(norm_stress_k)
   norm_stress_k_1 = 1000
   # start loop
     while(k == 0 | (norm_stress_k_1 - norm_stress_k) > eps){
       k = k + 1
       F = dissimilarities/D_Z 
       diag(F) = 0
       F1 = rowSums(F)
       B_Z = diag(F1) - F
       X_k =  dim(dissimilarities)[1]^(-1)*(B_Z%*%Z)
       Z = X_k
       D_Z = euclideandist(Z)
       dissimilarities_D_Z = dissimilarities*D_Z
       norm_stress_k_1 = norm_stress_k
       raw_stress_k = sum(dissimilarities[lower.tri(dissimilarities, diag = FALSE)]^2) + sum(D_Z[lower.tri(D_Z, diag = FALSE)]^2) - 2*sum(dissimilarities_D_Z[lower.tri(dissimilarities_D_Z, diag = FALSE)])
       norm_stress_k = raw_stress_k/(sum(dissimilarities[lower.tri(dissimilarities, diag = FALSE)]^2))
       # Function should print the normalized stress values and the improvement over the previous stress values
       print(k)
       print(norm_stress_k)
       print(norm_stress_k_1-norm_stress_k)
     }
   return(X_k)
 }
 
 # testing function with random initial values
 initial_coordinates = abs(replicate(2, rnorm(12)))
 eps = 10^(-6)
 X_k = smacov_ours(dissimilarities, initial_coordinates, eps)

 # After convergence make a plot of the final configuration. Add labels to the points
 # TO DO
 
 # Compare own code with that od mds of smacof package
 # Run mds() on data while setting temporarily itmax = 1 so that you can save the initial configuration and use it on own programe
 resultpackage_1it = mds(dissimilarities, ndim=2, type="ratio", itmax = 1, eps = 1e-06)
 initial_coordinates_1it = resultpackage_1it$conf
 
 # Do a full run of mds() in R and your own function
 # our code
 X_k_ourcode = smacov_ours(dissimilarities, initial_coordinates_1it, eps)
 resultpackage = mds(dissimilarities, ndim=2, type="ratio", eps = 1e-06)
 X_k_package = resultpackage$conf
 # X_k equal up to a constant !!!!! because of ratio and not absolute??
 X_k_ourcode/X_k_package # to check what I just said
 
 ####Making nice plots
 
 #Configuration plot
 plot(resultpackage, plot.type = "confplot", plot.dim = c(1,2), sphere = TRUE, 
      bubscale = 1, col = 1, label.conf = list(label = TRUE, pos = 3, 
                                               col = 1, cex = 0.8), hull.conf = list(hull = FALSE, col = 1, 
                                                                                     lwd = 1, ind = NULL), shepard.x = NULL, identify = FALSE, 
      type = "p", pch = 20, cex = 0.5, asp = 1)
 
 #Residual plot
 plot(resultpackage, plot.type = "resplot", plot.dim = c(1,2), sphere = TRUE, 
      bubscale = 1, col = 1, label.conf = list(label = TRUE, pos = 3, 
                                               col = 1, cex = 0.8), hull.conf = list(hull = FALSE, col = 1, 
                                                                                     lwd = 1, ind = NULL), shepard.x = NULL, identify = FALSE, 
      type = "p", pch = 20, cex = 0.5, asp = 1)
 
 #Shepard
 plot(resultpackage, plot.type = "Shepard", plot.dim = c(1,2), sphere = TRUE, 
      bubscale = 1, col = 1, label.conf = list(label = TRUE, pos = 3, 
                                               col = 1, cex = 0.8), hull.conf = list(hull = FALSE, col = 1, 
                                                                                     lwd = 1, ind = NULL), shepard.x = NULL, identify = FALSE, 
      type = "p", pch = 20, cex = 0.5, asp = 1)
 
 plot(resultpackage, plot.type = "Shepard", plot.dim = c(1,2), sphere = TRUE, 
      bubscale = 1, col = 1, label.conf = list(label = TRUE, pos = 3, 
                                               col = 1, cex = 0.8), hull.conf = list(hull = FALSE, col = 1, 
                                                                                     lwd = 1, ind = NULL), shepard.x = basket, identify = FALSE, 
      type = "p", pch = 20, cex = 0.5, asp = 1)
 
 plot(resultpackage, plot.type = "stressplot", plot.dim = c(1,2), sphere = TRUE, 
      bubscale = 1, col = 1, label.conf = list(label = TRUE, pos = 3, 
                                               col = 1, cex = 0.8), hull.conf = list(hull = FALSE, col = 1, 
                                                                                     lwd = 1, ind = NULL), shepard.x = basket, identify = FALSE, 
      type = "p", pch = 20, cex = 0.5, asp = 1)
 
 plot(resultpackage, plot.type = "bubbleplot", plot.dim = c(1,2), sphere = TRUE, 
      bubscale = 1, col = 1, label.conf = list(label = TRUE, pos = 3, 
                                               col = 1, cex = 0.8), hull.conf = list(hull = FALSE, col = 1, 
                                                                                     lwd = 1, ind = NULL), shepard.x = basket, identify = FALSE, 
      type = "p", pch = 20, cex = 0.5, asp = 1)
 
 #CHECK DIMENSION WITH PERMUTATION TEST?
 #CHECK DISSIMILARITY
 #CHECK IF DOING STRESS CORRECT
 #CHECK IF DOING RATIO
 #CHECK ABOUT RATIO IN BUBBLEPLOT

