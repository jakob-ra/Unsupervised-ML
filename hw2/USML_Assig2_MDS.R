

# Unsupervised Machine Learning
# Assigm 2: Multidimensional Scaling
# March 2020, Maja Nordfeldt


# Load data ---------------------------------------------------------------

m_delta <- basket

# Prepare data ------------------------------------------------------------
  # Change to disimilarities (1-s_norm), set diagonal to 0 

# Normalizing similarity
diag(m_delta) <- 0
s_max <- max(m_delta) # Q:Not including diagonals?
m_delta<- m_delta*(1/s_max)

# Changing to dissimalirity
m_delta <-  matrix(1,dim(m_delta)[1],dim(m_delta)[1]) -m_delta

# Set diagonal to 0
diag(m_delta) <- 0

m_delta <- as.matrix(m_delta)


# Euclidean Distance Function ---------------------------------------------
  # c. Write a function in R that has as output the n ́n matrix of Euclidean distances (not squared Euclidean distances) between the rows of an n ́p input matrix of coordinates X.

D_finder <- function(X){

  n <- dim(X)[1]
  p <- dim(X)[2]
  D <- matrix(NA,n,n)
  
  for(i in 1:n){
    for(j in 1:n){
      D[i,j] <- norm(as.matrix((X[i,]-X[j,])),type="f") #Euclidean norm between row x_i and x_j
    }
  }
  
  return(D)
}


# Q: loop over n * n?


#  SMACOF (majorization) algorithm ----------------------------------------

SMACOF <- function(m_Xstart,m_delta,epsilon){
  
  # Check that similarities matrix fulfill conditions
  if(sum(diag(m_delta))!= 0 | sum(sign(m_delta))!=0 |isSymmetric(m_delta) == FALSE){
    print("Wrong format of sigma matrix. Check that diagonal is 0, elements are non-negative, it is symmetric")
  }
  else{
    
    # SMACOF
    
    # Initial Round
    i_k <- 0
    m_Z <- m_Xstart
    i_n <- dim(m_delta)[1]
    i_n2_delta <- sum(m_delta[lower.tri(m_delta)]^2)
    m_DZ <- D_finder(m_Z)
    
    # Create B matrix    
    m_F <- m_delta/m_DZ
    m_F1 <- rowSums(m_F)
    m_B_Z <- diag(m_F1)-m_F
    
    i_n2_Z <- i_n*(sum(diag(t(m_Z)%*%m_Z)))
    i_p_Z <- -(sum(diag(t(m_Z)%*%m_B_Z%>%m_Z)))
    i_stressZ <-  i_n2_delta -2*i_p_Z + i_n2_Z 
    i_stress_n <- i_stressZ*(1/i_n2_delta)
      
    i_impr <- 0
    
    while(k==0 | i_impr > epsilon){
      
      k <- k+1
      
      # Updata X
      m_F <- m_delta/m_DZ
      m_F1 <- rowSums(m_F)
      m_B_Z <- diag(m_F1)-m_F
      m_X_k <- (1/i_n)%*%m_B_Z%*%m_Z
      m_Z <- m_X_k
      m_DZ <- D_finder(m_Z)
      
      # Create B matrix    
      m_F <- m_delta/m_DZ
      m_F1 <- rowSums(m_F)
      m_B_Z <- diag(m_F1)-m_F
      
      # Calculate stress
      i_n2_Z <- i_n*(sum(diag(t(m_Z)%*%m_Z)))
      i_p_Z <- -(sum(diag(t(m_Z)%*%m_B_Z%>%m_Z)))
      i_stressZ <-  i_n2_delta -2*i_p_Z + i_n2_Z 
      
      # Normalized stress
      i_stress_nprev <- i_stress_n
      i_stress_n <- i_stressZ*(1/i_n2_delta)
      
      # Improvement
      i_impr <- i_stress_n - i_stress_nprev
      print(c("Iteration",k," gives normalized stress value",i_stress_n,", improving previous value with",i_impr))
            
      
    }
    

  }
  
  # Return (normalized) Stress value  + final configuration 
  return(list(i_stress_n,m_Z,m_DZ,k))
}

m_Xstart <- matrix(0.4,dim(m_delta)[1],2)
epsilon <- 10^(-6)
SMACOF(m_Xstart,m_delta,epsilon)


SMACOF(m_sigma,m_sigma,epsilon)

