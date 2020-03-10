# --------------------------------------------------------------------------

# Package Results Sparse PC for comparison 

# --------------------------------------------------------------------------
if (!require('PMA')) install.packages('PMA'); library('PMA')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('matlib')) install.packages('matlib'); library('matlib')

# --------------------------------------------------------------------------

# Set seed
set.seed(1234)

# Select variables
dropcol <-c("name", "club", "Position", "eur_value", "eur_wage", "eur_release_clause")
dropcolindex <- which(colnames(fifa)%in% dropcol)
X <- fifa[,-dropcolindex] 

# Scale variables
X <- as.matrix(scale(X))

# Save names & other var
Y <- as.data.frame(fifa[,1])
v_position <- fifa[,3]
v_club <- fifa[,2]

# Set c
dim(X)[1]
dim(X)[2]
sqrt(dim(X)[2]) # 5.38 max c2
c1 <- 2
c2 <- 2

# Run Sparse PC
l_PMD2 <- PMD(X,sumabsv=c1, sumabsu=c2,K=2,center=TRUE)
l_PMD5 <- PMD(X,sumabsv=c1, sumabsu=c2,K=5,center=TRUE)
l_PMD10 <- PMD(X,sumabsv=c1, sumabsu=c2,K=10,center=TRUE)

# Retrieve loadings 
m_loadings2 <- cbind(colnames(X),l_PMD2$v)
m_loadings5 <- cbind(colnames(X),l_PMD5$v)
m_loadings10 <- cbind(colnames(X),l_PMD10$v)

colnames(m_loadings2) <- c("Features","PC1","PC2")
colnames(m_loadings5) <- c("Features","PC1","PC2","PC3","PC4","PC5")
colnames(m_loadings10) <- c("Features","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")

# Visualize PC
data2 <- X%*%l_PMD2$v
data2 <- cbind(fifa[,1], data2)
plot(data=data2,x=data2[,1], y=data2[,2])
colnames(data2) <- c("Player","PC1","PC2")
data2 <- cbind(v_club,v_position,data2)

# Plot by position
ggplot(data2, aes(x=PC1, y=PC2,colour=Position))+geom_point(size=0.75) + geom_text(aes(label=club),hjust=0, vjust=0,size=2)

# Plot by club
ggplot(data2, aes(x=PC1, y=PC2,colour=club))+geom_point(size=0.75) + geom_text(aes(label=club),hjust=0, vjust=0,size=2)


# SPC function

l_SPC2 <- SPC(X, sumabsv=c1,K = 2,center = TRUE, trace = FALSE)
l_SPC3 <- SPC(X, sumabsv=c1,K = 3,center = TRUE, trace = FALSE)
l_SPC4 <- SPC(X, sumabsv=c1,K = 4,center = TRUE, trace = FALSE)
l_SPC5 <- SPC(X, sumabsv=c1,K = 5,center = TRUE, trace = FALSE)
l_SPC6 <- SPC(X, sumabsv=c1,K = 6,center = TRUE, trace = FALSE)
l_SPC7 <- SPC(X, sumabsv=c1,K = 7,center = TRUE, trace = FALSE)
l_SPC8 <- SPC(X, sumabsv=c1,K = 8,center = TRUE, trace = FALSE)
l_SPC10 <- SPC(X, sumabsv=c1,K = 10,center = TRUE, trace = FALSE)
l_SPC20 <- SPC(X, sumabsv=c1,K = 20,center = TRUE, trace = FALSE)

# sumabsu = sqrt(nrow(x)) automatically
l_SPC2$prop.var.explained
l_SPC3$prop.var.explained
l_SPC5$prop.var.explained
l_SPC6$prop.var.explained
l_SPC7$prop.var.explained
l_SPC8$prop.var.explained
l_SPC10$prop.var.explained
#  [1] 0.1253214 0.2574548 0.3731493 0.4759221 0.5811415 0.6960312 0.7574040 0.8146097 0.8442545
#  [10] 0.8678843
l_SPC20$prop.var.explained
plot(l_SPC20$prop.var.explained) # Looks like around 10 PC is enough
