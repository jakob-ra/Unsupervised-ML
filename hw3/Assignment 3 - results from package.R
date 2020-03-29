# Unsupervised Machine Learning
# Assigm 3: K-means clustering
# March 2020, Maja Nordfeldt & Gabriela Szini

####clear cache
rm(list = ls())

#### Libraries
library(cluster)
library(NbClust)
library(GGally)
library(dplyr)
library(ggplot2)
library(factoextra)
library(gridExtra)
library(Hmisc)

#### Read data
df_sample <- read_xlsx("/Users/gabrielaszini/Documents/Unsupervised ML/Assignment 3/df_sample.xlsx")
# Excluding variables we do not use in the clustering
df_sample1 <- df_sample %>% select(-c(ST004D01T,BSMJ,W_FSTUWT,CNT,SCIEEFF))
# Convert all variable types to numeric
df_sample1 <- as.data.frame(apply(df_sample1, 2, as.numeric))
# Scaling
df_sample1 <- as.data.frame(apply(df_sample1, 2,
                                 function(x) (x - min(x)) / (max(x) - min(x))))

#### Data exploration ggplots
ggpairs(as.data.frame(df_sample1),mapping = aes(colour = factor(Cluster))) #FACTOR CLUSTER NOT FOUND ???
# can't separate by clster 
#ggparcoord(df_sample1, scale = "uniminmax", groupColumn =factor(df_sample1[,11]))

#### K means clustering 
cluster6 <- kmeans(df_sample1, centers = 6)
cluster5 <- kmeans(df_sample1, centers = 5)
cluster4 <- kmeans(df_sample1, centers = 4)
cluster3 <- kmeans(df_sample1, centers = 3)
cluster2 <- kmeans(df_sample1, centers = 2)
cluster1 <- kmeans(df_sample1, centers = 1)

# Cluster plot general dimension - compare K
plotk6 <- fviz_cluster(cluster6, geom = "point",data=df_sample1)  + ggtitle("K = 6")
plotk5 <- fviz_cluster(cluster5, geom = "point",data=df_sample1)  + ggtitle("K = 5")
plotk4 <-fviz_cluster(cluster4, geom = "point",data=df_sample1)  + ggtitle("K = 4")
plotk3 <-fviz_cluster(cluster3, geom = "point",data=df_sample1)  + ggtitle("K = 3")
plotk2 <-fviz_cluster(cluster2, geom = "point",data=df_sample1)  + ggtitle("K = 2")
plotk1 <-fviz_cluster(cluster1, geom = "point",data=df_sample1)  + ggtitle("K = 1")
grid.arrange(plotk1,plotk2,plotk3,plotk4,plotk5,plotk6,nrow = 2)

#### Methods for selecting k
#Elbow method
test1 <- fviz_nbclust(df_sample1, kmeans, method = "wss",linecolor="turquoise") + ggtitle("Elbow") + theme_gray()# Elbow method
#Silhouette
test2 <- fviz_nbclust(df_sample1, kmeans, method = "silhouette",linecolor="turquoise") + ggtitle("Silhouette") + theme_gray()
#gap_stat <- fviz_nbclust(df_sample, kmeans, method = "gap_stat",nboot=100) # Gap stat
gap_stat <- clusGap(df_sample1, FUN = kmeans,
                    K.max = 10, B = 50)
test3 <- fviz_gap_stat(gap_stat,linecolor="turquoise",) + ggtitle("Gap Statistic") + theme_gray()# use 6 clusters 
grid.arrange(test1,test2,test3,nrow = 1)

#### Plot on variables compared according to clusters 
df_sample1$Cluster <- as.factor(cluster2$cluster)

plotanx1 <- ggplot(data = df_sample1) + geom_point(aes(x = INSTSCIE, y = ANXTEST, colour = as.factor(Cluster))) + ggtitle("Instrumental Motivation") + labs(colour = "Clusters")
plotanx2 <- ggplot(data = df_sample1) + geom_point(aes(x = MOTIVAT, y = ANXTEST, colour = as.factor(Cluster))) + ggtitle("Achievement Motivation") + labs(colour = "Clusters")
plotanx3 <- ggplot(data = df_sample1) + geom_point(aes(x = CPSVALUE, y = ANXTEST, colour = as.factor(Cluster))) + ggtitle("Team work valuation") + labs(colour = "Clusters")
plotanx4 <- ggplot(data = df_sample1) + geom_point(aes(x = EMOSUPP, y = ANXTEST, colour = as.factor(Cluster))) + ggtitle("Parental Emotional Support") + labs(colour = "Clusters")
plotanx5 <- ggplot(data = df_sample1) + geom_point(aes(x = PARED, y = ANXTEST, colour = as.factor(Cluster))) + ggtitle("Parental Education") + labs(colour = "Clusters")
plotanx6 <- ggplot(data = df_sample1) + geom_point(aes(x = HOMEPOS, y = ANXTEST, colour = as.factor(Cluster))) + ggtitle("Home Possessions") + labs(colour = "Clusters")
plotanx7 <- ggplot(data = df_sample1) + geom_point(aes(x = BMMJ1, y = ANXTEST, colour = as.factor(Cluster))) + ggtitle("Mothers Occupational Status") + labs(colour = "Clusters")
plotanx8 <- ggplot(data = df_sample1) + geom_point(aes(x = BFMJ2, y = ANXTEST, colour = as.factor(Cluster))) + ggtitle("Fathers Occupational Status") + labs(colour = "Clusters")
grid.arrange(plotanx1,plotanx2,plotanx3,plotanx4,plotanx5,plotanx6,plotanx7,plotanx8,nrow = 2)
#obtain weird graph

#### Fraction of TSS explained by between variation 
BSS <- cluster2$betweenss
TSS <- cluster2$totss
BSS / TSS 

#### Trying different nstart values
# Given that we choose k = 2, trying with different numbers of random starts for this number of clusters to check if differs
# package default = 10 (are we sure?)

cluster2 <- kmeans(df_sample1, centers = 2)
cluster2_10 <- kmeans(df_sample1, centers = 2, nstart= 10)
cluster2_25 <- kmeans(df_sample1, centers = 2, nstart= 25)
cluster2_100 <- kmeans(df_sample1, centers = 2, nstart= 100)
cluster2_1000 <- kmeans(df_sample1, centers = 2, nstart= 1000)

# comparing the assignments
difference1_10 = sum(cluster2$cluster != cluster2_10$cluster)
difference1_25 = sum(cluster2$cluster != cluster2_25$cluster)
difference1_100 = sum(cluster2$cluster != cluster2_100$cluster)
difference1_1000 = sum(cluster2$cluster != cluster2_1000$cluster)
# we see that there are no differences

#### Checking if there is any relation to the expectations variable

# 1st we take the group assigment from the cluster2, and convert those into dummies
dums_cluster2 <- dummy(cluster2$cluster)
# Now we get the expectations variable from the original dataset and create matrix with dummies and them
data_expectation = as.data.frame(cbind(cluster2$cluster, dums_cluster2, df_sample$BSMJ))
data_expectation_cluster1  = data_expectation[which(data_expectation[,1] == 1),]
data_expectation_cluster1[,4] = as.numeric(data_expectation_cluster1[,4])
data_expectation_cluster2  = data_expectation[which(data_expectation[,1] == 2),]
data_expectation_cluster2[,4] = as.numeric(data_expectation_cluster2[,4])

# Now we look at summary statistics of the expectations for both clusters
summary(data_expectation_cluster1[,4])
histogram(data_expectation_cluster1[,4])
summary(data_expectation_cluster2[,4])
histogram(data_expectation_cluster2[,4])
#cluster 2 has slightly higher expectations but nothing too dramatic

#### Checking if there is any relation to the hability in science variable
data_hab_science = as.data.frame(cbind(cluster2$cluster, dums_cluster2, df_sample$SCIEEFF))
data_hab_science_cluster1  = data_hab_science[which(data_hab_science[,1] == 1),]
data_hab_science_cluster1[,4] = as.numeric(data_hab_science_cluster1[,4])
data_hab_science_cluster2  = data_hab_science[which(data_hab_science[,1] == 2),]
data_hab_science_cluster2[,4] = as.numeric(data_hab_science_cluster2[,4])

summary(data_hab_science_cluster1[,4])
histogram(data_hab_science_cluster1[,4])
summary(data_hab_science_cluster2[,4])
histogram(data_hab_science_cluster2[,4])
# no apparent indication here

#### Checking if there is any relation to gender
data_gender = as.data.frame(cbind(cluster2$cluster, dums_cluster2, df_sample$ST004D01T))
data_gender_cluster1  = data_gender[which(data_gender[,1] == 1),]
data_gender_cluster1[,4] = as.numeric(data_gender_cluster1[,4])
data_gender_cluster2  = data_gender[which(data_gender[,1] == 2),]
data_gender_cluster2[,4] = as.numeric(data_gender_cluster2[,4])

summary(data_gender_cluster1[,4])
histogram(data_gender_cluster1[,4])
summary(data_gender_cluster2[,4])
histogram(data_gender_cluster2[,4])
# no evidence here as well



