
# Unsupervised Machine Learning
# Assigm 3: K-means clustering
# March 2020, Maja Nordfeldt


# Libraries
library(cluster)
library(NbClust)
library(GGally)
library(dplyr)
library(ggplot2)
#library(purrr)
#library(tibble)
library(factoextra)
library(gridExtra)


df_sample1 <- df_sample


# Data exploration ggplots
ggpairs(as.data.frame(df_sample1),mapping = aes(colour = factor(Cluster)))
# can't separate by clster 
#ggparcoord(df_sample1, scale = "uniminmax", groupColumn =factor(df_sample1[,11]))


# K means clustering 
cluster6 <- kmeans(df_sample, centers = 6)
cluster5 <- kmeans(df_sample, centers = 5)
cluster4 <- kmeans(df_sample, centers = 4)
cluster3 <- kmeans(df_sample, centers = 3)
cluster2 <- kmeans(df_sample, centers = 2)

# Cluster plot general dimension - compare K
plotk6 <- fviz_cluster(cluster6, geom = "point",data=df_sample)  + ggtitle("K = 6")
plotk5 <- fviz_cluster(cluster5, geom = "point",data=df_sample)  + ggtitle("K = 5")
plotk4 <-fviz_cluster(cluster4, geom = "point",data=df_sample)  + ggtitle("K = 4")
plotk3 <-fviz_cluster(cluster3, geom = "point",data=df_sample)  + ggtitle("K = 3")
plotk2 <-fviz_cluster(cluster2, geom = "point",data=df_sample)  + ggtitle("K = 2")
plotk1 <-fviz_cluster(cluster1, geom = "point",data=df_sample)  + ggtitle("K = 1")
grid.arrange(plotk1,plotk2,plotk3,plotk4,plotk5,plotk6,nrow = 2)

# Elbow method
test1 <- fviz_nbclust(df_sample, kmeans, method = "wss",linecolor="turquoise") + ggtitle("Elbow") + theme_gray()# Elbow method
test2 <- fviz_nbclust(df_sample, kmeans, method = "silhouette",linecolor="turquoise") + ggtitle("Silhouette") + theme_gray()
#gap_stat <- fviz_nbclust(df_sample, kmeans, method = "gap_stat",nboot=100) # Gap stat
gap_stat <- clusGap(df_sample, FUN = kmeans,
                    K.max = 10, B = 50)
test3 <- fviz_gap_stat(gap_stat,linecolor="turquoise",) + ggtitle("Gap Statistic") + theme_gray()# use 6 clusters 
grid.arrange(test1,test2,test3,nrow = 1)


# Plot on variables compared according to clusters 
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


# Fraction of TSS explained by between variation 
BSS <- cluster2$betweenss
TSS <- cluster2$totss
BSS / TSS 
