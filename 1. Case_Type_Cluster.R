library(plyr)
library(caret)
library(stringr)
library(glmnet)
library(dendextend)
library(tidyverse)
library(cluster)
library(factoextra)
library(NbClust)
library(cluster)

##########################################################################################
# NB. Please Enter the Directory Path to the Folder Containing This File After 'setwd' Below
##########################################################################################

setwd("C:/Users/jcodo/Documents/Lurger_ODoherty_Capstone")

##########################################################################################



set.seed(101)

###top 33 casetypes (exclusing casetype 3216 which is a significant outlier)
retained_rows <- c(2813, 2864, 2759, 2793, 2867, 2582, 2859, 2893, 2739, 2847, 2762, 2837, 2829, 
                     2842, 2810, 2845, 2862, 2878, 2805, 2841, 2868, 2709, 2647, 2860, 2714, 
                     2737, 2892, 2846, 2719, 3044, 2720, 2850)
retained_rows <- sort(retained_rows)
retained_rows



###processing data, removing leaky and unhelpful variables
casetype <- read.csv(file='Cases_Type.csv', header = TRUE)
head(casetype)
casetype <- casetype[-c(2, 3, 5, 16, 19, 22, 23, 24, 25)]
names(casetype)[names(casetype) == 'ï..CaseTypeID'] <- 'CaseTypeID'
casetype <- casetype %>% filter(CaseTypeID %in% retained_rows)
summary(casetype)
kmeans_casetype <- casetype
casetypescale <- scale(casetype[c(2, 3)])
casetypescale
summary(casetype)

casetype <- casetype[,c(1, 2, 4, 5, 9, 11, 12, 15, 16)]

###########################################################
# HIERARCHICAL CLUSTERING
###########################################################
clust <- dist(casetype, method = 'euclidean')
hierarch <- hclust(clust, method = "complete")

dendro <- as.dendrogram(hierarch)
dendro <- color_branches(dendro, k = 3)
plot(dendro, main = 'Hierarchical Clustering of CaseTypes')
cutdendro <- cutree(dendro,  3)
table(cutdendro)

######exploring different numbers of cuts
dendro4 <- as.dendrogram(hierarch)
dendro4 <- color_branches(dendro, k = 4)
plot(dendro4, main = 'Hierarchical Clustering of CaseTypes')
cutdendro4 <- cutree(dendro,  4)
table(cutdendro4)

dendro5 <- as.dendrogram(hierarch)
dendro5 <- color_branches(dendro, k = 5)
plot(dendro5, main = 'Hierarchical Clustering of CaseTypes')
cutdendro5 <- cutree(dendro,  5)
table(cutdendro5)

dendro6 <- as.dendrogram(hierarch)
dendro6 <- color_branches(dendro, k = 6)
plot(dendro6, main = 'Hierarchical Clustering of CaseTypes')
cutdendro6 <- cutree(dendro,  6)
table(cutdendro6)
cutdendro

##adding cluster numbers from optimal number of cuts
casetype$hclust <- cutdendro4
summary(casetype)
casetype
###########################################################
# TABLE OF RESULTS
###########################################################
table(cutdendro4)
table(cutdendro)

###find the casetypes that fit into each cluster
hclust_cluster1 <- as.integer(rownames(subset(casetype, hclust == 1)))
hclust_cluster2 <- as.integer(rownames(subset(casetype, hclust == 2)))
hclust_cluster3 <- as.integer(rownames(subset(casetype, hclust == 3)))
hclust_cluster4 <- as.integer(rownames(subset(casetype, hclust == 4)))

###eg. with cluster 1
hclust_cluster1
summary(casetype)
###########################################################
## SCALED
###########################################################

###creating scaled data
# scaled <- casetype[,c(1, 2, 3, 4, 5, 7, 9, 11, 12, 13, 15, 16)]
scaled <- casetype
rownames(scaled) <- scaled$ï..CaseTypeID
scaled <- subset(scaled[,2:10])
scaled <- as.matrix(scaled)
scaled <- (scale(scaled))

###clustering hierarchically with 4 clusters
hierarch_scaled <- hclust(dist(scaled), method="complete")
dendro_scaled <- as.dendrogram(hierarch_scaled)
dendro_scaled <- color_branches(dendro_scaled, k = 4)
plot(dendro_scaled, main = 'Scaled Hierarchical Case Types', xlab = ' ')
cutdendro_scaled <- cutree(dendro_scaled,  4)


casetype$hclust_scaled <- cutdendro_scaled
table(cutdendro_scaled)
casetype$hclust_scaled
scaled
scaled_hclust_cluster1 <- as.integer(rownames(subset(casetype, hclust_scaled == 1)))
scaled_hclust_cluster2 <- as.integer(rownames(subset(casetype, hclust_scaled == 2)))
scaled_hclust_cluster3 <- as.integer(rownames(subset(casetype, hclust_scaled == 3)))
scaled_hclust_cluster4 <- as.integer(rownames(subset(casetype, hclust_scaled == 4)))


summary(scaled)
summary(casetype)

###########################################################
## KMEANS
###########################################################

#performing kmeans clustering on the scaled dataset
kme_scaled4 <- kmeans(scaled[,1:8], centers = 4, nstart = 25)
kme_scaled4
table(kme_scaled4$cluster)

#performing PCA on the scaled kmeans clusters
PCAvisualise4 <- fviz_cluster(kme_scaled4, data = scaled, main = 'Case Type PCA: Scaled Data', 
                             ggtheme = theme_light())
PCAvisualise4
kme_scaled4$centers

head(scaled)

#trying out different numbers of clusters
kme_scaled5 <- kmeans(scaled[,1:8], centers = 5, nstart = 25)
kme_scaled5
PCAvisualise5 <- fviz_cluster(kme_scaled5, data = scaled, main = 'Case Type PCA: Scaled Data', 
                              ggtheme = theme_light())
PCAvisualise5
table(kme_scaled5$cluster)

kme_scaled6 <- kmeans(scaled[,1:8], centers = 6, nstart = 25)
kme_scaled6
PCAvisualise6 <- fviz_cluster(kme_scaled6, data = scaled, main = 'Case Type PCA: Scaled Data', 
                              ggtheme = theme_light())
PCAvisualise6
table(kme_scaled6$cluster)

kme_scaled3 <- kmeans(scaled[,1:8], centers = 3, nstart = 25)
kme_scaled3
PCAvisualise3 <- fviz_cluster(kme_scaled3, data = scaled, main = 'Case Type PCA: Scaled Data', 
                              ggtheme = theme_light())
PCAvisualise3
table(kme_scaled3$cluster)
table(kme_scaled4$cluster)
table(kme_scaled5$cluster)
table(kme_scaled6$cluster)

###screeplot of components
pc <- princomp(scaled[,1:8])
summary(pc)
pc$loadings
screeplot(pc)

###using relative clustering validation to determine optimal number of clusters
nb <- NbClust(scaled[,1:8], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")
# Visualize the result
fviz_nbclust(nb) + theme_minimal()


###silhoette analysis
sil <- silhouette(kme_scaled3$cluster, dist(scaled[,1:8]))
head(sil[, 1:3], 10)
plot(sil, main = 'Silhouette plot - K-means')
fviz_silhouette(sil)
sil.sum <- summary(sil)
sil.sum$clus.avg.widths
sil.sum$clus.sizes


###optimal number of cluster = 3, so add these cluster numbers
casetype$km_Scaled <- kme_scaled3$cluster

kmeans_clusters <- casetype[c(1,12)]
kmeans_clusters
summary(casetype)

scaled_kmeans_cluster1 <- as.integer(rownames(subset(casetype, km_Scaled == 1)))
scaled_kmeans_cluster2 <- as.integer(rownames(subset(casetype, km_Scaled == 2)))
scaled_kmeans_cluster3 <- as.integer(rownames(subset(casetype, km_Scaled == 3)))

summary(subset(casetype, km_Scaled == 1))
summary(subset(casetype, km_Scaled == 2))
summary(subset(casetype, km_Scaled == 3))

scaled_kmeans_cluster1
scaled_kmeans_cluster2
scaled_kmeans_cluster3

summary(casetype)

cluster1 <- (subset(casetype, km_Scaled == 1))$CaseTypeID
cluster2 <- (subset(casetype, km_Scaled == 2))$CaseTypeID
cluster3 <- (subset(casetype, km_Scaled == 3))$CaseTypeID
cluster3
cluster3
abc <- read.csv(file='Cases_Type.csv', header = TRUE)
head(abc)
names(abc)[names(abc) == 'ï..CaseTypeID'] <- 'CaseTypeID'
cluster1_cases <- abc[abc$CaseTypeID %in% cluster1,]
cluster2_cases <- abc[abc$CaseTypeID %in% cluster2,]
cluster3_cases <- abc[abc$CaseTypeID %in% cluster3,]
cluster1_cases$Name
cluster2_cases$Name
cluster3_cases$Name
abc$CaseTypeID
scaled_kmeans_cluster1

