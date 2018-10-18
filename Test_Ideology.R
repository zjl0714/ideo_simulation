################################################################################################
# Program: Test_Ideology
# Author:	Ye Wang (NYU)
# Aim: Test the performance of K means clustering on the Zuobiao dataset
# Revised: Sep. 23. 2018
################################################################################################
library("flexclust")
library(mclust)
library(cluster)
library(fpc)
library(factoextra)
library(vegan)
library(NbClust)

rm(list=ls())
load("/Users/yewang/Dropbox/CurrentProjects/PartyAlignmentChina/RawData/Ideology/data/sample10K.RData")
load("/Users/Junlong/Dropbox/Data/CGSS_share/RawData/Ideology/data/sample10K.RData")
load("c:/Users/jaz352/Dropbox/Data/CGSS_share/RawData/Ideology/data/sample10K.RData")

opinions <- d[, c(16:65)]
ninstance <- dim(opinions)[1]
train_idx <- c(sample(ninstance, ninstance * 0.8))
train_set <- opinions[train_idx, ]
test_set <- opinions[-train_idx, ]

m_clust <- Mclust(as.matrix(opinions), G=1:100) #聚类数目从1一直试到20
summary(m_clust) # Got 3 clusters
plot(m_clust,"BIC") 

#       PAM(Partitioning Around Medoids) 
pamk.best <- pamk(opinions)
pamk.best$nc # Got 2 clusters
clusplot(pam(opinions, 2))

#       Calinsky criterion
ca_clust <- cascadeKM(opinions, 1, 100, iter = 1000)
ca_clust$results 
calinski.best <- as.numeric(which.max(ca_clust$results[2,]))
calinski.best # 2

#        Gap Statistic
gap_clust <- clusGap(opinions, kmeans, 100, B = 500, verbose = interactive())
gap_clust # 2
fviz_gap_stat(gap_clust)

#       Average silhouette method
fviz_nbclust(opinions, kmeans, method = "silhouette")

#       NBCLUSTER
set.seed(1234) #因为method选择的是kmeans，所以如果不设定种子，每次跑得结果可能不同
nb_clust <- NbClust(opinions,  distance = "euclidean", 
                    min.nc=2, max.nc=100, method = "kmeans",
                    index = "alllong", alphaBeale = 0.1)
barplot(table(nb_clust$Best.nc[1,]),xlab = "聚类数",ylab = "支持指标数")

performance <- rep(NA, 49)
within.performance <- rep(NA, 49)
between.performance <- rep(NA, 49)

for (k in 2:50){
  kmeans.result <- kmeans(train_set, iter.max = 100, k)
  performance[k-1] <- kmeans.result$totss
  within.performance[k-1] <- kmeans.result$tot.withinss
  between.performance[k-1] <- kmeans.result$betweenss
}
par(mfrow=c(3, 1))
plot(performance~seq(2, 50))
plot(within.performance~seq(2, 50))
plot(between.performance~seq(2, 50))