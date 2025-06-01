#Carga del dataset

library(ggplot2)
library(cluster)
library(factoextra)
library(dplyr)

df = read.csv("Mall_Customers.csv")
head(df)

#Exploración y limpieza de datos

dim(df)
str(df)
summary(df)
colnames(df) = c("CustomerID", "Genre", "Age", "AnnualIncome", "SpendingScore")
colnames(df)
df$Genre = ifelse(df$Genre == "Male", 1, 0)
df_scaled = scale(df[, c("AnnualIncome", "SpendingScore")])

#Exploración de variables

hist(df$Age)
hist(df$Genre)
boxplot(df$AnnualIncome)
boxplot(df$SpendingScore)

#Entrenamiento de modelos de clustering
##K-Means

set.seed(123)
wss = sapply(2:6, function(k){
  kmeans(df_scaled, centers = k, nstart = 25)$tot.withinss
})
plot(2:6, wss, type = "b", pch = 19, col = "blue",
     xlab = "Número de clusters", ylab = "Suma de cuadrados")

set.seed(123)
kmeans_model = kmeans(df_scaled, centers = 4, nstart = 25)
df$Cluster_KMeans = kmeans_model$cluster

##Clustering jerarquico

dist_matrix = dist(df_scaled)
hc = hclust(dist_matrix, method = "ward.D")
plot(hc, labels = FALSE, main = "Dendrograma")

df$Cluster_HC = cutree(hc, k = 4)

#Evaluación de modelos
## Silhouette para KMeans
sil_kmeans = silhouette(df$Cluster_KMeans, dist_matrix)
mean(sil_kmeans[, 3])

## Silhouette para Jerárquico
sil_hc = silhouette(df$Cluster_HC, dist_matrix)
mean(sil_hc[, 3])

#Análisis descriptivo de segmentos

aggregate(df[, c("Age", "AnnualIncome", "SpendingScore")], 
          by = list(Cluster = df$Cluster_KMeans), FUN = mean)

#Visualización
##KMeans
fviz_cluster(kmeans_model, data = df_scaled,
             geom = "point", ellipse.type = "norm", 
             palette = "jco", ggtheme = theme_minimal())

##Clustering Jerárquico
fviz_cluster(list(data = df_scaled, cluster = df$Cluster_HC),
             geom = "point", ellipse.type = "norm", 
             palette = "Set2", ggtheme = theme_minimal())

