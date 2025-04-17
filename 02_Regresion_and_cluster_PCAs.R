library(ggfortify)

dat_r = read.csv("PCA_data.csv")

names(dat_r)

names(dat_r)[c(2:17)] = c("Variable", "Values_Chin", "Values_Tail", "Values_Crown", "Values_Low.back", "Values_Mid.back", "Values_Upp.back", "Values_Forehead", "Values_Throat", "Values_Shoulder", "Values_Nape", "Values_Belly", "Values_Malar.patch", "Values_Chest", "Values_Weight", "Weight_log2")

UV = dat_r[dat_r$Variable == "UV",]
UV

VIS = dat_r[dat_r$Variable == "VIS",]
VIS
  
NIR = dat_r[dat_r$Variable == "NIR",]
NIR

#PCA UV
pca <- prcomp(UV[,c(3:15,17)], scale=TRUE)
summary(pca)
autoplot(pca, data = UV, colour = 'gray', size = 'Values_Weight', loadings = TRUE, loadings.colour = 'blue',loadings.label.colour = "black",
         loadings.label = TRUE, loadings.label.size = 3)+ ggtitle("PCA con datos de UV")

#PCA VIS
pca <- prcomp(VIS[,c(3:15,17)], scale=TRUE)
summary(pca)
autoplot(pca, data = VIS, colour = 'gray', size = 'Values_Weight', loadings = TRUE, loadings.colour = 'blue',loadings.label.colour = "black",
         loadings.label = TRUE, loadings.label.size = 3)+ ggtitle("PCA con datos de VIS")

#PCA NIR
pca <- prcomp(NIR[,c(3:15,17)], scale=TRUE)
summary(pca)
autoplot(pca, data = NIR, colour = 'gray', size = 'Values_Weight', loadings = TRUE, loadings.colour = 'blue',loadings.label.colour = "black",
         loadings.label = TRUE, loadings.label.size = 3)+ ggtitle("PCA con datos de NIR")

#Clusters generation
# install.packages("Cluster")
library(cluster)
library(factoextra)

#Gap statistic
fviz_nbclust(NIR[,3:17], pam, method = "gap_stat")
fviz_nbclust(VIS[,3:17], pam, method = "gap_stat")


#Silhouette statistic
fviz_nbclust(NIR[,3:17], pam, method = "silhouette")+ theme_classic()
fviz_nbclust(VIS[,3:17], pam, method = "silhouette")+ theme_classic()

#Distance matrix
library(vegan)
spe.ch <- vegdist(NIR[,3:17], "euc")
names(spe.ch) = NIR$Species

# Hierarchical cluster
spe.ch.UPGMA <- hclust(spe.ch, method = "average") 
plot(spe.ch.UPGMA, sub = "Average method", labels = NIR$Species)

spe.ch.WARD <- hclust(spe.ch, method = "ward.D") 
plot(spe.ch.WARD, sub = "ward.D method", labels = NIR$Species)

#Creation of Cluster
set.seed(240) # Setting seed
kmeans.re <- kmeans(NIR[,3:17], centers = 2, nstart = 20)
kmeans.re$cluster

#Adding clusters groups to data
NIR$Cluster = kmeans.re$cluster
NIR$Cluster = as.factor(kmeans.re$cluster)

#Exporting NIR-PCA
jpeg(filename = "biplot_NIR.jpg", width = 8, height = 5, units = "in", res = 600)
autoplot(pca, data = NIR, colour = 'Cluster', size = 'Values_Weight', loadings = TRUE, loadings.colour = 'blue',loadings.label.colour = "black",
           loadings.label = TRUE, loadings.label.size = 3,  loadings.label.vjust = -0.05, loadings.label.hjust = -0.05)+ ggtitle("PCA con datos de NIR") 
dev.off()

#Boxplot PC1 between groups
boxplot(pca$x[,1]~Cluster, NIR, col = c("red", "blue"), outline = FALSE, ylab = "PC1", main = "NIR")
#ANOVA PC1 between groups
summary(aov(pca$x[,1]~Cluster, NIR))

#Adding clusters groups to data
VIS$Cluster = as.factor(kmeans.re$cluster)
pca <- prcomp(VIS[,c(3:15,17)], scale=TRUE)
summary(pca)

#Exporting VIS-PCA
jpeg(filename = "biplot_VIS.jpg", width = 8, height = 5, units = "in", res = 600)
autoplot(pca, data = VIS, colour = 'Cluster', size = 'Values_Weight', loadings = TRUE, loadings.colour = 'blue',loadings.label.colour = "black",
         loadings.label = TRUE, loadings.label.size = 3,  loadings.label.vjust = -0.05, loadings.label.hjust = -0.05)+ ggtitle("PCA con datos de VIS")
dev.off()

#Boxplot PC1 between groups
boxplot(pca$x[,1]~Cluster, VIS, col = c("red", "blue"), outline = FALSE, ylab = "PC1", main = "VIS")
#ANOVA PC1-VIS between groups
summary(aov(pca$x[,1]~Cluster, VIS))


#Creation of Cluster
set.seed(240) # Setting seed
kmeans.re <- kmeans(UV[,3:17], centers = 2, nstart = 20)
kmeans.re$Cluster

#Adding clusters groups to data
UV$Cluster = as.factor(kmeans.re$cluster)
pca <- prcomp(UV[,c(3:15,17)], scale=TRUE)
summary(pca)

#Exporting UV-PCA
jpeg(filename = "biplot_UV.jpg", width = 8, height = 5, units = "in", res = 600)
autoplot(pca, data = UV, colour = 'Cluster', size = 'Values_Weight', loadings = TRUE, loadings.colour = 'blue',loadings.label.colour = "black",
         loadings.label = TRUE, loadings.label.size = 3,  loadings.label.vjust = -0.05, loadings.label.hjust = -0.05)+ ggtitle("PCA con datos de UV")
dev.off()

#Boxplot PC1 between groups
boxplot(pca$x[,1]~Cluster, UV, col = c("red", "blue"), outline = FALSE, ylab = "PC1", main = "UV")
#ANOVA PC1-UV between groups
summary(aov(pca$x[,1]~Cluster, UV))


#Boxplots

jpeg(filename = "boxplots_btw_groups.jpg", width = 8, height = 5, units = "in", res = 600)
pca <- prcomp(NIR[,c(3:15,17)], scale=TRUE)
summary(pca)

par(mfrow=c(1,3))

boxplot(pca$x[,1]~Cluster, NIR, col = c("#d44e4e", "#7084f4"), outline = FALSE, ylab = "PC1", main = "NIR")

pca <- prcomp(VIS[,c(3:15,17)], scale=TRUE)
summary(pca)

boxplot(pca$x[,1]~Cluster, VIS, col = c("#d44e4e", "#7084f4"), outline = FALSE, ylab = "PC1", main = "VIS")

pca <- prcomp(UV[,c(3:15,17)], scale=TRUE)
summary(pca)

boxplot(pca$x[,1]~Cluster, UV, col = c("#d44e4e", "#7084f4"), outline = FALSE, ylab = "PC1", main = "UV")
dev.off()
