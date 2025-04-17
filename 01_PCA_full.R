library(vegan)
library(ape)
library(ggfortify)
library(reshape2)
library(factoextra)


env <- read.csv("Final_data.csv", sep = ";")
env

#Main variables and filtering by site
env2 = env[env$Localidad == "Uribe-Uribe",] 
env2

#Excluding Chloroceryle amazona
env2 = env2[env2$Especie != "Chloroceryle amazona",]
env2


# line charts -------------------------------------------------------------

mat_T = as.data.frame.matrix(t(env2[,c(7:1217)])) #Transponer la matriz
colnames(mat_T) = paste0(env2$Especie,"_", env2$Punto_corporal)
rownames(mat_T)

#UV
UV = apply(mat_T[1:168,], 2, FUN = mean)

#VIS
VIS = apply(mat_T[169:680,], 2, FUN = mean)

#NIR
NIR = apply(mat_T[681:1211,], 2, FUN = mean)


dat = data.frame(UV = UV, VIS = VIS, NIR = NIR)
dat$Species = env2$Especie
dat$b_part = env2$Punto_corporal
dat$Peso = env2$Peso


#Matrix reshaping
dat_melt =  melt(dat)

m = reshape(dat_melt, direction="wide", idvar=c("Species", "variable"), timevar="b_part")
  
mat_ok = m[order(m$Species), ]
mat_ok = mat_ok[,-16]
str(mat_ok)

W = mat_ok[mat_ok$variable == "Peso",3]

mata = mat_ok[mat_ok$variable != "Peso",]
mata

mata$value.Peso = rep(W, each = 3)
mata$Peso_log2 = log2(mata$value.Peso)

#generate mata cluster
set.seed(240) # Setting seed
kmeans.re <- kmeans(mata[,3:17], centers = 2, nstart = 20)
kmeans.re$cluster

mata$cluster = kmeans.re$cluster

#Exporting data
# write.csv(mata, "datos_para_PCA_No_passerines.csv", row.names = FALSE)

#Body parts names
names(mata)[2:17] = c("Variable","Chin", "Tail","Crown","Low_back","Mid_back","Upp_back","Forehead","Throat","Shoulder","Nape","Belly","Malar_patch","Chest","Weight", "Weight_log2")

pca <- prcomp(mata[,c(3:15,17)], scale=TRUE)
summary(pca)

#Exporting PCA with full data
jpeg(filename = "biplot_full_data_Feb_08.jpg", width = 8, height = 5, units = "in", res = 600)
autoplot(pca, data = mata, colour = 'Variable', size = 'Weight', loadings = TRUE, loadings.colour = 'blue',loadings.label.colour = "black",
         loadings.label = TRUE, loadings.label.size = 3)+ ggtitle("PCA with aggregated data")
dev.off()

#Gradient color by weight
autoplot(pca, data = mata, colour = 'cluster', size = 'Weight', loadings = TRUE, loadings.colour = 'blue',loadings.label.colour = "black",loadings.label = TRUE, loadings.label.size = 3)+ ggtitle("PCA con datos totales")

#Eigenvalues PCA
fviz_eig(pca, addlabels = TRUE)

ev <- pca$sdev^2
names(ev) = colnames(pca$x)

evplot <- function(ev)
{
  # Broken stick model (MacArthur 1957)
  n <- length(ev)
  bsm <- data.frame(j=seq(1:n), p=0)
  bsm$p[1] <- 1/n
  for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))
  bsm$p <- 100*bsm$p/n
  # Plot eigenvalues and % of variation for each axis
  op <- par(mfrow=c(1,2))
  barplot(ev, main="Eigenvalues", col="bisque", las=2, names.arg = names(ev))
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
          main="% variation", col=c("bisque",2), las=2 , names.arg = names(ev))
  legend("topright", c("% eigenvalue", "Broken stick model"), 
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}

#Exporting comparison between eigenvalues- average and broken stick model
jpeg("Eigenvalues_Broken_stick.jpg", width = 10, height = 7, units = "in", res = 600)
evplot(ev)
dev.off()


# Contributions of variables to PC1
jpeg("Contri_var_PC1.jpg", width = 10, height = 7, units = "in", res = 600)
fviz_contrib(pca, choice = "var", axes = 1, top = 5) + ggtitle("Variables contribution to the PC1") + ylab("Contribution (%)")
dev.off()

# Contributions of variables to PC2
jpeg("Contri_var_PC2.jpg", width = 10, height = 7, units = "in", res = 600)
fviz_contrib(pca, choice = "var", axes = 2, top = 5) + ggtitle("Variables contribution to PC2") + ylab("Contribution (%)")
dev.off()


# Lineal model ------------------------------------------------------------
library(tidyverse)

#Most important variables for PC1

#Model Sup back
jpeg(filename = "reg_esp_sup.jpg", width = 8, height = 5, units = "in", res = 600)
mata %>%
    ggplot(aes(x=Weight_log2,
               y=Upp_back,
               color=Variable))+
    geom_point(aes(size = Weight))+
    geom_smooth(method="lm",fullrange=FALSE) +
  ggtitle("Weight vs Upper back") + xlab("Log2(Weight)") + ylab("Reflectance Upp_back")
dev.off()


NIR_dat = mata[mata$Variable == "NIR",]
lm1= lm(Upp_back~Weight_log2, data = NIR_dat)
summary(lm1)

# Model Mid Back
jpeg(filename = "reg_Mid_back.jpg", width = 8, height = 5, units = "in", res = 600)
mata %>%
  ggplot(aes(x=Weight_log2,
             y=Mid_back,
             color=Variable))+
  geom_point(aes(size = Weight))+
  geom_smooth(method="lm",fullrange=FALSE) +
  ggtitle("Weight vs Mid Back") + xlab("Log2(Weight)") + ylab("Reflectance Mid_back")
dev.off()

lm2= lm(Mid_back~Weight_log2, data = NIR_dat)
summary(lm2)

# Model Low back
jpeg(filename = "reg_Low_back.jpg", width = 8, height = 5, units = "in", res = 600)
mata %>%
  ggplot(aes(x=Weight_log2,
             y=Low_back,
             color=Variable))+
  geom_point(aes(size = Weight))+
  geom_smooth(method="lm",fullrange=FALSE) +
  ggtitle("Weight vs Lower Back") + xlab("Log2(Weight)") + ylab("Reflectance Low_back")
dev.off()

lm3= lm(Low_back~Weight_log2, data = NIR_dat)
summary(lm3)


# Model Chest
jpeg(filename = "reg_Chest.jpg", width = 8, height = 5, units = "in", res = 600)
mata %>%
  ggplot(aes(x=Weight_log2,
             y=Chest,
             color=Variable))+
  geom_point(aes(size = Weight))+
  geom_smooth(method="lm",fullrange=FALSE) +
  ggtitle("Weight vs Chest") + xlab("Log2(Weight)") + ylab("Reflectance Chest")
dev.off()

# Model Belly
jpeg(filename = "reg_Belly.jpg", width = 8, height = 5, units = "in", res = 600)
mata %>%
  ggplot(aes(x=Weight_log2,
             y=Belly,
             color=Variable))+
  geom_point(aes(size = Weight))+
  geom_smooth(method="lm",fullrange=FALSE) +
  ggtitle("Weight vs Belly") + xlab("Log2(Weight)") + ylab("Reflectance Belly")
dev.off()

#Most important variables for PC2
#Chin
#Throat
#Shoulder
#Forehead

# Modelo Chin
jpeg(filename = "reg_Chin.jpg", width = 8, height = 5, units = "in", res = 600)
mata %>%
  ggplot(aes(x=Weight_log2,
             y=Chin,
             color=Variable))+
  geom_point(aes(size = Weight))+
  geom_smooth(method="lm",fullrange=FALSE) +
  ggtitle("Weight vs Chin") + xlab("Log2(Weight)") + ylab("Reflectance Chin")
dev.off()

# Model Throat
jpeg(filename = "reg_Throat.jpg", width = 8, height = 5, units = "in", res = 600)
mata %>%
  ggplot(aes(x=Weight_log2,
             y=Throat,
             color=Variable))+
  geom_point(aes(size = Weight))+
  geom_smooth(method="lm",fullrange=FALSE) +
  ggtitle("Weight vs Throat") + xlab("Log2(Weight)") + ylab("Reflectance Throat")
dev.off()

# Model Shoulder
jpeg(filename = "reg_Shoulder.jpg", width = 8, height = 5, units = "in", res = 600)
mata %>%
  ggplot(aes(x=Weight_log2,
             y=Shoulder,
             color=Variable))+
  geom_point(aes(size = Weight))+
  geom_smooth(method="lm",fullrange=FALSE) +
  ggtitle("Weight vs Shoulder") + xlab("Log2(Weight)") + ylab("Reflectance Shoulder")
dev.off()

# Model Crown
jpeg(filename = "reg_Crown.jpg", width = 8, height = 5, units = "in", res = 600)
mata %>%
  ggplot(aes(x=Weight_log2,
             y=Crown,
             color=Variable))+
  geom_point(aes(size = Weight))+
  geom_smooth(method="lm",fullrange=FALSE) +
  ggtitle("Weight vs Crown") + xlab("Log2(Weight)") + ylab("Reflectance Crown")
dev.off()


# Model Forehead
jpeg(filename = "reg_Forehead.jpg", width = 8, height = 5, units = "in", res = 600)
mata %>%
  ggplot(aes(x=Weight_log2,
             y=Forehead,
             color=Variable))+
  geom_point(aes(size = Weight))+
  geom_smooth(method="lm",fullrange=FALSE) +
  ggtitle("Weight vs Forehead") + xlab("Log2(Weight)") + ylab("Reflectance Forehead")
dev.off()


# Regression coefficients -------------------------------------------------

m1 = lm(Upp_back ~ Weight_log2 + Variable, dat = mata)
summary(m1)

m2 = lm(Upp_back ~ Weight_log2 * Variable, dat = mata)
summary(m2)

AIC(m1, m2)

m3 = lm(Mid_back ~ Weight_log2 + Variable, dat = mata)
summary(m3)

m4 = lm(Mid_back ~ Weight_log2 * Variable, dat = mata)
summary(m4)

AIC(m3, m4)

m5 = lm(Low_back ~ Weight_log2 + Variable, dat = mata)
summary(m5)

m6 = lm(Low_back ~ Weight_log2 * Variable, dat = mata)
summary(m6)

AIC(m5,m6)

m7 = lm(Forehead ~ Weight_log2 + Variable, dat = mata)
summary(m7)

m8 = lm(Forehead ~ Weight_log2 * Variable, dat = mata)
summary(m8)

AIC(m7,m8)

m9 = lm(Throat ~ Weight_log2 + Variable, dat = mata)
summary(m9)

m10 = lm(Throat ~ Weight_log2 * Variable, dat = mata)
summary(m10)

AIC(m9,m10)

m11 = lm(Shoulder ~ Weight_log2 + Variable, dat = mata)
summary(m11)

m12 = lm(Shoulder ~ Weight_log2 * Variable, dat = mata)
summary(m12)

AIC(m11, m12)

m13 = lm(Chest ~ Weight_log2 + Variable, dat = mata)
summary(m13)

m14 = lm(Chest ~ Weight_log2 * Variable, dat = mata)
summary(m14)

AIC(m13, m14)

m15 = lm(Belly ~ Weight_log2 + Variable, dat = mata)
summary(m15)

m16 = lm(Belly ~ Weight_log2 * Variable, dat = mata)
summary(m16)

AIC(m15, m16)

m17 = lm(Chin ~ Weight_log2 + Variable, dat = mata)
summary(m17)

m18 = lm(Chin ~ Weight_log2 * Variable, dat = mata)
summary(m18)

AIC(m17, m18)

m19 = lm(Crown ~ Weight_log2 + Variable, dat = mata)
summary(m19)

m20 = lm(Crown ~ Weight_log2 * Variable, dat = mata)
summary(m20)

AIC(m19, m20)

