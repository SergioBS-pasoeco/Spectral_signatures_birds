library(reshape2)
library(forcats)

env <- read.csv("Final_data.csv", sep = ";")
env

#Algunas de las principales variables
env2 = env[env$Localidad == "Uribe-Uribe",] 
env2

#Excluding Chloroceryle amazona
env2 = env2[env2$Especie != "Chloroceryle amazona",]
env2


# Sgolay filter -----------------------------------------------------------

#sgolay: Efficient Savitzky-Golay Filtering 
library(signal)

mat_dat = as.matrix(env2[1:1598,7:1217])
mat_sgolay = matrix(data = NA, nrow = 1598, ncol = 1211)

for (i in 1:1598){
  mat_sgolay[i,] = sgolayfilt(mat_dat[i,], p = 3, n = 3 + 10)
}

mat_sgolay

env_sgolay = cbind(env2[,1:6],mat_sgolay)
names(env_sgolay)[7:1217] = names(env2)[7:1217]

# Sgolay_charts -----------------------------------------------------------

# Passerines_No_passerines ------------------------------------------------
unique(env2$Familia)

#NO passerines
#"Picidae"
#"Bucconidae"
#"Psittacidae"
#"Columbidae"
#"Trochilidae"

id = which(env2$Familia == "Picidae" | env2$Familia == "Bucconidae" | env2$Familia == "Psittacidae" | env2$Familia == "Columbidae" | env2$Familia == "Trochilidae")
id

NPASS = env_sgolay[id,]
PASS = env_sgolay[-id,]

# Signatures_Passerines ---------------------------------------------------

#Espalda media
Esp_med = PASS[PASS$Punto_corporal == "Esp_Med",]

#Aggregate by specie
Esp_med_agg = aggregate(.~Familia +Especie, data = Esp_med[,c(2,3,7:1217)], FUN = mean)

#extract cluster values
ID = NIR[NIR$Species %in% Esp_med_agg$Especie,]  
ID$Species == Esp_med_agg$Especie
Esp_med_agg$cluster = ID$Cluster


#Espalda superior
Esp_sup = PASS[PASS$Punto_corporal == "Esp_Sup",]

#Aggregate by specie
Esp_sup_agg = aggregate(.~Familia+Especie, data = Esp_sup[,c(2,3,7:1217)], FUN = mean)

Esp_sup_agg$cluster = ID$Cluster

#Espalda inferior
Esp_inf = PASS[PASS$Punto_corporal == "Esp_Inf",]

#Aggregate by specie
Esp_inf_agg = aggregate(.~Familia+Especie, data = Esp_inf[,c(2,3,7:1217)], FUN = mean)

Esp_inf_agg$Especie
Esp_inf_agg$cluster = ID$Cluster

#Pecho
pecho = PASS[PASS$Punto_corporal == "Pecho",]

#Aggregate by specie
pecho_agg = aggregate(.~Familia+Especie, data = pecho[,c(2,3,7:1217)], FUN = mean)

pecho_agg$Especie
pecho_agg$cluster = ID$Cluster

#Panza
panza = PASS[PASS$Punto_corporal == "Panza",]

#Aggregate by specie
panza_agg = aggregate(.~Familia+Especie, data = panza[,c(2,3,7:1217)], FUN = mean)

panza_agg$Especie
panza_agg$cluster = ID$Cluster

#Charts signatures passerines
library(RColorBrewer)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#ad0505", "#00256f", "#3d4045", "#14bc19", "#802d04", "#ff36f9", "#b3e30f")


mm = melt(Esp_med_agg)
jpeg(filename = "Spec_PASS_sig_Esp_med_sgolay.jpg", width = 9, height = 6, units = "in", res = 600)
ggplot(mm, aes(x=variable, y=value, group=Especie, color=Familia))+geom_line()+ggtitle("Mid Back - Passeriformes")+ theme(axis.text.x = element_text(angle = 90), axis.text.x.bottom =  element_blank())+
  facet_grid(. ~ cluster,
             scales = "free_x") + geom_vline(xintercept = c(168, 682),size=0.8,linetype="dashed") + scale_colour_manual(values = cbbPalette) +
  geom_smooth(size=1.5)
dev.off()

mm = melt(Esp_sup_agg)
jpeg(filename = "Spec_PASS_sig_Esp_sup_sgolay.jpg", width = 9, height = 6, units = "in", res = 600)
ggplot(mm, aes(x=variable, y=value, group=Especie, color=Familia))+geom_line()+ggtitle("Upper Back - Passeriformes")   +
  facet_grid(. ~ cluster,
             scales = "free_x") + theme(axis.text.x = element_text(angle = 90), axis.text.x.bottom =  element_blank())+
  facet_grid(. ~ cluster,
             scales = "free_x") + geom_vline(xintercept = c(168, 682),size=0.5,linetype="dashed") + scale_colour_manual(values = cbbPalette) +
  geom_smooth(size=1.5)
dev.off()

mm = melt(Esp_inf_agg)
jpeg(filename = "Spec_PASS_sig_Esp_inf_sgolay.jpg", width = 9, height = 6, units = "in", res = 600)
ggplot(mm, aes(x=variable, y=value, group=Especie, color=Familia))+geom_line()+ggtitle("Lower Back - Passeriformes")   +
  facet_grid(. ~ cluster,
             scales = "free_x")+ theme(axis.text.x = element_text(angle = 90), axis.text.x.bottom =  element_blank())+
  facet_grid(. ~ cluster,
             scales = "free_x") + geom_vline(xintercept = c(168, 682),size=0.5,linetype="dashed")  + scale_colour_manual(values = cbbPalette) +
  geom_smooth(size=1.5)
dev.off()


mm = melt(pecho_agg)
jpeg(filename = "Spec_PASS_sig_pecho_sgolay.jpg", width = 9, height = 6, units = "in", res = 600)
ggplot(mm, aes(x=variable, y=value, group=Especie, color=Familia))+geom_line()+ggtitle("Chest - Passeriformes")   + theme(axis.text.x = element_text(angle = 90), axis.text.x.bottom =  element_blank())+
  facet_grid(. ~ cluster,
             scales = "free_x") + geom_vline(xintercept = c(168, 682),linetype="dashed", size=0.5)  + scale_colour_manual(values = cbbPalette) +
  geom_smooth(size=1.5)
dev.off()


mm = melt(panza_agg)
jpeg(filename = "Spec_PASS_sig_Panza_sgolay.jpg", width = 9, height = 6, units = "in", res = 600)
ggplot(mm, aes(x=variable, y=value, group=Especie, color=Familia))+geom_line()+ggtitle("Belly - Passeriformes")+ theme(axis.text.x = element_text(angle = 90), axis.text.x.bottom =  element_blank())+
  facet_grid(. ~ cluster,
             scales = "free_x") + geom_vline(xintercept = c(168, 682),size=0.5 ,linetype="dashed")  + scale_colour_manual(values = cbbPalette) +
  geom_smooth(size=1.5)
dev.off()


# Signatures_No_Passerines ------------------------------------------------


#Espalda media
Esp_med = NPASS[NPASS$Punto_corporal == "Esp_Med",]

#Aggregate by specie
Esp_med_agg = aggregate(.~Familia +Especie, data = Esp_med[,c(2,3,7:1217)], FUN = mean)

#extract cluster values
ID = NIR[NIR$Species %in% Esp_med_agg$Especie,]  
ID$Species == Esp_med_agg$Especie
Esp_med_agg$cluster = ID$Cluster


#Espalda superior
Esp_sup = NPASS[NPASS$Punto_corporal == "Esp_Sup",]

#Aggregate by specie
Esp_sup_agg = aggregate(.~Familia+Especie, data = Esp_sup[,c(2,3,7:1217)], FUN = mean)

Esp_sup_agg$cluster = ID$Cluster

#Espalda inferior
Esp_inf = NPASS[NPASS$Punto_corporal == "Esp_Inf",]

#Aggregate by specie
Esp_inf_agg = aggregate(.~Familia+Especie, data = Esp_inf[,c(2,3,7:1217)], FUN = mean)

Esp_inf_agg$Especie
Esp_inf_agg$cluster = ID$Cluster

#Pecho
pecho = NPASS[NPASS$Punto_corporal == "Pecho",]

#Aggregate by specie
pecho_agg = aggregate(.~Familia+Especie, data = pecho[,c(2,3,7:1217)], FUN = mean)

pecho_agg$Especie
pecho_agg$cluster = ID$Cluster

#Panza
panza = NPASS[NPASS$Punto_corporal == "Panza",]

#Aggregate by specie
panza_agg = aggregate(.~Familia+Especie, data = panza[,c(2,3,7:1217)], FUN = mean)

panza_agg$Especie
panza_agg$cluster = ID$Cluster

#Charts signatures NPASSerines
library(RColorBrewer)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#ad0505", "#00256f", "#3d4045", "#14bc19", "#802d04", "#ff36f9", "#b3e30f")


mm = melt(Esp_med_agg)
jpeg(filename = "Spec_NPASS_sig_Esp_med_sgolay.jpg", width = 9, height = 6, units = "in", res = 600)
ggplot(mm, aes(x=variable, y=value, group=Especie, color=Familia))+geom_line()+ggtitle("Mid Back - No Passeriformes")+ theme(axis.text.x = element_text(angle = 90), axis.text.x.bottom =  element_blank())+
  facet_grid(. ~ cluster,
             scales = "free_x") + geom_vline(xintercept = c(168, 682),size=0.8,linetype="dashed") + scale_colour_manual(values = cbbPalette) +
  geom_smooth(size=1.5)
dev.off()

mm = melt(Esp_sup_agg)
jpeg(filename = "Spec_NPASS_sig_Esp_sup_sgolay.jpg", width = 9, height = 6, units = "in", res = 600)
ggplot(mm, aes(x=variable, y=value, group=Especie, color=Familia))+geom_line()+ggtitle("Upper Back - No Passeriformes")   +
  facet_grid(. ~ cluster,
             scales = "free_x") + theme(axis.text.x = element_text(angle = 90), axis.text.x.bottom =  element_blank())+
  facet_grid(. ~ cluster,
             scales = "free_x") + geom_vline(xintercept = c(168, 682),size=0.5,linetype="dashed") + scale_colour_manual(values = cbbPalette) +
  geom_smooth(size=1.5)
dev.off()

mm = melt(Esp_inf_agg)
jpeg(filename = "Spec_NPASS_sig_Esp_inf_sgolay.jpg", width = 9, height = 6, units = "in", res = 600)
ggplot(mm, aes(x=variable, y=value, group=Especie, color=Familia))+geom_line()+ggtitle("Lower Back - No Passeriformes")   +
  facet_grid(. ~ cluster,
             scales = "free_x")+ theme(axis.text.x = element_text(angle = 90), axis.text.x.bottom =  element_blank())+
  facet_grid(. ~ cluster,
             scales = "free_x") + geom_vline(xintercept = c(168, 682),size=0.5,linetype="dashed")  + scale_colour_manual(values = cbbPalette) +
  geom_smooth(size=1.5)
dev.off()


mm = melt(pecho_agg)
jpeg(filename = "Spec_NPASS_sig_pecho_sgolay.jpg", width = 9, height = 6, units = "in", res = 600)
ggplot(mm, aes(x=variable, y=value, group=Especie, color=Familia))+geom_line()+ggtitle("Chest - No Passeriformes")   + theme(axis.text.x = element_text(angle = 90), axis.text.x.bottom =  element_blank())+
  facet_grid(. ~ cluster,
             scales = "free_x") + geom_vline(xintercept = c(168, 682),linetype="dashed", size=0.5)  + scale_colour_manual(values = cbbPalette) +
  geom_smooth(size=1.5)
dev.off()


mm = melt(panza_agg)
jpeg(filename = "Spec_NPASS_sig_Panza_sgolay.jpg", width = 9, height = 6, units = "in", res = 600)
ggplot(mm, aes(x=variable, y=value, group=Especie, color=Familia))+geom_line()+ggtitle("Belly - No Passeriformes")+ theme(axis.text.x = element_text(angle = 90), axis.text.x.bottom =  element_blank())+
  facet_grid(. ~ cluster,
             scales = "free_x") + geom_vline(xintercept = c(168, 682),size=0.5 ,linetype="dashed")  + scale_colour_manual(values = cbbPalette) +
  geom_smooth(size=1.5)
dev.off()



