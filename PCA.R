rm(list=ls())

library(factoextra)
library(FactoMineR)
library(ggplot2)
library(Cairo)

setwd('E:\\R')   
data <- data <- read.csv(file="20240614.csv",sep=",",header=T,check.names=FALSE)
pca.res <- PCA(data[,-11], graph = F, scale.unit = T) 
pca.res
tiff(
  filename = "cell0614.tiff", 
  width = 5,           
  height = 4,         
  units = "in",          
  bg = "transparent",         
  res = 300)
fviz_pca_ind(pca.res,
              geom.ind = "point",
              pointsize =3,
              pointshape = 19,
              col.ind = data$dign, 
              palette = c("#1597A5","#FFC24B","#FEB3AE"), 
              addEllipses = TRUE, 
              legend.title = "",
              title="")+
  theme_test() +
  theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
    text=element_text(size=0,face="plain",color="black"),
        axis.title=element_text(size=12,face="plain",color="black"),
        axis.text = element_text(size=10,face="plain",color="black"),
        legend.title = element_text(size=11,face="plain",color="black"),
        legend.text = element_text(size=0,face="plain",color="black"),
        legend.background = element_blank(),
        legend.position=c(0.75,0.15)
)


dev.off()
