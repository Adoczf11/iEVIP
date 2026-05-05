
rm(list=ls())
library(MASS)
library(ggplot2)

setwd('E:\\R')   

data <- read.csv(file="20240614.csv")
nx = 63
irisdata = data[1:nx, 1:10]
irisgrp = data[1:nx, 11]
head(irisdata)
head(irisgrp)
class(irisgrp)

irisgrp = as.factor(irisgrp)

(lda.sol = lda(irisdata, irisgrp))


result = predict(lda.sol, irisdata)
result$class
table(irisgrp, result$class) 

P = lda.sol$scaling

means = lda.sol$means %*% P

total_means = as.vector(lda.sol$prior %*% means)
n_samples = nrow(irisdata)

x<-as.matrix(irisdata) %*% P - (rep(1, n_samples) %o% total_means)
head(x)
df = as.data.frame(x)



ggplot(data = df, aes(LD1,LD2,color=irisgrp)) +
  geom_point (size = 3)+

  stat_ellipse(aes(x=LD1,y=LD2,fill= irisgrp), geom="polygon", level=0.95, alpha=0.2) +
 
  scale_color_manual(values=c("#1597A5","#FFC24B","#FEB3AE")) +
  scale_fill_manual(values=c("#1597A5","#FFC24B","#FEB3AE"))+ 
  labs(x = "LD1",y="LD2")+
  # labs(fill = "")+
  theme_test()+
  theme(panel.border = element_rect(color = "black", size = 1, fill = NA),
    text=element_text(size=12,face="plain",color="black"),
    axis.title=element_text(size=12,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title = element_text(size=0,face="plain",color="black"),
    legend.text = element_text(size=0,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position=c(0.06,0.12)
  )



ggsave(
  filename = "LDA-0614.tiff",
  width = 5,            
  height = 4,           
  units = "in",        
  dpi = 300             
)
