rm(list=ls())

library(mixOmics)
library(ggplot2)

setwd('E:\\R') 
nx = 63
otu <- data[1:nx, 1:10]
group <- data[1:nx, 11]

df_plsda <- plsda(otu, group$group, ncomp = 2)

plotIndiv(df_plsda , comp = c(1,2),
          group = group$group, style = 'ggplot2',ellipse = T, 
          size.xlabel = 20, size.ylabel = 20, size.axis = 20, pch = 16, cex = 5)

df <- unclass(df_plsda)

df1 = as.data.frame(df$variates$X)
df1$group = group$group
df1$samples = rownames(df1)

explain = df$prop_expl_var$X
x_lable <- round(explain[1],digits=3)
y_lable <- round(explain[2],digits=3)

col=c("#1597A5","#FFC24B","#FEB3AE")
p1<-ggplot(df1,aes(x=comp1,y=comp2,
                   color=group,shape=group))+
  theme_test()+
  geom_point(size=3,shape=19)+
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  labs(x=paste0("PC1 (",x_lable*100,"%)"),
       y=paste0("PC2 (",y_lable*100,"%)"))+
  stat_ellipse(data=df1,
               geom = "polygon",level = 0.95,
               linetype = 1,linewidth=0.5,
               aes(fill=group),
               alpha=0.2,
               show.legend = T)+
  scale_color_manual(values = col) +
  scale_fill_manual(values = c("#1597A5","#FFC24B","#FEB3AE"))+
  theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
    axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12,angle=90),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        panel.grid=element_blank(),
        legend.title = element_text(size=0,face="plain",color="black"),
        legend.text = element_text(size=0,face="plain",color="black"),
        legend.background = element_blank(),
        legend.position=c(0.10,0.13))
p1
ggsave(
  filename = "PLS-DA0614.tiff", 
  width = 5,            
  height = 4,            
  units = "in",          
  dpi = 300              
)


