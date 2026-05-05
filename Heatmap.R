library(RColorBrewer)
library(pheatmap)
library(Cairo)

setwd('E:\\R')
data <- read.csv(file="20240614.csv", header=TRUE,row.names = 1,sep=",",stringsAsFactors=FALSE)
df<-scale(data) 
row_dend = hclust(dist(df))   
as.matrix(data)
tiff(
  filename = "Heatmap2.tiff", 
  width = 5,           
  height = 4,        
  units = "in",        
  res = 300) 

colormap <- colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)

p1 <- pheatmap(data, 
               color = colormap, 
               border_color = "white",  
               scale = "none", 
               cluster_rows = TRUE,
               cluster_cols = TRUE, 
               legend = TRUE,
               show_rownames = TRUE, 
               show_colnames = TRUE, 
               fontsize_row = 3,
               fontsize_col = 3, 
)

dev.off()

