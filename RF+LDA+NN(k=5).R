rm(list=ls())
library(randomForest)
library(caret)
library(ISLR2)
library(dummy)
library(multiROC)
library(ggplot2)
library(tidyr)
setwd('E:\\R')  
data <- read.csv(file="20240614.csv", header=T,sep=",",stringsAsFactors=FALSE)
features <- data[, -ncol(data)]  
labels <- data[, ncol(data)]   

set.seed(123)  
train_control <- trainControl(method = "repeatedcv", number = 5, savePredictions = "final")

rf_model <- train(
  x = features,
  y = labels,
  method = "rf",
  trControl = train_control
  ,ntree = 1000  
)


print(rf_model)

 rf_predictions <- rf_model$pred
write.table(rf_predictions,"rf0614.csv",row.names=FALSE,col.names=TRUE,sep=",")

lda_model <- train(
  x = features,
  y = labels,
  method = "lda",
  trControl = train_control
)
print(lda_model)

mn_model <- train(
  x = features,
  y = labels,
  method = "nnet",
  trControl = train_control
  # ,ntree = 1000
)

print(mn_model)

rf_predictions <- rf_model$pred
lda_predictions <- lda_model$pred
mn_predictions <- mn_model$pred
rf_predictions_sorted <- rf_predictions[order(rf_predictions$rowIndex), ]
lda_predictions_sorted <- lda_predictions[order(lda_predictions$rowIndex), ]
mn_predictions_sorted <- mn_predictions[order(mn_predictions$rowIndex), ]

combined_results <- data.frame(
  Actual = rf_predictions_sorted$obs,  
  RF_Pred = rf_predictions_sorted$pred,
  LDA_Pred = lda_predictions_sorted$pred,
  MN_Pred = mn_predictions_sorted$pred
)

write.csv(combined_results, "0614.csv", row.names = FALSE)


actual <- factor(combined_results$Actual)
predicted <- factor(combined_results$MN_Pred)

levels(actual) <- levels(predicted) <- c( 'MCF 10A', 'MCF 7', 'MDA-MB-231'  )

conf_matrix <- confusionMatrix(predicted, actual)
print(conf_matrix)

conf_matrix_table <- as.data.frame(conf_matrix$table)

reference_totals <- aggregate(Freq ~ Reference, data = conf_matrix_table, sum)


conf_matrix_table <- merge(conf_matrix_table, reference_totals, by = "Reference", suffixes = c("", ".total"))

conf_matrix_table$Ratio <- conf_matrix_table$Freq / conf_matrix_table$Freq.total


ggplot(data = conf_matrix_table, aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = Ratio)) +
  scale_fill_gradientn(colors = rev(hcl.colors(10, "Blues")),
                       breaks = NULL) +
  labs( x = "Predicted", y = "Actual") +
  theme_test()+                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
  theme(
   
    axis.title=element_text(size=12,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
    axis.ticks = element_line(colour = "black",size = 1,lineend=10),
    legend.title = element_text(size=0,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position='none'
  )

ggsave(
  filename = "CM1.tiff",
  width = 5,             
  height = 4,           
  units = "in",         
  dpi = 300              
)

