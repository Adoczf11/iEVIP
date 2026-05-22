rm(list = ls())

library(caret)
library(randomForest)
library(MASS)
library(nnet)
library(pROC)
library(PRROC)
library(ggplot2)

set.seed(123)

# ===============================
# Load data
# ===============================

df <- read.csv(
  file = "Batch_mix_EV.csv",
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)

labels_raw <- factor(df[, ncol(df)])

label_key <- data.frame(
  Class_Raw = levels(labels_raw),
  Class_Safe = make.names(levels(labels_raw), unique = TRUE),
  stringsAsFactors = FALSE
)

labels <- factor(
  label_key$Class_Safe[match(as.character(labels_raw), label_key$Class_Raw)],
  levels = label_key$Class_Safe
)

display_names <- setNames(label_key$Class_Raw, label_key$Class_Safe)

features <- df[, -ncol(df), drop = FALSE]

# Remove possible identifier columns
id_cols <- grep("id|ID|sample|Sample|number|Number", colnames(features))
if (length(id_cols) > 0) {
  features <- features[, -id_cols, drop = FALSE]
}

# Convert all feature columns to numeric values
features <- as.data.frame(features)
features[] <- lapply(features, function(x) {
  suppressWarnings(as.numeric(x))
})

# Remove columns with all missing values
all_na_cols <- which(colSums(!is.na(features)) == 0)
if (length(all_na_cols) > 0) {
  keep_cols <- setdiff(seq_len(ncol(features)), all_na_cols)
  features <- features[, keep_cols, drop = FALSE]
}

# Keep complete cases only
complete_idx <- complete.cases(features) & !is.na(labels)
features <- features[complete_idx, , drop = FALSE]
labels <- droplevels(labels[complete_idx])

display_names <- display_names[levels(labels)]

# Remove near-zero variance predictors
nzv <- nearZeroVar(features)
if (length(nzv) > 0) {
  keep_cols <- setdiff(seq_len(ncol(features)), nzv)
  features <- features[, keep_cols, drop = FALSE]
}

# Remove linearly dependent predictors
if (ncol(features) > 1) {
  linear_combo <- findLinearCombos(features)
  if (!is.null(linear_combo$remove)) {
    if (length(linear_combo$remove) > 0) {
      keep_cols <- setdiff(seq_len(ncol(features)), linear_combo$remove)
      features <- features[, keep_cols, drop = FALSE]
    }
  }
}

if (ncol(features) < 1) {
  stop("No valid feature column is available for model training.")
}

print(table(labels))
print(round(prop.table(table(labels)), 4))
print(dim(features))

# ===============================
# Cross-validation settings
# ===============================

min_class_n <- min(table(labels))

if (min_class_n < 2) {
  stop("At least one class has fewer than two samples; cross-validation cannot be performed.")
}

cv_number <- min(10, min_class_n)

fold_index <- createMultiFolds(
  y = labels,
  k = cv_number,
  times = 20
)

train_control <- trainControl(
  method = "repeatedcv",
  number = cv_number,
  repeats = 20,
  index = fold_index,
  savePredictions = "final",
  classProbs = TRUE,
  sampling = NULL
)

# ===============================
# Utility functions
# ===============================

trapz_manual <- function(x, y) {
  idx <- order(x)
  x <- x[idx]
  y <- y[idx]
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

prepare_roc_plot_curve <- function(fpr, tpr) {
  df0 <- data.frame(FPR = fpr, TPR = tpr)
  df0 <- df0[is.finite(df0$FPR) & is.finite(df0$TPR), ]
  
  df0$FPR <- pmin(pmax(df0$FPR, 0), 1)
  df0$TPR <- pmin(pmax(df0$TPR, 0), 1)
  
  df0 <- df0[order(df0$FPR, df0$TPR), ]
  rownames(df0) <- NULL
  
  return(df0)
}

prepare_roc_interp_curve <- function(fpr, tpr) {
  df0 <- data.frame(FPR = fpr, TPR = tpr)
  df0 <- df0[is.finite(df0$FPR) & is.finite(df0$TPR), ]
  
  df0$FPR <- pmin(pmax(df0$FPR, 0), 1)
  df0$TPR <- pmin(pmax(df0$TPR, 0), 1)
  
  df1 <- aggregate(TPR ~ FPR, data = df0, FUN = max)
  df1 <- df1[order(df1$FPR), ]
  
  if (min(df0$FPR) == 0) {
    df1$TPR[df1$FPR == 0] <- min(df0$TPR[df0$FPR == 0])
  }
  
  if (max(df0$FPR) == 1) {
    df1$TPR[df1$FPR == 1] <- max(df0$TPR[df0$FPR == 1])
  }
  
  rownames(df1) <- NULL
  
  return(df1)
}

prepare_pr_plot_curve <- function(recall, precision) {
  df0 <- data.frame(Recall = recall, Precision = precision)
  df0 <- df0[is.finite(df0$Recall) & is.finite(df0$Precision), ]
  
  df0$Recall <- pmin(pmax(df0$Recall, 0), 1)
  df0$Precision <- pmin(pmax(df0$Precision, 0), 1)
  
  df0 <- df0[!(df0$Recall == 1 & df0$Precision == 0), ]
  
  df1 <- aggregate(Precision ~ Recall, data = df0, FUN = max)
  df1 <- df1[order(df1$Recall), ]
  rownames(df1) <- NULL
  
  return(df1)
}

# ===============================
# Model training function
# ===============================

run_model <- function(method_name, model_name, x_data, y_data, tr_ctrl) {
  
  if (method_name == "rf") {
    
    fit <- train(
      x = x_data,
      y = y_data,
      method = "rf",
      trControl = tr_ctrl,
      preProcess = c("center", "scale"),
      ntree = 1500,
      metric = "Kappa"
    )
    
    pred_df <- fit$pred
    pred_df <- pred_df[pred_df$mtry == fit$bestTune$mtry, ]
    
  } else if (method_name == "lda") {
    
    fit <- train(
      x = x_data,
      y = y_data,
      method = "lda",
      trControl = tr_ctrl,
      preProcess = c("center", "scale", "pca"),
      metric = "Kappa"
    )
    
    pred_df <- fit$pred
    
  } else if (method_name == "nnet") {
    
    fit <- train(
      x = x_data,
      y = y_data,
      method = "nnet",
      trControl = tr_ctrl,
      preProcess = c("center", "scale"),
      tuneLength = 10,
      metric = "Kappa",
      trace = FALSE,
      MaxNWts = 10000,
      maxit = 1000
    )
    
    pred_df <- fit$pred
    pred_df <- pred_df[
      pred_df$size == fit$bestTune$size &
        pred_df$decay == fit$bestTune$decay,
    ]
    
  } else {
    
    stop("Unknown model type.")
  }
  
  pred_df$obs <- factor(pred_df$obs, levels = levels(y_data))
  pred_df$pred <- factor(pred_df$pred, levels = levels(y_data))
  
  conf_mat <- confusionMatrix(pred_df$pred, pred_df$obs)
  by_class <- as.data.frame(conf_mat$byClass)
  
  precision <- by_class$`Pos Pred Value`
  recall <- by_class$Sensitivity
  f1 <- 2 * precision * recall / (precision + recall)
  
  metrics_class <- data.frame(
    Model = model_name,
    Class = rownames(by_class),
    Precision = precision,
    Recall = recall,
    F1 = f1,
    Balanced_Accuracy = by_class$`Balanced Accuracy`
  )
  
  summary_metrics <- data.frame(
    Model = model_name,
    Accuracy = as.numeric(conf_mat$overall["Accuracy"]),
    Macro_F1 = mean(f1, na.rm = TRUE)
  )
  
  class_levels <- levels(y_data)
  
  agg_df <- aggregate(
    pred_df[, class_levels, drop = FALSE],
    by = list(rowIndex = pred_df$rowIndex, obs = pred_df$obs),
    FUN = mean
  )
  
  prob_mat <- as.matrix(agg_df[, class_levels, drop = FALSE])
  
  true_mat <- sapply(class_levels, function(class_name) {
    ifelse(agg_df$obs == class_name, 1, 0)
  })
  
  true_mat <- as.matrix(true_mat)
  colnames(true_mat) <- class_levels
  
  roc_plot_list <- list()
  pr_plot_list <- list()
  roc_metric_list <- list()
  pr_metric_list <- list()
  roc_interp_list <- list()
  pr_interp_list <- list()
  
  fpr_grid <- seq(0, 1, length.out = 1001)
  recall_grid <- seq(0, 1, length.out = 1001)
  
  for (i in seq_along(class_levels)) {
    
    class_name <- class_levels[i]
    y_true <- true_mat[, i]
    y_score <- prob_mat[, i]
    
    roc_obj <- roc(response = y_true, predictor = y_score, quiet = TRUE)
    roc_auc <- as.numeric(auc(roc_obj))
    
    roc_df0 <- data.frame(
      FPR = 1 - roc_obj$specificities,
      TPR = roc_obj$sensitivities
    )
    
    roc_df1 <- prepare_roc_plot_curve(roc_df0$FPR, roc_df0$TPR)
    roc_interp_df <- prepare_roc_interp_curve(roc_df0$FPR, roc_df0$TPR)
    
    roc_df1$Model <- model_name
    roc_df1$Group <- class_name
    roc_df1$Curve_Type <- "Class"
    roc_plot_list[[class_name]] <- roc_df1
    
    roc_metric_list[[class_name]] <- data.frame(
      Model = model_name,
      Group = class_name,
      Curve_Type = "Class",
      ROC_AUC = roc_auc
    )
    
    roc_interp_y <- approx(
      x = roc_interp_df$FPR,
      y = roc_interp_df$TPR,
      xout = fpr_grid,
      method = "linear",
      yleft = roc_interp_df$TPR[1],
      yright = roc_interp_df$TPR[nrow(roc_interp_df)],
      ties = max
    )$y
    
    roc_interp_list[[class_name]] <- roc_interp_y
    
    pr_obj <- pr.curve(
      scores.class0 = y_score[y_true == 1],
      scores.class1 = y_score[y_true == 0],
      curve = TRUE
    )
    
    pr_auprc <- pr_obj$auc.integral
    
    pr_df0 <- data.frame(
      Recall = pr_obj$curve[, 1],
      Precision = pr_obj$curve[, 2]
    )
    
    pr_df1 <- prepare_pr_plot_curve(pr_df0$Recall, pr_df0$Precision)
    
    pr_df1$Model <- model_name
    pr_df1$Group <- class_name
    pr_df1$Curve_Type <- "Class"
    pr_plot_list[[class_name]] <- pr_df1
    
    pr_metric_list[[class_name]] <- data.frame(
      Model = model_name,
      Group = class_name,
      Curve_Type = "Class",
      PR_AUPRC = pr_auprc
    )
    
    pr_interp_y <- approx(
      x = pr_df1$Recall,
      y = pr_df1$Precision,
      xout = recall_grid,
      method = "linear",
      yleft = pr_df1$Precision[1],
      yright = pr_df1$Precision[nrow(pr_df1)],
      ties = max
    )$y
    
    pr_interp_list[[class_name]] <- pr_interp_y
  }
  
  y_true_micro <- as.vector(true_mat)
  y_score_micro <- as.vector(prob_mat)
  
  roc_micro <- roc(
    response = y_true_micro,
    predictor = y_score_micro,
    quiet = TRUE
  )
  
  roc_micro_auc <- as.numeric(auc(roc_micro))
  
  roc_micro_df <- data.frame(
    FPR = 1 - roc_micro$specificities,
    TPR = roc_micro$sensitivities
  )
  
  roc_micro_df <- prepare_roc_plot_curve(
    roc_micro_df$FPR,
    roc_micro_df$TPR
  )
  roc_micro_df$Model <- model_name
  roc_micro_df$Group <- "Micro"
  roc_micro_df$Curve_Type <- "Micro"
  
  pr_micro <- pr.curve(
    scores.class0 = y_score_micro[y_true_micro == 1],
    scores.class1 = y_score_micro[y_true_micro == 0],
    curve = TRUE
  )
  
  pr_micro_auprc <- pr_micro$auc.integral
  
  pr_micro_df <- data.frame(
    Recall = pr_micro$curve[, 1],
    Precision = pr_micro$curve[, 2]
  )
  
  pr_micro_df <- prepare_pr_plot_curve(
    pr_micro_df$Recall,
    pr_micro_df$Precision
  )
  pr_micro_df$Model <- model_name
  pr_micro_df$Group <- "Micro"
  pr_micro_df$Curve_Type <- "Micro"
  
  roc_macro_tpr <- rowMeans(do.call(cbind, roc_interp_list), na.rm = TRUE)
  roc_macro_auc <- trapz_manual(fpr_grid, roc_macro_tpr)
  
  roc_macro_df <- data.frame(
    FPR = fpr_grid,
    TPR = roc_macro_tpr,
    Model = model_name,
    Group = "Macro",
    Curve_Type = "Macro"
  )
  
  pr_macro_precision <- rowMeans(do.call(cbind, pr_interp_list), na.rm = TRUE)
  pr_macro_auprc <- trapz_manual(recall_grid, pr_macro_precision)
  
  pr_macro_df <- data.frame(
    Recall = recall_grid,
    Precision = pr_macro_precision,
    Model = model_name,
    Group = "Macro",
    Curve_Type = "Macro"
  )
  
  roc_metrics <- rbind(
    do.call(rbind, roc_metric_list),
    data.frame(
      Model = model_name,
      Group = "Micro",
      Curve_Type = "Micro",
      ROC_AUC = roc_micro_auc
    ),
    data.frame(
      Model = model_name,
      Group = "Macro",
      Curve_Type = "Macro",
      ROC_AUC = roc_macro_auc
    )
  )
  
  pr_metrics <- rbind(
    do.call(rbind, pr_metric_list),
    data.frame(
      Model = model_name,
      Group = "Micro",
      Curve_Type = "Micro",
      PR_AUPRC = pr_micro_auprc
    ),
    data.frame(
      Model = model_name,
      Group = "Macro",
      Curve_Type = "Macro",
      PR_AUPRC = pr_macro_auprc
    )
  )
  
  roc_plot_df <- rbind(
    do.call(rbind, roc_plot_list),
    roc_micro_df,
    roc_macro_df
  )
  
  pr_plot_df <- rbind(
    do.call(rbind, pr_plot_list),
    pr_micro_df,
    pr_macro_df
  )
  
  return(list(
    fit = fit,
    pred = pred_df,
    conf_mat = conf_mat,
    metrics_class = metrics_class,
    summary_metrics = summary_metrics,
    roc_metrics = roc_metrics,
    pr_metrics = pr_metrics,
    roc_plot_df = roc_plot_df,
    pr_plot_df = pr_plot_df
  ))
}

# ===============================
# Train models
# ===============================

rf_res <- run_model(
  method_name = "rf",
  model_name = "RF",
  x_data = features,
  y_data = labels,
  tr_ctrl = train_control
)

lda_res <- run_model(
  method_name = "lda",
  model_name = "LDA",
  x_data = features,
  y_data = labels,
  tr_ctrl = train_control
)

nnet_res <- run_model(
  method_name = "nnet",
  model_name = "NNET",
  x_data = features,
  y_data = labels,
  tr_ctrl = train_control
)

print(rf_res$fit)
print(lda_res$fit)
print(nnet_res$fit)

print(rf_res$conf_mat)
print(lda_res$conf_mat)
print(nnet_res$conf_mat)

# ===============================
# Export model metrics
# ===============================

summary_all <- rbind(
  rf_res$summary_metrics,
  lda_res$summary_metrics,
  nnet_res$summary_metrics
)

metrics_class_all <- rbind(
  rf_res$metrics_class,
  lda_res$metrics_class,
  nnet_res$metrics_class
)

roc_metrics_all <- rbind(
  rf_res$roc_metrics,
  lda_res$roc_metrics,
  nnet_res$roc_metrics
)

pr_metrics_all <- rbind(
  rf_res$pr_metrics,
  lda_res$pr_metrics,
  nnet_res$pr_metrics
)

auc_auprc_all <- merge(
  roc_metrics_all,
  pr_metrics_all,
  by = c("Model", "Group", "Curve_Type"),
  all = TRUE
)

auc_auprc_all$ROC_AUC <- round(auc_auprc_all$ROC_AUC, 6)
auc_auprc_all$PR_AUPRC <- round(auc_auprc_all$PR_AUPRC, 6)

print(summary_all)
print(metrics_class_all)
print(auc_auprc_all)

write.csv(summary_all, "overall_metrics_RF_LDA_NNET.csv", row.names = FALSE)
write.csv(metrics_class_all, "class_metrics_RF_LDA_NNET.csv", row.names = FALSE)
write.csv(as.data.frame(rf_res$conf_mat$table), "confusion_matrix_RF.csv", row.names = FALSE)
write.csv(as.data.frame(lda_res$conf_mat$table), "confusion_matrix_LDA.csv", row.names = FALSE)
write.csv(as.data.frame(nnet_res$conf_mat$table), "confusion_matrix_NNET.csv", row.names = FALSE)
write.csv(roc_metrics_all, "ROC_AUC_values_RF_LDA_NNET.csv", row.names = FALSE)
write.csv(pr_metrics_all, "PR_AUPRC_values_RF_LDA_NNET.csv", row.names = FALSE)
write.csv(auc_auprc_all, "ROC_AUC_PR_AUPRC_summary_RF_LDA_NNET.csv", row.names = FALSE)

# ===============================
# Prepare confusion matrix data
# ===============================

make_cm_df <- function(conf_obj, model_name, class_levels) {
  
  cm_df <- as.data.frame(conf_obj$table)
  colnames(cm_df) <- c("Prediction", "Reference", "Freq")
  
  cm_df$Model <- model_name
  
  cm_df$Reference <- factor(cm_df$Reference, levels = class_levels)
  cm_df$Prediction <- factor(cm_df$Prediction, levels = class_levels)
  
  cm_df$Percent <- ave(
    cm_df$Freq,
    cm_df$Reference,
    FUN = function(x) {
      if (sum(x) == 0) {
        rep(0, length(x))
      } else {
        x / sum(x) * 100
      }
    }
  )
  
  cm_df$Percent01 <- cm_df$Percent / 100
  cm_df$Label <- sprintf("%.1f%%", cm_df$Percent)
  
  return(cm_df)
}

class_levels <- levels(labels)

rf_cm_df <- make_cm_df(rf_res$conf_mat, "RF", class_levels)
lda_cm_df <- make_cm_df(lda_res$conf_mat, "LDA", class_levels)
nnet_cm_df <- make_cm_df(nnet_res$conf_mat, "NNET", class_levels)

cm_all_df <- rbind(
  rf_cm_df,
  lda_cm_df,
  nnet_cm_df
)

write.csv(
  cm_all_df,
  "confusion_matrix_percentage_plot_data_RF_LDA_NNET.csv",
  row.names = FALSE
)

# ===============================
# Confusion matrix plotting function
# ===============================

plot_cm_without_text <- function(cm_df, model_name, display_names) {
  
  p <- ggplot(
    cm_df,
    aes(x = Reference, y = Prediction, fill = Percent01)
  ) +
    geom_tile(color = NA) +
    scale_x_discrete(labels = display_names) +
    scale_y_discrete(labels = display_names) +
    scale_fill_gradientn(
      colors = rev(hcl.colors(10, "Blues")),
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1),
      name = "Percent"
    ) +
    labs(
      x = "True class",
      y = "Predicted class"
    ) +
    coord_equal() +
    theme_test() +
    theme(
      panel.border = element_rect(
        fill = NA,
        color = "black",
        linewidth = 1,
        linetype = "solid"
      ),
      text = element_text(size = 12, face = "plain", color = "black"),
      axis.title = element_text(size = 12, face = "plain", color = "black"),
      axis.text.x = element_text(
        size = 10,
        face = "plain",
        color = "black",
        angle = 45,
        hjust = 1
      ),
      axis.text.y = element_text(size = 10, face = "plain", color = "black"),
      legend.title = element_text(size = 10, face = "plain", color = "black"),
      legend.text = element_text(size = 10, face = "plain", color = "black")
    )
  
  print(p)
  
  ggsave(
    filename = paste0("Confusion_Matrix_", model_name, "_Percentage_No_Text.tiff"),
    plot = p,
    width = 4.5,
    height = 4,
    units = "in",
    dpi = 300
  )
  
  return(p)
}

# ===============================
# Export confusion matrix plots
# ===============================

p_cm_rf_notext <- plot_cm_without_text(rf_cm_df, "RF", display_names)
p_cm_lda_notext <- plot_cm_without_text(lda_cm_df, "LDA", display_names)
p_cm_nnet_notext <- plot_cm_without_text(nnet_cm_df, "NNET", display_names)

# ===============================
# Prepare ROC and PR data
# ===============================

roc_plot_all <- rbind(
  rf_res$roc_plot_df,
  lda_res$roc_plot_df,
  nnet_res$roc_plot_df
)

pr_plot_all <- rbind(
  rf_res$pr_plot_df,
  lda_res$pr_plot_df,
  nnet_res$pr_plot_df
)

all_groups <- c(levels(labels), "Micro", "Macro")

roc_plot_all$Group <- factor(roc_plot_all$Group, levels = all_groups)
pr_plot_all$Group <- factor(pr_plot_all$Group, levels = all_groups)

roc_plot_all$Model <- factor(roc_plot_all$Model, levels = c("RF", "LDA", "NNET"))
pr_plot_all$Model <- factor(pr_plot_all$Model, levels = c("RF", "LDA", "NNET"))

color_pool <- c(
  "#A8D5BA",
  "#F3E1A2",
  "#1597A5",
  "#FFC24B",
  "#BEB8DC",
  "#f7a1cc",
  "#FEB3AE",
  "#E7DAD2",
  "#999999"
)

group_colors <- setNames(
  rep(color_pool, length.out = length(all_groups)),
  all_groups
)

group_labels <- c(display_names, Micro = "Micro", Macro = "Macro")

model_linetypes <- c(
  RF = "solid",
  LDA = "dashed",
  NNET = "twodash"
)

# ===============================
# Plot ROC curves
# ===============================

p_roc <- ggplot(
  roc_plot_all,
  aes(
    x = FPR,
    y = TPR,
    color = Group,
    linetype = Model,
    group = interaction(Group, Model)
  )
) +
  geom_line(linewidth = 0.8) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "grey",
    linetype = "dotdash",
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = group_colors,
    labels = group_labels,
    drop = FALSE
  ) +
  scale_linetype_manual(
    values = model_linetypes,
    drop = FALSE
  ) +
  labs(
    x = "1-Specificity",
    y = "Sensitivity"
  ) +
  theme_test() +
  theme(
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 1,
      linetype = "solid"
    ),
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12, face = "plain", color = "black"),
    axis.title = element_text(size = 12, face = "plain", color = "black"),
    axis.text = element_text(size = 10, face = "plain", color = "black"),
    legend.text = element_text(size = 10, face = "plain", color = "black"),
    legend.justification = c(1, 0),
    legend.position = c(.7, .02),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = NA,
      linewidth = 0.5,
      linetype = "blank",
      colour = "black"
    )
  )

print(p_roc)

ggsave(
  filename = "ROC_Class_Micro_Macro_RF_LDA_NNET.tiff",
  plot = p_roc,
  width = 4.5,
  height = 4,
  units = "in",
  dpi = 300
)

# ===============================
# Plot PR curves
# ===============================

p_pr <- ggplot(
  pr_plot_all,
  aes(
    x = Recall,
    y = Precision,
    color = Group,
    linetype = Model,
    group = interaction(Group, Model)
  )
) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = group_colors,
    labels = group_labels,
    drop = FALSE
  ) +
  scale_linetype_manual(
    values = model_linetypes,
    drop = FALSE
  ) +
  labs(
    x = "Recall",
    y = "Precision"
  ) +
  theme_test() +
  theme(
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 1,
      linetype = "solid"
    ),
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12, face = "plain", color = "black"),
    axis.title = element_text(size = 12, face = "plain", color = "black"),
    axis.text = element_text(size = 10, face = "plain", color = "black"),
    legend.text = element_text(size = 10, face = "plain", color = "black"),
    legend.justification = c(1, 0),
    legend.position = c(.7, .02),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = NA,
      linewidth = 0.5,
      linetype = "blank",
      colour = "black"
    )
  )

print(p_pr)

ggsave(
  filename = "PR_Class_Micro_Macro_RF_LDA_NNET.tiff",
  plot = p_pr,
  width = 4.5,
  height = 4,
  units = "in",
  dpi = 300
)
