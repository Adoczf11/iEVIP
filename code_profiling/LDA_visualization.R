
rm(list = ls())

library(MASS)
library(ggplot2)

# -------------------------
# 1. Set working directory and load data
# -------------------------

data <- read.csv(
  file = "cell_EV.csv",
  sep = ",",
  header = TRUE,
  check.names = FALSE
)

# -------------------------
# 2. Extract features and class labels
# -------------------------

nx <- 63
feature_data <- data[1:nx, 1:10]
group_label <- data[1:nx, 11]

head(feature_data)
head(group_label)
class(group_label)

# Convert class labels to factor
group_label <- as.factor(group_label)

# -------------------------
# 3. Perform linear discriminant analysis
# -------------------------

lda_model <- lda(feature_data, group_label)
print(lda_model)

# -------------------------
# 4. Predict class labels and generate confusion matrix
# -------------------------

prediction_result <- predict(lda_model, feature_data)

print(prediction_result$class)

# The predicted class labels can be used to generate a confusion matrix.
confusion_matrix <- table(group_label, prediction_result$class)
print(confusion_matrix)

# -------------------------
# 5. Project samples onto LDA space
# -------------------------

# Extract the LDA projection matrix
projection_matrix <- lda_model$scaling

# Project class means into the LDA space
projected_means <- lda_model$means %*% projection_matrix

# Calculate the weighted global mean in the LDA space
# The weights are the prior probabilities of each class.
global_projected_mean <- as.vector(lda_model$prior %*% projected_means)

n_samples <- nrow(feature_data)

# Project samples and center them in the LDA space
lda_scores <- as.matrix(feature_data) %*% projection_matrix -
  (rep(1, n_samples) %o% global_projected_mean)

head(lda_scores)

# Convert LDA scores to data frame for plotting
plot_df <- as.data.frame(lda_scores)
plot_df$Group <- group_label

# -------------------------
# 6. Plot LDA results
# -------------------------

p <- ggplot(plot_df, aes(x = LD1, y = LD2, color = Group)) +
  geom_point(size = 3) +
  stat_ellipse(
    aes(fill = Group),
    geom = "polygon",
    level = 0.95,
    alpha = 0.2,
    color = NA
  ) +
  scale_color_manual(
    values = c("#1597A5", "#FFC24B", "#FEB3AE")
  ) +
  scale_fill_manual(
    values = c("#1597A5", "#FFC24B", "#FEB3AE")
  ) +
  labs(
    x = "LD1",
    y = "LD2"
  ) +
  theme_test() +
  theme(
    panel.border = element_rect(
      color = "black",
      linewidth = 1,
      fill = NA
    ),
    text = element_text(size = 12, face = "plain", color = "black"),
    axis.title = element_text(size = 12, face = "plain", color = "black"),
    axis.text = element_text(size = 10, face = "plain", color = "black"),
    legend.title = element_text(size = 0, face = "plain", color = "black"),
    legend.text = element_text(size = 0, face = "plain", color = "black"),
    legend.background = element_blank(),
    legend.position = c(0.06, 0.12)
  )

print(p)

# -------------------------
# 7. Save the LDA plot
# -------------------------

ggsave(
  filename = "LDA_cell_visualization.tiff",
  plot = p,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)
