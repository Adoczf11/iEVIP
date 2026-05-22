rm(list = ls())

library(ggplot2)
library(reshape2)

data <- read.csv(
  file = "cell_EV.csv",
  header = TRUE,
  row.names = 1,
  sep = ",",
  stringsAsFactors = FALSE
)

# Convert data frame to matrix
data_matrix <- as.matrix(data)

# Set row and column names
rownames(data_matrix) <- paste("Row", 1:nrow(data_matrix), sep = "")
colnames(data_matrix) <- paste("Col", 1:ncol(data_matrix), sep = "")

# -------------------------
# 2. Define normalization function
# -------------------------
normalize <- function(x) {
  if (max(x) == min(x)) {
    return(rep(0, length(x)))
  } else {
    return((x - min(x)) / (max(x) - min(x)))
  }
}

# Apply normalization to each row
normalized_matrix <- t(apply(data_matrix, 1, normalize))

# Keep row and column names
rownames(normalized_matrix) <- rownames(data_matrix)
colnames(normalized_matrix) <- colnames(data_matrix)

# -------------------------
# 3. Reshape matrix into long-format data frame
# -------------------------
data_long <- melt(
  normalized_matrix,
  varnames = c("Row", "Col"),
  value.name = "Value"
)

# -------------------------
# 4. Draw heatmap
# -------------------------
p <- ggplot(data_long, aes(x = Col, y = Row, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#4575B4",
    mid = "white",
    high = "#D73027",
    midpoint = 0.5,
    limits = c(0, 1),
    na.value = "grey50"
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
    text = element_text(size = 8, face = "plain", color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 0, face = "plain", color = "black"),
    axis.text = element_text(size = 2, face = "plain", color = "black"),
    legend.title = element_text(size = 0, face = "plain", color = "black"),
    legend.text = element_text(size = 8, face = "plain", color = "black"),
    legend.background = element_blank(),
    legend.position = c(0.9, 0.1)
  )

print(p)

# -------------------------
# 5. Save heatmap
# -------------------------
ggsave(
  filename = "cell_heatmap.tiff",
  plot = p,
  width = 9,
  height = 4,
  units = "in",
  dpi = 300
)