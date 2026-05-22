rm(list = ls())

library(Rtsne)
library(ggplot2)
library(RColorBrewer)

# -------------------------
# 1. Set working directory
# -------------------------
setwd("t_SNE")

# -------------------------
# 2. Read input data
# -------------------------
data_raw <- read.csv(
  file = "cell_EV.csv",
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)

head(data_raw)
head(as.matrix(data_raw[, 1:10]))

# Extract feature matrix
feature_matrix <- as.matrix(data_raw[, 1:10])

# -------------------------
# 3. Run t-SNE
# -------------------------
set.seed(15)

tsne_out <- Rtsne(
  feature_matrix,
  perplexity = 20
)

head(tsne_out$Y)
str(tsne_out$Y)

# Check group information
str(data_raw$cell_type)

# -------------------------
# 4. Prepare plotting data
# -------------------------
plot_data <- data.frame(
  tsne_out$Y,
  data_raw$cell_type
)

colnames(plot_data) <- c(
  "t_DistributedY1",
  "t_DistributedY2",
  "Group"
)

# -------------------------
# 5. Draw t-SNE plot
# -------------------------
p <- ggplot(
  data = plot_data,
  aes(
    x = t_DistributedY1,
    y = t_DistributedY2,
    fill = Group
  )
) +
  geom_point(
    size = 3,
    colour = "white",
    alpha = 1,
    shape = 21
  ) +
  scale_fill_manual(
    values = c("#1597A5", "#FFC24B", "#FEB3AE")
  ) +
  theme_test() +
  theme(
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 1,
      linetype = "solid"
    ),
    text = element_text(size = 12, face = "plain", color = "black"),
    axis.title = element_text(size = 11, face = "plain", color = "black"),
    axis.text = element_text(size = 10, face = "plain", color = "black"),
    legend.title = element_text(size = 0, face = "plain", color = "black"),
    legend.text = element_text(size = 0, face = "plain", color = "black"),
    legend.background = element_blank(),
    legend.position = c(0.75, 0.12)
  )

print(p)

# -------------------------
# 6. Save the figure
# -------------------------
ggsave(
  filename = "tSNE_cell.tiff",
  plot = p,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

