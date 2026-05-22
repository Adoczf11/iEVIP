rm(list = ls())  # Clear the global environment

# Load required packages
library(mixOmics)  # Package for partial least squares discriminant analysis
library(ggplot2)   # Package for plotting

setwd("PLS_DA")  # Set working directory

data <- read.csv(
  file = "cell_EV.csv",
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)

otu <- data[, -c(ncol(data):ncol(data))]
group <- data[, c(1, ncol(data))]

# Perform PLS-DA
df_plsda <- plsda(otu, group$group, ncomp = 2)

# Simple plot
plotIndiv(
  df_plsda,
  comp = c(1, 2),
  group = group$group,
  style = "ggplot2",
  ellipse = TRUE,
  size.xlabel = 20,
  size.ylabel = 20,
  size.axis = 20,
  pch = 16,
  cex = 5
)

# ===============================
# Extract data for customized plotting
# ===============================

df <- unclass(df_plsda)

# Extract sample coordinates
df1 <- as.data.frame(df$variates$X)
df1$group <- group$group
df1$samples <- rownames(df1)

# Extract explained variance
explain <- df$prop_expl_var$X
x_label <- round(explain[1], digits = 3)
y_label <- round(explain[2], digits = 3)

# Define colors
col <- c("#1597A5", "#FFC24B", "#FEB3AE")

# Draw PLS-DA plot
p1 <- ggplot(
  df1,
  aes(x = comp1, y = comp2, color = group, shape = group)
) +
  theme_test() +
  geom_point(size = 3, shape = 19) +
  theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 0, lty = "dashed") +
  geom_hline(yintercept = 0, lty = "dashed") +
  # geom_text(
  #   aes(label = samples, y = comp2 + 0.4, x = comp1 + 0.5, vjust = 0),
  #   size = 3.5
  # ) +
  labs(
    x = paste0("PC1 [", x_label * 100, "%]"),
    y = paste0("PC2 [", y_label * 100, "%]")
  ) +
  stat_ellipse(
    data = df1,
    geom = "polygon",
    level = 0.95,
    linetype = 1,
    linewidth = 0.5,
    aes(fill = group),
    alpha = 0.2,
    show.legend = TRUE
  ) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = c("#1597A5", "#FFC24B", "#FEB3AE")) +
  theme(
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 1,
      linetype = "solid"
    ),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    panel.grid = element_blank(),
    legend.title = element_text(size = 0, face = "plain", color = "black"),
    legend.text = element_text(size = 0, face = "plain", color = "black"),
    legend.background = element_blank(),
    legend.position = c(0.10, 0.13)
  )

print(p1)

# Save the figure
ggsave(
  filename = "PLSDA_EV_cell.tiff",
  plot = p1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)