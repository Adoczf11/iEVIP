# Clear the global environment
rm(list = ls())

# Load required packages
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(Cairo)

# -------------------------
# 1. Read input data
# -------------------------
setwd("PCA")

data <- read.csv(
  file = "cell_EV.csv",
  sep = ",",
  header = TRUE,
  check.names = FALSE
)

# Display the first few rows of the data
head(data)

# -------------------------
# 2. Perform principal component analysis
# -------------------------
# Exclude the 11th column from PCA
pca.res <- PCA(data[, -11], graph = FALSE, scale.unit = TRUE)

print(pca.res)

# Eigenvalues, percentage of variance, and cumulative variance
# get_eigenvalue(pca.res)

# Scree plot
# fviz_eig(pca.res, addlabels = TRUE)

# -------------------------
# 3. Variable-related PCA results
# -------------------------
# The following commands can be used to extract variable results:
# res.var <- get_pca_var(pca.res)
# res.var$cor      # Correlation between variables and principal components
# res.var$coord    # Coordinates of variables on principal components
# res.var$cos2     # Quality of representation
# res.var$contrib  # Contributions of variables

# Visualization of variable results
# fviz_pca_var(pca.res)

# Visualization of cos2 values
# library(corrplot)
# corrplot(res.var$cos2, is.corr = FALSE)

# Visualization of variable contributions
# corrplot(res.var$contrib, is.corr = FALSE)
# fviz_contrib(pca.res, choice = "var", axes = 1)
# fviz_contrib(pca.res, choice = "var", axes = 1:2)

# -------------------------
# 4. Individual-related PCA results
# -------------------------
# The following commands can be used to extract individual results:
# res.ind <- get_pca_ind(pca.res)
# head(res.ind$coord)
# head(res.ind$contrib)
# head(res.ind$cos2)

# Visualization of individual PCA results
# fviz_pca_ind(pca.res)

# -------------------------
# 5. Save PCA plot
# -------------------------
tiff(
  filename = "cell_PCA.tiff",
  width = 5,
  height = 4,
  units = "in",
  bg = "transparent",
  res = 300
)

p <- fviz_pca_ind(
  pca.res,
  geom.ind = "point",                  # Show points only
  pointsize = 3,
  pointshape = 19,
  col.ind = data$dign,                 # Color points by group
  palette = c("#1597A5", "#FFC24B", "#FEB3AE"),
  addEllipses = TRUE,                  # Add confidence ellipses
  legend.title = "",
  title = ""
) +
  theme_test() +
  theme(
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 1,
      linetype = "solid"
    ),
    text = element_text(size = 0, face = "plain", color = "black"),
    axis.title = element_text(size = 12, face = "plain", color = "black"),
    axis.text = element_text(size = 10, face = "plain", color = "black"),
    legend.title = element_text(size = 11, face = "plain", color = "black"),
    legend.text = element_text(size = 0, face = "plain", color = "black"),
    legend.background = element_blank(),
    legend.position = c(0.75, 0.15)
  )

print(p)

# Close the graphics device
dev.off()