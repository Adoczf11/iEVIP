library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)

# -------------------------
# 1. Load CSV data
# -------------------------

data <- read_csv("features.csv")

batch <- data[[1]]
group <- data[[2]]
X <- as.matrix(data[, 3:ncol(data)])

# Remove rows containing missing values
idx_nan <- apply(X, 1, function(x) any(is.na(x))) | is.na(group)
X <- X[!idx_nan, ]
batch <- batch[!idx_nan]
group <- group[!idx_nan]

# -------------------------
# 2. Identify shared classes across all batches
# -------------------------

unique_batches <- unique(batch)
unique_groups <- unique(group)

common_group_mask <- rep(FALSE, length(group))

for (g in unique_groups) {
  has_all_batches <- all(
    sapply(unique_batches, function(b) any(group[batch == b] == g))
  )
  
  if (has_all_batches) {
    common_group_mask <- common_group_mask | (group == g)
  }
}

common_idx <- common_group_mask

# -------------------------
# 3. Perform batch correction for shared classes only
# -------------------------

X_corrected <- X

X_std <- X[common_idx, ]
mu_global <- colMeans(X_std)
sigma_global <- apply(X_std, 2, sd)
sigma_global[sigma_global == 0] <- 1

X_std <- sweep(X_std, 2, mu_global, "-")
X_std <- sweep(X_std, 2, sigma_global, "/")

# Remove batch-specific mean shifts
X_combat <- X_std

for (b in unique(batch[common_idx])) {
  idx <- which(batch[common_idx] == b)
  batch_mean <- colMeans(X_std[idx, ])
  X_combat[idx, ] <- sweep(X_std[idx, ], 2, batch_mean, "-")
}

# Update corrected values in the full feature matrix
X_corrected[common_idx, ] <- X_combat

# -------------------------
# 4. PCA visualization for shared classes only
# -------------------------

pca_raw <- prcomp(X_std, scale. = FALSE)
pca_corrected <- prcomp(X_combat, scale. = FALSE)

# Plot PCA before batch correction
df_raw <- data.frame(
  PC1 = pca_raw$x[, 1],
  PC2 = pca_raw$x[, 2],
  Batch = factor(batch[common_idx])
)

p1 <- ggplot(df_raw, aes(x = PC1, y = PC2, color = Batch)) +
  geom_point(size = 2) +
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
    axis.text = element_text(size = 10, face = "plain", color = "black"),
    legend.title = element_text(size = 11, face = "plain", color = "black"),
    legend.text = element_text(size = 11, face = "plain", color = "black"),
    legend.background = element_blank(),
    legend.position = "none"
  ) +
  coord_fixed()

print(p1)

ggsave(
  filename = "PCA_before_batch_correction.tiff",
  plot = p1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

# Plot PCA after batch correction
df_corrected <- data.frame(
  PC1 = pca_corrected$x[, 1],
  PC2 = pca_corrected$x[, 2],
  Batch = factor(batch[common_idx])
)

p2 <- ggplot(df_corrected, aes(x = PC1, y = PC2, color = Batch)) +
  geom_point(size = 2) +
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
    axis.text = element_text(size = 10, face = "plain", color = "black"),
    legend.title = element_text(size = 11, face = "plain", color = "black"),
    legend.text = element_text(size = 11, face = "plain", color = "black"),
    legend.background = element_blank(),
    legend.position = "none"
  ) +
  coord_fixed()

print(p2)

ggsave(
  filename = "PCA_after_batch_correction.tiff",
  plot = p2,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

# -------------------------
# 5. Save the full batch-corrected dataset
#    including both shared and unique classes
# -------------------------

df_out <- data.frame(
  Batch = batch,
  Group = group,
  X_corrected
)

write_csv(df_out, "Fig9G.csv")
