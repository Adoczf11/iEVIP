rm(list = ls())

library(ggplot2)
library(ggsignif)
library(ggpubr)
library(reshape2)
library(dplyr)


# Read input data
data <- read.csv(
  file = "Batch1_EV.csv",
  stringsAsFactors = FALSE
)

# Convert grouping variable to factor
data$group <- as.factor(data$group)

# Create a function to automatically identify numeric dependent variables
# and perform significance analysis
analyze_significance <- function(data, group_var) {
  
  # Automatically identify numeric variables
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  
  if (length(numeric_vars) < 1) {
    stop("No numeric variables were found in the dataset.")
  }
  
  # Perform significance analysis for each numeric variable
  results <- list()
  
  for (dependent_var in numeric_vars) {
    if (dependent_var != group_var) {
      
      result_i <- compare_means(
        formula = as.formula(paste(dependent_var, "~", group_var)),
        data = data
      )
      
      result_i$Feature <- dependent_var
      results[[dependent_var]] <- result_i
    }
  }
  
  return(results)
}

# Run significance analysis for all numeric variables against group
result_list <- analyze_significance(data, "group")

# Combine all results into one data frame
result_df <- bind_rows(result_list)

# Add comparison labels
result_df <- result_df %>%
  mutate(Comparison = paste(group1, "vs", group2))

# Save significance analysis results
write.csv(
  result_df,
  file = "20260420_clinical_sample_significance_results.csv",
  row.names = FALSE
)

# Print results
print(result_df)

set.seed(123)

# Convert adjusted p-values into a matrix for heatmap plotting
p_value_matrix <- acast(
  result_df,
  Feature ~ Comparison,
  value.var = "p.adj"
)

# Convert significance labels into a matrix
significance_matrix <- acast(
  result_df,
  Feature ~ Comparison,
  value.var = "p.signif"
)

# Convert matrices to long format
data_long <- melt(
  p_value_matrix,
  varnames = c("Feature", "Comparison"),
  value.name = "Value"
)

significance_long <- melt(
  significance_matrix,
  varnames = c("Feature", "Comparison"),
  value.name = "Significance"
)

# Merge p-value data and significance labels
data_long <- merge(
  data_long,
  significance_long,
  by = c("Feature", "Comparison")
)

# Draw heatmap and add significance labels
p <- ggplot(data_long, aes(x = Comparison, y = Feature, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#4575B4",
    mid = "white",
    high = "#D73027",
    midpoint = 0.5,
    limits = c(0, 1),
    na.value = "grey50",
    name = "Adjusted p-value"
  ) +
  geom_text(aes(label = Significance), color = "black", size = 3) +
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
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8,
      face = "plain",
      color = "black"
    ),
    axis.text.y = element_text(
      size = 8,
      face = "plain",
      color = "black"
    ),
    axis.title = element_blank(),
    legend.title = element_text(
      size = 8,
      face = "plain",
      color = "black"
    ),
    legend.text = element_text(
      size = 8,
      face = "plain",
      color = "black"
    ),
    legend.background = element_blank(),
    legend.position = c(0.9, 0.1)
  )

print(p)

# Save heatmap
ggsave(
  filename = "clinical_sample_significance_heatmap.tiff",
  plot = p,
  width = 4,
  height = 8,
  units = "in",
  dpi = 300
)