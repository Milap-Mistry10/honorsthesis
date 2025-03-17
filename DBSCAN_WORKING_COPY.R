#######################################################################
# DBSCAN Algorithm Implementation (Modified Version)
#
# This script is based on the original DBSCAN implementation found at:
# https://github.com/mhahsler/dbscan/blob/master/R/dbscan.R
#
# The original implementation was developed by Michael Hahsler (2015)
# following the original DBSCAN formulation by Ester et al. (1996).
#
# License:
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; Version 3
#
# Modifications in this version:
# -----------------------------------
# 1. **Simplification of DBSCAN Execution**:
#    - Instead of implementing DBSCAN manually using kd-tree optimizations,
#      this version **relies on prebuilt functions** from the `dbscan` package.
#    - This change improves usability but removes low-level algorithmic control.
#
# 2. **Changes to Code Structure**:
#    - The script now **directly loads** required R libraries (`dbscan`, `ggplot2`, `readr`).
#    - Function calls have been revamped to work with `dbscan::dbscan()`
#      rather than a custom-coded DBSCAN algorithm.
#
# 3. **Removal of Detailed Algorithm Documentation**:
#    - The original script contained extensive comments on the mathematical
#      formulation of DBSCAN. These have been **removed for brevity**.
#    - Users are encouraged to refer to the original implementation if they
#      require those explanations.
#
# 4. **Visualization Enhancements**:
#    - New visualization functions using `ggplot2` have been added to analyze
#      clustering results.
#    - This was not part of the original `dbscan.R` implementation.
#
# 5. **Changes in Parameter Handling**:
#    - The modified script **simplifies the process of setting parameters (`eps`, `minPts`)**.
#    - The original script used **an internal kd-tree-based optimization**,
#      whereas this version lets the `dbscan` package handle clustering.
#
# 6. **License Compliance**:
#    - This modified version retains all rights and licensing conditions
#      under the **GNU General Public License v3**.
#    - The **full text of the GNU GPL license v3** is provided in the repository.
#
#######################################################################

# Load Necessary Libraries
library(dbscan)
library(ggplot2)
library(readr)

# Helper Functions
nobs <- function(x) {
  length(x$cluster)  # Total number of objects
}

ncluster <- function(x) {
  length(unique(x$cluster)) - (0 %in% x$cluster)  # Exclude cluster 0 (noise)
}

nnoise <- function(x) {
  sum(x$cluster == 0)  # Number of noise points (cluster 0)
}

# Set Working Directory and Load Data
path <- "/Users/mystery10/Desktop/R/tests/simulation_1"  # Update with your path
setwd(path)

# Read and Preprocess Data
data <- read_csv("normal_50_single_DetectionList_1.csv")
data <- as.matrix(data[, 1:3])  # Use the relevant columns

# Validate Data
if (!is.matrix(data)) {
  stop("Data must be a numeric matrix.")
}
if (anyNA(data)) {
  stop("Data contains missing values. Please handle NAs before clustering.")
}

# Scale Data for Uniformity (optional, but recommended)
data <- scale(data)

# Plot k-NN Distance to Determine eps
k <- 4  # k = minPts - 1; adjust based on expected minPts
kNNdistplot(data, k = k)
abline(h = 0.5, col = "red", lty = 2)  # Adjust h-value based on the "knee"

# Parameter Ranges
eps_values <- seq(0.1, 1, by = 0.1)  # Adjust eps range as needed
minPts_values <- 3:10                # Adjust minPts range as needed

# Storage for Results
results <- list()

# Nested Loop to Test DBSCAN with Different Parameters
for (eps in eps_values) {
  for (minPts in minPts_values) {
    # Run DBSCAN
    res <- dbscan(data, eps = eps, minPts = minPts)
    
    # Store Results
    results[[paste0("eps=", eps, "_minPts=", minPts)]] <- list(
      eps = eps,
      minPts = minPts,
      clusters = res$cluster,
      noise_points = sum(res$cluster == 0),
      num_clusters = length(unique(res$cluster)) - (0 %in% res$cluster)
    )
    
    # Print Progress
    cat("DBSCAN run: eps =", eps, ", minPts =", minPts, 
        "\nClusters =", results[[paste0("eps=", eps, "_minPts=", minPts)]]$num_clusters, 
        ", Noise points =", results[[paste0("eps=", eps, "_minPts=", minPts)]]$noise_points, "\n\n")
  }
}

# Summarize Results into a Data Frame
summary_results <- do.call(rbind, lapply(names(results), function(name) {
  data.frame(
    eps = results[[name]]$eps,
    minPts = results[[name]]$minPts,
    num_clusters = results[[name]]$num_clusters,
    noise_points = results[[name]]$noise_points
  )
}))

# Save Results to a CSV File
write.csv(summary_results, "dbscan_results_summary.csv", row.names = FALSE)

# Visualize the Effect of Parameters on Clustering
ggplot(summary_results, aes(x = eps, y = minPts, fill = num_clusters)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(summary_results$num_clusters)) +
  labs(title = "Effect of eps and minPts on Number of Clusters", x = "eps", y = "minPts", fill = "Clusters") +
  theme_minimal()

# Optional: Scatter Plot for Noise Points
ggplot(summary_results, aes(x = eps, y = minPts, size = noise_points, color = num_clusters)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "DBSCAN Parameter Tuning", x = "eps", y = "minPts", 
       size = "Noise Points", color = "Clusters") +
  theme_minimal()

# Example of Visualizing Clusters for Specific Parameters
selected_eps <- 0.5  # Example parameter
selected_minPts <- 4
res <- dbscan(data, eps = selected_eps, minPts = selected_minPts)
plot(data, col = res$cluster + 1L, pch = 20, cex = 1.5)
legend("topright", legend = unique(res$cluster), col = unique(res$cluster + 1L), pch = 20)

cat("Final run: eps =", selected_eps, ", minPts =", selected_minPts, 
    "\nClusters =", length(unique(res$cluster)) - (0 %in% res$cluster), 
    ", Noise points =", sum(res$cluster == 0), "\n")