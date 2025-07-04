setwd("D:/Masters/VCU/Classes/SCMA/R/A5")

# ---------------------------
# Install & Load Required Packages
# ---------------------------
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}
packages <- c("dplyr", "psych", "GPArotation", "factoextra", "cluster", "pheatmap")
install_and_load(packages)


survey_df <- read.csv("Survey.csv", header = TRUE)

# ---------------------------
# PART 1: PCA & FACTOR ANALYSIS
# ---------------------------

# Select Likert-scale columns (Q17–Q40)
sur_int <- survey_df[, 17:40]
sur_int <- data.frame(lapply(sur_int, as.numeric))

# Remove all-NA and zero-variance columns
sur_int <- sur_int[, colSums(is.na(sur_int)) < nrow(sur_int)]
sur_int <- sur_int[, apply(sur_int, 2, var, na.rm = TRUE) > 0]

# Mean imputation for missing values
for (i in 1:ncol(sur_int)) {
  sur_int[is.na(sur_int[, i]), i] <- mean(sur_int[, i], na.rm = TRUE)
}

# Scale the full data first
sur_scaled <- scale(sur_int)

# Run KMO and extract item-wise MSA scores
kmo_res <- KMO(sur_scaled)
msa_scores <- kmo_res$MSAi

# Create dataframe and plot MSA scores
library(ggplot2)
msa_df <- data.frame(
  Variable = names(msa_scores),
  MSA = round(msa_scores, 3)
)

ggplot(msa_df, aes(x = reorder(Variable, MSA), y = MSA)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0.6, linetype = "dashed", color = "red") +
  geom_text(aes(label = MSA), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "MSA (Measure of Sampling Adequacy) by Variable",
    x = "Variable",
    y = "MSA Score"
  ) +
  theme_minimal() +
  ylim(0, 1)

# Filter variables with MSA ≥ 0.6
valid_vars <- names(msa_scores[msa_scores >= 0.6])
sur_int_filtered <- sur_int[, valid_vars]
sur_scaled_filtered <- scale(sur_int_filtered)

# Recheck suitability with filtered data
cat("\nKMO (after filtering):\n")
print(KMO(sur_scaled_filtered))

cat("\nBartlett Test (after filtering):\n")
print(cortest.bartlett(cor(sur_scaled_filtered), n = nrow(sur_scaled_filtered)))

# Determine number of factors
fa.parallel(sur_scaled_filtered, fa = "fa")

# Final Factor Analysis with updated factor count
fa_res <- fa(sur_scaled_filtered, nfactors = 2, rotate = "varimax")
print(fa_res$loadings, cutoff = 0.4, sort = TRUE)
fa.diagram(fa_res)

# Factor reliability (Omega)
omega_res <- omega(sur_scaled_filtered, n.obs = nrow(sur_scaled_filtered))
print(omega_res)
# ---------------------------
# PART 2: CLUSTER ANALYSIS (Optimized)
# ---------------------------

# Use the filtered PCA/FA variables only
cluster_vars <- as.data.frame(sur_scaled_filtered)

# Standardize (if not already scaled)
# cluster_vars <- scale(cluster_vars)  # skip if already scaled

# Determine optimal number of clusters
fviz_nbclust(cluster_vars, kmeans, method = "wss")

# Apply KMeans clustering (e.g., 3 clusters)
set.seed(123)
km_res <- kmeans(cluster_vars, centers = 3, nstart = 25)

# Add cluster labels to original dataset
survey_df$cluster <- km_res$cluster

# Cluster visualization
fviz_cluster(km_res, data = cluster_vars, palette = "jco", ggtheme = theme_minimal())

# Cluster-wise mean profiles
aggregate(cluster_vars, by = list(Cluster = km_res$cluster), FUN = mean)

# Hierarchical clustering
hc_res <- hclust(dist(cluster_vars), method = "ward.D2")
fviz_dend(hc_res, k = 3, palette = "jco")

# Heatmap
pheatmap(t(cluster_vars), cutree_cols = 3)
