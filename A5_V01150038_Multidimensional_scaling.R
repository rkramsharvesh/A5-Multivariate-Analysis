setwd("D:/Masters/VCU/Classes/SCMA/R/A5")

# Load packages
install_and_load <- function(pkgs) {
  for (p in pkgs) {
    if (!require(p, character.only = TRUE)) install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
install_and_load(c("tidyverse"))

# Load data
icecream <- read.csv("icecream.csv")
brand_names <- icecream$Brand
attr_data <- icecream[, -1]

# Compute distance matrix
dist_matrix <- dist(attr_data)

# Classical MDS with eigenvalues
mds_result <- cmdscale(dist_matrix, k = nrow(attr_data) - 1, eig = TRUE)

# Scree Plot (Eigenvalues)
eigen_df <- data.frame(Dim = 1:length(mds_result$eig), Eigenvalue = mds_result$eig)

ggplot(eigen_df, aes(x = Dim, y = Eigenvalue)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Scree Plot of MDS Eigenvalues",
       x = "Dimension", y = "Eigenvalue") +
  theme_minimal()

icecream <- read.csv("icecream.csv")
brand_names <- icecream$Brand
attr_data <- icecream[, -1]  # Drop 'Brand'

# --------------------------
# Compute Distance Matrix
# --------------------------
dist_matrix <- dist(attr_data)

# --------------------------
# Apply Classical MDS
# --------------------------
mds_result <- cmdscale(dist_matrix, k = 2, eig = TRUE)

# Create a DataFrame for plotting
mds_df <- as.data.frame(mds_result$points)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$Brand <- brand_names

# --------------------------
# Plot the MDS Map
# --------------------------
ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Brand)) +
  geom_point(size = 3, color = "blue") +
  geom_text(vjust = -1, size = 5) +
  labs(title = "MDS Map of Ice Cream Brands",
       x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()
