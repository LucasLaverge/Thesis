# packages
library(dplyr)
library(ggplot2)
library(likert)
library(tidyr)
library(broom)
library(psych)
library(GPArotation)

# Categorization -----------------------------------------------------------------
# Create the plot
  ggplot(df, aes(innovation.score, execution.score)) +
    geom_point() + # Add points
    scale_x_continuous(limits = c(0, 120)) + # Set the x-axis limits
    scale_y_continuous(limits = c(0, 120)) # Set the y-axis limits
  # Plot scores 
  plot(df$innovation.score, df$execution.score)

# K-means clustering  ----> is random at the moment, in final version set a seed so that we obtain a consistent result
  # Select the variables to use in clustering
  vars <- c("innovation.score", "execution.score")
  dataset <- df[vars]
  # Run k-means clustering with k=3
  kmeans.result <- kmeans(dataset, centers = 4)
  # View the cluster assignments
  kmeans.result$cluster
  # View the centroids
  kmeans.result$centers
  # Create a scatter plot of the clustering results
  ggplot(df, aes(x = innovation.score, y = execution.score, color = factor(kmeans.result$cluster))) +
    geom_point(size = 3) +
    scale_color_discrete(name = "Cluster") +
    geom_point(data = as.data.frame(kmeans.result$centers),
               aes(x = innovation.score, y = execution.score),
               color = "black", size = 5, shape = 21) +
    ggtitle("K-means Clustering Results") +
    labs(x = "Innovation Score", y = "Execution Score")
  # Remove
  remove(vars, dataset, kmeans.result)

# Create groups
  
  # Plot per group the agile practice scores







