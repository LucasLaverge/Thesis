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
  # Plot scores 
  plot(df$innovation.score, df$execution.score)

# K-means clustering  ----> is random at the moment, in final version set a seed so that we obtain a consistent result
  # Select the variables to use in clustering
  vars <- c("innovation.score", "execution.score")
  dataset <- df[vars]
  # Run k-means clustering with k=4
  # set.seed(12345)
  initial.centres <- matrix(c(1, 1,0.5, 0.5,
                              1,0.5,1, 0.5), ncol = 2)
  kmeans.result <- kmeans(dataset, centers = initial.centres) # with initial.centers
   # kmeans.result <- kmeans(dataset, centers = 4) # with set.seed
  
  # View the cluster assignments
  kmeans.result$cluster
  # View the centroids
  kmeans.result$centers
  # Get number of iterations
  print(kmeans.result$iter)
  
## PLOTTING --------------------------------------------------------
  cluster.names <- c("Leaders", "Innovators", "Executors", "Laggers")
  # Create a scatter plot of the clustering results
  ggplot(df, aes(x = innovation.score, y = execution.score, color = factor(kmeans.result$cluster))) +
    geom_point(size = 3) +
    scale_color_discrete(name = "Cluster", labels = cluster.names) +
    geom_point(data = as.data.frame(kmeans.result$centers),
               aes(x = innovation.score, y = execution.score),
               color = "black", size = 4, shape = 21) +
    ggtitle("K-means Clustering Results") +
    labs(x = "Innovation Score", y = "Execution Score")
  
# Plot initial state
  # Create a data frame with the coordinates of the additional points
  new.points <- data.frame(innovation.score = c(1, 1, 0.5, 0.5),
                           execution.score = c(1, 0.5, 1, 0.5))
  
  # Plot the data points and the additional points
  ggplot(df, aes(x = innovation.score, y = execution.score)) +
    geom_point(size = 3) +
    geom_point(data = new.points, aes(x = innovation.score, y = execution.score), color = "black", size = 4, shape= 21) +
    ggtitle("K-means Initial Cluster Centers")
  
  remove(vars, cluster.names, initial.centres, new.points)
  #-------------------------------------------------------- 
  summary(df$execution.score)
  summary(df$innovation.score)
  
############################
# Combine kmeans cluster data to original df
df <- df %>% mutate(cluster = kmeans.result$cluster)
# Create groups
  # Create new dataframe with only cluster 1 (Leaders)
  leaders.df <- filter(df, cluster == 1)
  summary(leaders.df$agile.score.abs)
  # Create new dataframe with only cluster 2 (Innovators)
  innovators.df <- filter(df, cluster == 2)
  summary(innovators.df$agile.score.abs)
  # Create new dataframe with only cluster 3 (Executors)
  executors.df <- filter(df, cluster == 3)
  summary(executors.df$agile.score.abs)
  # Create new dataframe with only cluster 4 (Laggers)
  laggers.df <- filter(df, cluster == 4)
  summary(laggers.df$agile.score.abs)
  
  
  # Plot per group the agile practice scores
  plot(leaders.df$agile.score, leaders.df$product.score)
  hist(leaders.df$agile.score.abs, breaks = 10, xlab = "Product Culture Score", 
       ylab = "Frequency", 
       main = "Histogram of Product Culture Scores",
       col = "lightblue")
  
  plot(innovators.df$agile.score, innovators.df$product.score)
  hist(innovators.df$agile.score.abs, breaks = 10, xlab = "Product Culture Score", 
       ylab = "Frequency", 
       main = "Histogram of Product Culture Scores",
       col = "lightblue")
  
  plot(executors.df$agile.score, executors.df$product.score)
  hist(executors.df$agile.score.abs, breaks = 10, xlab = "Product Culture Score", 
       ylab = "Frequency", 
       main = "Histogram of Product Culture Scores",
       col = "lightblue")
  
  plot(laggers.df$agile.score, laggers.df$product.score)
  hist(laggers.df$agile.score.abs, breaks = 10, xlab = "Product Culture Score", 
       ylab = "Frequency", 
       main = "Histogram of Product Culture Scores",
       col = "lightblue")
  
  
  
## Agile practices number -------------------------------------------
  ## LEADERS
  barplot((colSums(leaders.df[, col.names])/nrow(leaders.df)), 
          main = "LEADERS", 
          xlab = "Statements", 
          ylab = "# Agile Practices", 
          names.arg = c(paste0("q6.", 2:13)), 
          col = "lightblue",
          ylim = c(0, 7))
  
  ## LAGGERS
  barplot((colSums(laggers.df[, col.names])/nrow(laggers.df)), 
          main = "LAGGERS", 
          xlab = "Statements", 
          ylab = "# Agile Practices", 
          names.arg = c(paste0("q6.", 2:13)), 
          col = "lightblue",
          ylim = c(0, 7))
  
  ## EXECUTORS
  barplot((colSums(executors.df[, col.names])/nrow(executors.df)), 
          main = "EXECUTORS", 
          xlab = "Statements", 
          ylab = "# Agile Practices", 
          names.arg = c(paste0("q6.", 2:13)), 
          col = "lightblue",
          ylim = c(0, 7))
  
  ## INNOVATORS
  barplot((colSums(innovators.df[, col.names])/nrow(innovators.df)), 
          main = "INNOVATORS", 
          xlab = "Statements", 
          ylab = "# Agile Practices", 
          names.arg = c(paste0("q6.", 2:13)), 
          col = "lightblue",
          ylim = c(0, 7))

  
  
