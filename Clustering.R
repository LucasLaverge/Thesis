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
  dataset <- df[c("innovation.score", "execution.score")]
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
  cluster.colors <- c("salmon","#008080", "thistle", "lightblue") # COLOR GRADING ADAPT ONLY HERE
  # Create a scatter plot of the clustering results
  ggplot(df, aes(x = innovation.score, y = execution.score, color = factor(kmeans.result$cluster))) +
    geom_point(size = 3) +
    scale_color_manual(name = "Cluster", labels = cluster.names, values = cluster.colors) +
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
  
  remove(initial.centres, new.points, dataset)
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
  var(leaders.df$agile.score.abs)
  summary(leaders.df$product.score.abs)
  summary(leaders.df$product.score)
  # Create new dataframe with only cluster 2 (Innovators)
  innovators.df <- filter(df, cluster == 2)
  summary(innovators.df$agile.score)
  var(innovators.df$agile.score.abs)
  summary(innovators.df$product.score.abs)
  summary(innovators.df$product.score)
  # Create new dataframe with only cluster 3 (Executors)
  executors.df <- filter(df, cluster == 3)
  summary(executors.df$agile.score)
  var(executors.df$agile.score.abs)
  summary(executors.df$product.score.abs)
  summary(executors.df$product.score)
  # Create new dataframe with only cluster 4 (Laggers)
  laggers.df <- filter(df, cluster == 4)
  summary(laggers.df$agile.score)
  var(laggers.df$agile.score.abs)
  summary(laggers.df$product.score.abs)
  summary(laggers.df$product.score)
  
  # Plot per group the agile practice scores
  plot(leaders.df$agile.score, leaders.df$product.score)
  hist(leaders.df$agile.score.abs, breaks = 10, xlab = "Product Culture Score", 
       ylab = "Frequency", 
       main = "Histogram of Product Culture Scores",
       col = cluster.colors[1])
  
  plot(innovators.df$agile.score, innovators.df$product.score)
  hist(innovators.df$agile.score.abs, breaks = 10, xlab = "Product Culture Score", 
       ylab = "Frequency", 
       main = "Histogram of Product Culture Scores",
       col = cluster.colors[3])
  
  plot(executors.df$agile.score, executors.df$product.score)
  hist(executors.df$agile.score.abs, breaks = 10, xlab = "Product Culture Score", 
       ylab = "Frequency", 
       main = "Histogram of Product Culture Scores",
       col = cluster.colors[2])
  
  plot(laggers.df$agile.score, laggers.df$product.score)
  hist(laggers.df$agile.score.abs, breaks = 10, xlab = "Product Culture Score", 
       ylab = "Frequency", 
       main = "Histogram of Product Culture Scores",
       col = cluster.colors[4])
  
  
  
## Agile practices number -------------------------------------------
  ## Calculate the # agile practices for each group
  leaders.n.practices <- colSums(leaders.df[, 6:17]) / nrow(leaders.df)
  laggers.n.practices <- colSums(laggers.df[, 6:17]) / nrow(laggers.df)
  executors.n.practices <- colSums(executors.df[, 6:17]) / nrow(executors.df)
  innovators.n.practices <- colSums(innovators.df[, 6:17]) / nrow(innovators.df)
  
  ## LEADERS
  bp.leaders <- barplot(leaders.n.practices, 
          main = "LEADERS", 
          xlab = "Statements", 
          ylab = "# Agile Practices", 
          names.arg = c(paste0("AP.", 1:12)), 
          col = cluster.colors[1],
          ylim = c(0, 7))
  # Add the bar values at the top of each bar
  for (i in 1:length(bp.leaders)) {
    text(
      x = bp.leaders[i], 
      y = leaders.n.practices[i] + 0.0001, 
      labels = round(leaders.n.practices[i], 1),
      pos = 3.5, cex = 0.8
    )
  }
  
  ## LAGGERS
  bp.laggers <- barplot(laggers.n.practices, 
          main = "LAGGERS", 
          xlab = "Statements", 
          ylab = "# Agile Practices", 
          names.arg = c(paste0("AP.", 1:12)), 
          col = cluster.colors[4],
          ylim = c(0, 7))
  # Add the bar values at the top of each bar
  for (i in 1:length(bp.laggers)) {
    text(
      x = bp.laggers[i], 
      y = laggers.n.practices[i] + 0.0001, 
      labels = round(laggers.n.practices[i], 1),
      pos = 3.5, cex = 0.8
    )
  }
  
  ## EXECUTORS
  bp.executors <- barplot(executors.n.practices, 
          main = "EXECUTORS", 
          xlab = "Statements", 
          ylab = "# Agile Practices", 
          names.arg = c(paste0("AP.", 1:12)), 
          col = cluster.colors[2],
          ylim = c(0, 7))
  # Add the bar values at the top of each bar
  for (i in 1:length(bp.executors)) {
    text(
      x = bp.executors[i], 
      y = executors.n.practices[i] + 0.0001, 
      labels = round(executors.n.practices[i], 1),
      pos = 3.5, cex = 0.8
    )
  }
  
  ## INNOVATORS
  bp.innovators <- barplot(innovators.n.practices, 
          main = "INNOVATORS", 
          xlab = "Statements", 
          ylab = "# Agile Practices", 
          names.arg = c(paste0("AP.", 1:12)), 
          col = cluster.colors[3],
          ylim = c(0, 7))
  # Add the bar values at the top of each bar
  for (i in 1:length(bp.innovators)) {
    text(
      x = bp.innovators[i], 
      y = innovators.n.practices[i] + 0.0001, 
      labels = round(innovators.n.practices[i], 1),
      pos = 3.5, cex = 0.8
    )
  }

  
######## Plot them next to eachother
  
  
# LEADERS VS LAGGERS
# Create the barplot
  bp1 <- barplot(rbind(leaders.n.practices, laggers.n.practices),
                beside = TRUE,
                main = "Agile Practices",
                xlab = "Statements",
                ylab = "# Agile Practices",
                names.arg = c(paste0("AP.", 1:12)),
                col = c(cluster.colors[1], cluster.colors[4]),
                ylim = c(0, 7),
                legend.text = c("Leaders", "Laggers"),
                args.legend = list(x = "topright", 
                                   fill = c(cluster.colors[1], cluster.colors[4])))
  # Add the bar values at the top of each bar
  for (i in 1:length(bp1)) {
    text(
      x = bp1[i], 
      y = rbind(leaders.n.practices, laggers.n.practices)[i] + 0.0001, 
      labels = round(rbind(leaders.n.practices, laggers.n.practices)[i], 1),
      pos = 3, cex = 0.8
    )
  }
  
  
# INNOVATORS VS EXECUTORS
  bp2 <- barplot(rbind(executors.n.practices, innovators.n.practices),
    beside = TRUE,
    main = "Agile Practices",
    xlab = "Statements",
    ylab = "# Agile Practices",
    names.arg = c(paste0("AP.", 1:12)),
    col = c(cluster.colors[2], cluster.colors[3]),
    ylim = c(0, 7),
    legend.text = c("Executors", "Innovators"),
    args.legend = list(x = "topright", 
                       fill = c(cluster.colors[2], cluster.colors[3])))
  # Add the bar values at the top of each bar
  for (i in 1:length(bp2)) {
    text(
      x = bp2[i], 
      y = rbind(executors.n.practices, innovators.n.practices)[i] + 0.0001, 
      labels = round(rbind(executors.n.practices, innovators.n.practices)[i], 1),
      pos = 3, cex = 0.8
    )
  }
  
# COMPARE AL FOUR OF THEM
  bp3 <- barplot(rbind(leaders.n.practices, executors.n.practices, innovators.n.practices, laggers.n.practices),
                 beside = TRUE,
                 main = "Agile Practices",
                 xlab = "Statements",
                 ylab = "# Agile Practices",
                 names.arg = c(paste0("AP.", 1:12)),
                 col = cluster.colors,
                 ylim = c(0, 7),
                 legend.text = c("Leaders", "Executors", "Innovators", "Laggers"),
                 args.legend = list(x = "topright",cex = 0.7,
                                    fill = cluster.colors))
  # Add the bar values at the top of each bar
  for (i in 1:length(bp3)) {
    text(
      x = bp3[i], 
      y = rbind(leaders.n.practices, executors.n.practices, innovators.n.practices, laggers.n.practices)[i] + 0.0001, 
      labels = round(rbind(leaders.n.practices,executors.n.practices, innovators.n.practices, laggers.n.practices)[i], 1),
      pos = 3.5, cex = 0.8
    )
  }
  
  remove(leaders.n.practices, executors.n.practices, laggers.n.practices, innovators.n.practices, i,
         bp.executors, bp.innovators, bp.laggers, bp.leaders) 
  
################## anova ##############################
product.scores.groups <- data.frame(
    scores = c(leaders.df$product.score, executors.df$product.score, innovators.df$product.score, laggers.df$product.score ),
    group = rep(c("Leaders", "Executors", "Innovators", "Laggers"),
                times = c(nrow(leaders.df), nrow(executors.df), nrow(innovators.df), nrow(laggers.df)))
  )

  model <- aov(scores ~ group, data = product.scores.groups)
  summary(model)
  
# Boxplots
  ggplot(product.scores.groups, aes(x = group, y = scores)) + 
    geom_boxplot()
  TukeyHSD(model)
  
  remove(model, product.scores.groups)
####################### IT vs NON-IT PIE CHARTS ######################
  # Create df with only IT companies
  df.it <- df %>% 
    filter(df$it == "Yes")
  # Create freq table
  freq.table.it <- table(df.it$cluster)
  # Create the pie chart
  pie(freq.table.it, labels = cluster.names, col = cluster.colors, main = "IT")
  
  # Create df with only  NON IT companies
  df.no.it <- df %>% 
    filter(df$it == "No")
  # Create freq table
  freq.table.no.it <- table(df.no.it$cluster)
  # Create the pie chart
  pie(freq.table.no.it, labels = cluster.names, col = cluster.colors, main = "NON IT")
  
  
  
  