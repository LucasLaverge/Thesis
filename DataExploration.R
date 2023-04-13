# Packages
library(dplyr)
library(ggplot2)
library(likert)
library(tidyr)
library(broom)
library(psych)
library(GPArotation)

# TO DO 
# Change to % instead of total values


# Overview of respondents -------------------------------------------------------
  # Industry
  # Region
  # Profession 
  # Size
  # IT vs non-IT

# Summary of the dataset
summary(df)
# Count the number of respondents by Industry
df %>%
  group_by(industry) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = industry, y = count)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Respondents by Industry")

# Count the number of respondents by region
df %>%
  group_by(region) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = region, y = count)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Respondents by Region")


# Count the number of respondents by profession
df %>%
  group_by(job.title) %>%
  summarise(count = n())%>%
  ggplot(aes(x = job.title, y = count)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Respondents by Profession")


# Count the number of respondents by company size
df %>%
  group_by(size) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = size, y = count)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Respondents by Company Size")

# Count the number of respondents by IT or not
df %>%
  group_by(it) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = it, y = count)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Respondents by IT or Not")
#-------------------------------------------------------------------------------------------------------------- 

# Overview of variables -------------------------------------------------------
  # Look at distribution of answers for each statement
  # Fill in bias (overestimating own capabilities)

# Agile spread ----
  # Create a data frame with just the answers to the Likert scale questions
  likert.data.agile <- df[,c(6:17)]
  # Create a vector of all 7 levels
  levels <- c("1", "2", "3", "4", "5", "6", "7")
  # Convert the data to a factor format with all 7 levels for all columns
  for (col in 1:ncol(likert.data.agile)) {
    # Identify the unique response values in each column
    col.levels <- unique(likert.data.agile[,col])
    # Add any missing levels to the vector of levels
    missing.levels <- setdiff(levels, col.levels)
    levels <- sort(union(levels, missing.levels))
    # Convert the column to a factor format with all 7 levels
    likert.data.agile[,col] <- factor(likert.data.agile[,col], levels = levels)
  }
  # Convert the data to a likert format
  likert.data.agile <- likert(likert.data.agile)
  # Create a Likert scale plot
  plot(likert.data.agile, main = "Likert Scale Responses Agile Practices")
  # Remove 
  remove(likert.data.agile, col, col.levels, levels, missing.levels)
  
  
# Innovation spread ----
  # Create a data frame with just the answers to the Likert scale questions
  likert.data.innovation <- df[,18:38]
  # Create a vector of all 7 levels
  levels <- c("1", "2", "3", "4", "5")
  # Convert the data to a factor format with all 7 levels for all columns
  for (col in 1:ncol(likert.data.innovation)) {
    # Identify the unique response values in each column
    col.levels <- unique(likert.data.innovation[,col])
    # Add any missing levels to the vector of levels
    missing.levels <- setdiff(levels, col.levels)
    levels <- sort(union(levels, missing.levels))
    # Convert the column to a factor format with all 7 levels
    likert.data.innovation[,col] <- factor(likert.data.innovation[,col], levels = levels)
  }
  # Convert the data to a likert format
  likert.data.innovation <- likert(likert.data.innovation)
  # Create a Likert scale plot
  plot(likert.data.innovation, main = "Likert Scale Responses Innovation Practices")
  # Remove 
  remove(likert.data.innovation, col, col.levels, levels, missing.levels)
  
  
# Execution spread ----
  # Create a data frame with just the answers to the Likert scale questions
  likert.data.execution <- df[,39:65]
  # Create a vector of all 7 levels
  levels <- c("1", "2", "3", "4", "5")
  # Convert the data to a factor format with all 7 levels for all columns
  for (col in 1:ncol(likert.data.execution)) {
    # Identify the unique response values in each column
    col.levels <- unique(likert.data.execution[,col])
    # Add any missing levels to the vector of levels
    missing.levels <- setdiff(levels, col.levels)
    levels <- sort(union(levels, missing.levels))
    # Convert the column to a factor format with all 7 levels
    likert.data.execution[,col] <- factor(likert.data.execution[,col], levels = levels)
  }
  # Convert the data to a likert format
  likert.data.execution <- likert(likert.data.execution)
  # Create a Likert scale plot
  plot(likert.data.execution, main = "Likert Scale Responses Execution Practices")
  # Remove 
  remove(likert.data.execution, col, col.levels, levels, missing.levels)

#--------------------------------------------------------------------------------------------------------------  
# Overview of scores -------------------------------------------------------
  # Calculate abslolute scores
  df$innovation.score.abs = rowSums(df[, paste0("I.", 1:21)]) # innovation score calculation
  df$execution.score.abs = rowSums(df[, paste0("E.", 1:27)]) # execution score calculation
  df$product.score.abs = df$execution.score + df$innovation.score # product culture score (sum of innovation and execution)
  df$agile.score.abs = rowSums(df[, paste0("AP.", 1:12)]) #agile score calculation
  
  # Add relative scores
  df$innovation.score <- df$innovation.score.abs/(21*5)
  df$execution.score <- df$execution.score.abs / (27*5)
  plot(df$innovation.score, df$execution.score)
  df$product.score <- (df$innovation.score + df$execution.score) /2
  df$agile.score <- df$agile.score.abs/(12*7)
  
#Product Culture Scores
  # A histogram plot of the total scores and brief analysis of results derived from plot.
  hist(df$product.score, breaks = 10, xlab = "Product Culture Score", 
                         ylab = "Frequency", 
                         main = "Histogram of Product Culture Scores",
                         col = "lightblue")
  # Look at histograms when we adjust for random effects (IT main business?, location, size…)
  ggplot(df, aes(x = product.score)) +
    geom_histogram(bins = 10, color = "black", fill = "lightblue") +
    labs(x = "Product Culture Score", y = "Frequency",
         title = "Histogram of Product Culture Scores") +
    facet_wrap(~ it ) # Change this with region, industry, size etc.
  # Assess the plot distribution
  # Summary statistics of distribution
  summary(df$product.score)
  sd(df$product.score)

# Agile Practices Score
  # A histogram plot of the total agile practices and brief analysis of results derived from plot.
  hist(df$agile.score, breaks = 10, xlab = "Agile Practice Score", 
       ylab = "Frequency", 
       main = "Histogram of Agile Practices",
       col = "lightblue")
  # Look at histograms when we adjust for random effects (IT main business?, location, size…)
  ggplot(df, aes(x = agile.score)) +
    geom_histogram(bins = 10, color = "black", fill = "lightblue") +
    labs(x = "Agile Practices", y = "Frequency",
         title = "Histogram of Agile Practices Scores") +
    facet_wrap(~ it ) # Change this with region, industry, size etc.
  # Assess the plot distribution
  # Summary statistics of distribution
  summary(df$agile.score)
  sd(df$agile.score)
  

# Scatterplot Product Culture and Agile Practices
  # Look at distribution and assess whether we have a trend/correlation
  # Creation of linear regression 
  model1 <- lm(agile.score ~ product.score, data = df)
  summary(model1) # Summary
  cor(df$product.score, df$agile.score) # Correlation
  
  # Scatter plot of product culture scores and agile practices scores
  plot(df$product.score, df$agile.score, col = "darkblue")
  # Plot the regression line
  abline(model1, col = "darkred")
  # plot the regression formula
  formula <- substitute(paste("y = ", a + b * x), 
                        list(a = format(coef(model1)[1], digits = 2), 
                             b = format(coef(model1)[2], digits = 2)))
  xrange <- range(df$product.score) 
  ypos <- predict(model1, newdata = data.frame(product.score = mean(xrange)))
  text(mean(xrange), ypos, formula, pos = 2)
  remove(xrange, formula, ypos)

# Try to improve correlation with transformations
  # Other linear regression
  # In this linear regression we will use both innovation score and execution score as individual predictors
  # Innovation and execution as predictors
  model2 <- lm(agile.score ~ innovation.score + execution.score, data = df)
  summary(model2)
  # Innovation as predictor
  model3 <- lm(agile.score ~ innovation.score, data = df)
  summary(model3)
  # Execution as predictor
  model4 <- lm(agile.score ~ execution.score, data = df)
  summary(model4)
  plot(df$execution.score, df$agile.score, col = "darkblue")
  # Plot the regression line
  abline(model4, col = "darkred")
  
  
######## Remove outliers and look again #############
  #make two df with and without outliers?

# Anova analysis of scores -----------------------------------------------------------
  # bin the variable agile scores into 12 groups
  df$agile.binned <- cut(df$agile.score, breaks = 12)
  
  # summarize data by group
  summary.df <- df %>%
    group_by(agile.binned) %>%
    summarize(
      n = n(),
      mean_score = mean(agile.score),
      conf_interval = confint_tidy(lm(agile.score ~ 1), .alpha = 0.05)$conf.low %>% paste0(",", confint_tidy(lm(agile.score ~ 1), .alpha = 0.05)$conf.high)
    )
  
  # print summary table
  print(summary.df)
  
  # Remove
  # Delete the column named "agile.binned" from the original data frame
  df <- df[, !names(df) %in% c("agile.binned")]
  remove(summary.df, model1, model2, model3, model4)
#-------------------------------------------------------------------------------------------------------------- 
  
  