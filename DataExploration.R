# Packages
library(dplyr)
library(ggplot2)
library(likert)


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

# Overview of scores -------------------------------------------------------
  # Calculate scores
  df$innovation.score = rowSums(df[, paste0("q4.", 2:22)]) # innovation score calculation
  df$execution.score = rowSums(df[, paste0("q5.", 2:28)]) # execution score calculation
  df$product.score = df$execution.score + df$innovation.score # product culture score (sum of innovation and execution)
  df$agile.score = rowSums(df[, paste0("q6.", 2:13)]) #agile score calculation
  
  
#Product Culture Scores
  # A histogram plot of the total scores and brief analysis of results derived from plot.
  # Look at histograms when we adjust for random effects (IT main business?, location, size…)
  # Assess the plot distribution


# Scatterplot Product Culture and Agile Practices
  # Look at distribution and assess whether we have a trend/correlation
  # Try to improve correlation with transformations
  




