# Packages
library(dplyr)
library(ggplot2)


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





# Overview of scores -------------------------------------------------------
#Product Culture Scores
  # A histogram plot of the total scores and brief analysis of results derived from plot.
  # Look at histograms when we adjust for random effects (IT main business?, location, size…)
  # Assess the plot distribution


# Scatterplot Product Culture and Agile Practices
  # Look at distribution and assess whether we have a trend/correlation
  # Try to improve correlation with transformations




