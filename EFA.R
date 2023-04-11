# Packages
library(broom)
library(psych)
library(GPArotation)

# Exploratory Factor Analysis of Execution culture ------------------------------
# select the variables for EFA
vars <- df[, paste0("q5.", 2:28)]

# perform EFA with 2 factors
efa.result <- fa(vars, nfactors = 6)

# view the EFA results
print(efa.result, sort=TRUE)

#---------------------------------------------------------------------------------