# Packages
library(broom)
library(psych)
library(GPArotation)

# Exploratory Factor Analysis of Execution culture ------------------------------
# select the variables for EFA
vars <- df[, paste0("E.", 1:27)]


# Do KMO and Barlett's test
  # Barlett's test
  bartlett <- bartlett.test(vars)
  print(bartlett$p.value)
  
  # KMO test
  kmo <- KMO(vars)
  print(kmo$MSA)
  print(kmo)

  
# EFA
  # Scree plot
  fa.parallel(vars, fm = "ml", fa = "fa")
  # EFA
  efa <- fa(vars, nfactors = 7, rotate = "varimax")
  ?fa
  # Print total variance explained
  efa$Vaccounted

# Print rotated component matrix
    print(efa$loadings,cutoff = 0.4) 


# Deleting case with cross-loadings


# Coefficient alphas

