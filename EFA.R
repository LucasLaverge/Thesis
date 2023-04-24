# Packages
library(broom)
library(psych)
library(GPArotation)

# Exploratory Factor Analysis of Execution culture ------------------------------
# select the variables for EFA
vars <- df[, paste0("E.", 1:27)]


# Do KMO and Barlett's test
  bart_spher(vars) ###### produces Bartletts test of spherecity (you want this to be significant)
  KMO(vars)       ###### Kaiser-Meyer-Olkin measure, you want to be above .7

  
# EFA
  # Scree plot
  fa.parallel(vars, fm = "ml", fa = "pc")
  # EFA
  pca <- principal(vars, nfactors = 7, rotate = "varimax")
?principal
  # Print total variance explained
  pca$Vaccounted

# Print rotated component matrix
  print(pca$loadings,cutoff = 0.4) 
  # Print the compone


  
# Deleting case with cross-loadings


# Coefficient alphas

