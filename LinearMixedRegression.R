# Packages
library(nlme)


# Fit linear mixed model
model <- lme(sc2 ~ sc0 + sc1 + (1 | it), data = df)

# Summarize model results
summary(model)











































