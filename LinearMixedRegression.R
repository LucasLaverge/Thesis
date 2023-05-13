# Packages
library(nlme)
library(robustbase)
library(lme4)
library(robustlmm)
library(MASS)



############################# NORMAL LINEAR REGRESSION ############################################
# Fit a robust linear regression model
    model.1 <- lm(agile.score ~ product.score, data = df)
    summary(model.1)
# Stargazer comparison of both regressiosn
    stargazer( model.1)
# Scatter plot of product culture scores and agile practices scores
plot(df$product.score, df$agile.score, col = "darkblue")
# Plot the regression line
abline(model.1, col = "darkred")
# plot the regression formula
formula <- substitute(paste("y = ", a + b * x), 
                      list(a = format(coef(model.1)[1], digits = 2), 
                           b = format(coef(model.1)[2], digits = 2)))
xrange <- range(df$product.score) 
ypos <- predict(model.1, newdata = data.frame(product.score = mean(xrange)))
text(mean(xrange), ypos+0.03, formula, pos = 2)
remove(xrange, formula, ypos)

########################### Regression with dimension scores instead of overal score ###############################

## Calculate Innovation Factor scores
df$IF.1 <-  rowSums(df[, c("I.1", "I.2", "I.3")]) / 3
df$IF.2 <-  rowSums(df[, c("I.4", "I.5", "I.6")]) / 3
df$IF.3 <-  rowSums(df[, c("I.7", "I.8", "I.9")]) / 3
df$IF.4 <-  rowSums(df[, c("I.10", "I.11", "I.12")]) / 3
df$IF.5 <-  rowSums(df[, c("I.13", "I.14", "I.15")]) / 3
df$IF.6 <-  rowSums(df[, c("I.16", "I.17", "I.18")]) / 3
df$IF.7 <-  rowSums(df[, c("I.19", "I.20", "I.21")]) / 3

## Calculate Execution Factor Scores
df$EF.1 <- rowSums(df[, c("E.1", "E.2")]) / 2
df$EF.2 <- rowSums(df[, c("E.7", "E.10", "E.11", "E.12", "E.3" )]) / 5
df$EF.3 <- rowSums(df[, c("E.13", "E.14", "E.17")]) / 3
df$EF.4 <- rowSums(df[, c("E.15", "E.16")]) / 2
df$EF.5 <- rowSums(df[, c("E.24", "E.23", "E.25", "E.21", "E.22")]) / 5
df$EF.6 <- rowSums(df[, c("E.19", "E.20", "E.18", "E.27")]) / 4


## Linear regression model1.1
model.2 <- lm(agile.score ~ IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6, data = df)
summary(model.2)
stargazer(model.2)
AIC(model.2)
BIC(model.2)

model.2.2 <- lm(agile.score ~ IF.1 + IF.3 + EF.1, data = df)
summary(model.2.2)
stargazer(model.2.2)


## Robust model
model.3 <- rlm(agile.score ~ IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6, data=df)
summary(model.3)
stargazer(model.3)

stargazer(model.2, model.3)


############################# MIXED LINEAR REGRESSION ############################################
## Mixed Linear Regression wiht one fixed effect
# Industry
model.4 <- lmer(agile.score ~  IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + 
                EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6 + (1 | industry), data = df)
AIC(model.4)
BIC(model.4)
summary(model.4)
ranef(model.4)

#Size
model.5 <- lmer(agile.score ~  IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + 
                    EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6 + (1 | size), data = df)
AIC(model.5)
BIC(model.5)
summary(model.5)
ranef(model.5)


#IT 
model.6 <- lmer(agile.score ~ IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + 
                    EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6 + (1 | it), data = df)
AIC(model.6)
BIC(model.6)
summary(model.6)
ranef(model.6)

# Region
model.7 <- lmer(agile.score ~  IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + 
                   EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6 + (1 | region), data = df)
AIC(model.7)
BIC(model.7)
summary(model.7)
ranef(model.7)

stargazer(model.4, model.5, model.6, model.7)
# Compare the models using likelihood ratio test
anova(model.4, model.5, model.6, model.7)


## Mixed Linear Regression wiht multiple fixed effects

model.8 <- lmer(agile.score ~ IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + 
                  EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6 + (1 | size/it), data = df)
AIC(model.8)
BIC(model.8)
summary(model.8)
stargazer(model.8)
ranef(model.8)



































