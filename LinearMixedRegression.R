# Packages
library(nlme)
library(lme4)

############################# NORMAL LINEAR REGRESSION ############################################
# First we look at the  normal linear regression between product score and agile practices score
model.1 <- lm(agile.score.abs ~ product.score, data = leaders.df)
summary(model.1) # Summary
cor(df$product.score, df$agile.score) # Correlation

# Scatter plot of product culture scores and agile practices scores
plot(df$product.score, df$agile.score.abs, col = "darkblue")
# Plot the regression line
abline(model.1, col = "darkred")
# plot the regression formula
formula <- substitute(paste("y = ", a + b * x), 
                      list(a = format(coef(model.1)[1], digits = 2), 
                           b = format(coef(model.1)[2], digits = 2)))
xrange <- range(df$product.score) 
ypos <- predict(model.1, newdata = data.frame(product.score = mean(xrange)))
text(mean(xrange), ypos, formula, pos = 2)
remove(xrange, formula, ypos)


############################# MIXED LINEAR REGRESSION ############################################

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
model1.1 <- lm(agile.score.abs ~ IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + 
                 EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6, data = df)
AIC(model1.1)
summary(model1.1)

## Mixed Linear Regression
# Industry
model2.1 <- lmer(agile.score.abs ~  IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + 
                EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6 + (1 | industry), data = df)
AIC(model2.1)
summary(model2.1)

#Size
model2.2 <- lmer(agile.score.abs ~  IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + 
                   EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6 + (1 | size), data = df)
AIC(model2.2)
summary(model2.2)

#IT 
model2.3 <-  lmer(agile.score.abs ~  IF.1 + IF.2 + IF.3 + IF.4 + IF.5 + IF.6 + IF.7 + 
                    EF.1 + EF.2 + EF.3 + EF.4 + EF.5 + EF.6 + (1 | it), data = df)
AIC(model2.3)
summary(model2.3)


stargazer(model.2, model.3)









































