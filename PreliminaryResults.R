# packages

# General visualizations -----------------------------------------------------------

# histogram of frequency and product culture scores
hist(df$product.score, breaks = 10, xlab = "Product Culture Score", 
                                    ylab = "Frequency", 
                                    main = "Histogram of Product Culture Scores",
                                    col = "lightblue")

# histogram of frequency and agile practices
hist(df$agile.score, breaks = 10, xlab = "Agile Practice Score", 
     ylab = "Frequency", 
     main = "Histogram of Agile Practices",
     col = "lightblue")


# First linear regression -----------------------------------------------------------

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
  #mtext(formula, side = 3, line = -1.5) # place it on top

  
# Other linear regression -----------------------------------------------------------
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
























