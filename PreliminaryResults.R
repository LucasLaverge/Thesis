# packages

# Plot the data

# histogram of frequency and product culture scores
hist(df$product.score, breaks = 10, xlab = "ProductCulture", 
                                    ylab = "Frequency", 
                                    main = "Histogrem of Product Culture Scores",
                                    col = "lightblue")

# histogram of frequency and agile practices
hist(df$agile.score, breaks = 10, xlab = "AgilePractices", 
     ylab = "Frequency", 
     main = "Histogrem of Agile Practices",
     col = "lightblue")



























