#packages
library(ggplot2)

# Bin scores --------------------------------------------------------------------
#bin product culture scores
df$score.bin = cut(df$product.score, breaks = c(-Inf,82,94,106,118,130,142,154,166,178,190,202,214,+Inf), 
                   labels = c("bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13"))


# Plot scores --------------------------------------------------------------------
plot(df$innovation.score, df$execution.score)
# Create the plot
ggplot(df, aes(innovation.score, execution.score)) +
  geom_point() + # Add points
  scale_x_continuous(limits = c(0, 120)) + # Set the x-axis limits
  scale_y_continuous(limits = c(0, 120)) # Set the y-axis limits







