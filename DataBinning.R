#packages
library(ggplot2)


############################################################################
########################## BIN DATA #######################################
############################################################################
#bin product culture scores
df$score.bin = cut(df$sc1, breaks = c(-Inf,82,94,106,118,130,142,154,166,178,190,202,214,+Inf), 
                   labels = c("bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13"))

df$innovation.score = rowSums(df[, paste0("q4.", 2:22)]) # innovation score calculation
df$execution.score = rowSums(df[, paste0("q5.", 2:28)]) # execution score calculation
df$product.score = df$execution.score + df$innovation.score # product culture score (sum of innovation and execution)
df$agile.score = rowSums(df[, paste0("q6.", 2:13)]) #agile score calculation


plot(df$innovation.score, df$execution.score)
# Create the plot
ggplot(df, aes(innovation.score, execution.score)) +
  geom_point() + # Add points
  scale_x_continuous(limits = c(0, 120)) + # Set the x-axis limits
  scale_y_continuous(limits = c(0, 120)) # Set the y-axis limits







