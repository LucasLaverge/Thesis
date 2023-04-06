#packages
library(ggplot2)


############################################################################
########################## BIN DATA #######################################
############################################################################
#bin product culture scores
df$score.bin = cut(df$sc1, breaks = c(-Inf,82,94,106,118,130,142,154,166,178,190,202,214,+Inf), 
                   labels = c("bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13"))

df$innovation.score = rowSums(df[,c("q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q4.8", "q4.9", "q4.10", 
                                            "q4.11", "q4.12", "q4.13", "q4.14", "q4.15", "q4.16", "q4.17", "q4.18", 
                                            "q4.19", "q4.20", "q4.21", "q4.22")])
df$product.score = rowSums(df[,c("q5.2","q5.3","q5.4","q5.5","q5.6","q5.7","q5.8","q5.9","q5.10","q5.11",
                                         "q5.12","q5.13","q5.14","q5.15","q5.16","q5.17","q5.18","q5.19","q5.20",
                                         "q5.21","q5.22","q5.23","q5.24","q5.25","q5.26","q5.27","q5.28")])




plot(df$innovation.culture.score, df$product.culture.score)
# Create the plot
ggplot(df, aes(innovation.culture.score, product.culture.score)) +
  geom_point() + # Add points
  scale_x_continuous(limits = c(0, 120)) + # Set the x-axis limits
  scale_y_continuous(limits = c(0, 120)) # Set the y-axis limits







