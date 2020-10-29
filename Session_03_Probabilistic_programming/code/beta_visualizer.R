# libraries
library(ggplot2)
library(ggdist)
library(distributional)
library(tidyverse)

# set parameters
alpha <- 6
beta <- 8

# plot
df <- data.frame(dist = "beta")
ggplot(data = df, aes(y = 0, dist = dist, arg1 = alpha, arg2 = beta)) +
  stat_dist_slab(alpha = 0.5, fill = "#67a9cf", normalize = "none", scale = 1) +
  xlim(0, 1) +
  ylim(0, 3) +
  xlab("") +
  ylab("density") +
  theme_minimal()
