# libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)

# helper function for plotting putting success in relation to sigma angle ------
# distances
precision <- 100
distances <- seq(from = 0, to = 25, length.out = precision)

# sigmas
sigmas <- c(0.001, 0.003, 0.01, 0.03)

# angle thresholds
r <- 2.135 / 100
R <- 5.398 / 100
threshold_angle <- atan((R - r) / distances)

# first one is NaN, lets fix it
threshold_angle[1] <- pi

# data frame
df <- data.frame(distance = numeric(), sigma = numeric(), p = numeric())

# probabilities
for (s in sigmas) {
  # calculate probs
  p <- 2 * pnorm(threshold_angle / s, 0, 1) - 1

  # append
  df <- df %>% add_row(distance = distances, sigma = s, p = p)
}

# change sigma to degrees
df$sigma <- round(df$sigma * 180 / pi, 1)

# change sigma to factor
df$sigma <- as.factor(df$sigma)

# plot
ggplot(data = df, aes(x = distance, y = p, group = sigma, color = sigma)) +
  geom_line(linewidth = 1) +
  scale_discrete_manual("color", values = c("grey80", "grey60", "grey40", "grey20")) +
  theme_minimal()
