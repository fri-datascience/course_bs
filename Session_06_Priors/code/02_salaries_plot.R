# libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)

# load the data ----------------------------------------------------------------
df <- read.csv("../data/salaries.csv")

# to thousands
df$salary <- df$salary / 1000

# gammas -----------------------------------------------------------------------
gammas <- data.frame(
  k = c(1.322, 1.8, 4.643),
  t = c(0.197, 0.156, 0.575)
)

# calculate density
x_max <- 15
x <- seq(0, x_max, length.out = 1000)

df_gamma <- data.frame(x = numeric(), y = numeric(), group = factor())

for (i in seq_len(nrow(gammas))) {
  gamma <- gammas[i, ]
  y <- dgamma(x, gamma$k, scale = gamma$t)
  df_gamma <- df_gamma %>% add_row(x = x, y = y, group = as.factor(i))
}

# plot -------------------------------------------------------------------------
ggplot(data = df, aes(x = salary)) +
  geom_density(color = NA, fill = "skyblue", alpha = 0.5) +
  geom_line(data = df_gamma, aes(x = x, y = y, color = group), linewidth = 2) +
  xlim(0, x_max) +
  scale_color_brewer(type = "qual", palette = 2)
