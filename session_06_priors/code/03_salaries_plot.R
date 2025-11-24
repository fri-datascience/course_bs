# libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)

# load the data ----------------------------------------------------------------
df <- read.csv("./session_06_priors/data/salaries_2025.csv")

# to thousands
df$salary <- df$salary / 1000

# median
median(df$salary)

# gammas -----------------------------------------------------------------------
gammas <- data.frame(
  k = c(6.166, 1.95, 3.86),
  t = c(1.47, 0.5, 1.06)
)

# calculate density
x_max <- 15
x <- seq(0, x_max, length.out = 1000)

df_gamma <- data.frame(x = numeric(), y = numeric(), group = factor())

for (i in seq_len(nrow(gammas))) {
  gamma <- gammas[i, ]
  y <- dgamma(x, gamma$k, rate = gamma$t)
  df_gamma <- df_gamma %>% add_row(x = x, y = y, group = as.factor(i))
}

# plot -------------------------------------------------------------------------
ggplot(data = df, aes(x = salary)) +
  geom_density(color = NA, fill = "skyblue") +
  geom_line(data = df_gamma, aes(x = x, y = y, color = group), linewidth = 2) +
  xlim(0, 10) +
  scale_color_brewer(type = "qual", palette = 2)
