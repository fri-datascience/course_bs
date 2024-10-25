# libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)

# load the data ----------------------------------------------------------------
df <- read.csv("../data/grades.csv")

# median and mean
median(df$grade)
mean(df$grade)

# betas ------------------------------------------------------------------------
betas <- data.frame(
  alpha = c(2.41, 1.98),
  beta = c(1.54, 1.04)
)

# calculate density
x_max <- 5
x <- seq(0, x_max, length.out = 1000)

df_beta <- data.frame(x = numeric(), y = numeric(), group = factor())

for (i in seq_len(nrow(betas))) {
  beta <- betas[i, ]
  y <- dbeta(x, beta$alpha, beta$beta)
  df_beta <- df_beta %>%
    add_row(x = (x * x_max) + x_max, y = y / x_max, group = as.factor(i))
}

# plot -------------------------------------------------------------------------
ggplot(data = df, aes(x = grade)) +
  geom_density(color = NA, fill = "skyblue") +
  geom_line(data = df_beta, aes(x = x, y = y, color = group), linewidth = 2) +
  xlim(x_max, 10) +
  scale_color_brewer(type = "qual", palette = 2)
