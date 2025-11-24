# libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)

# load the data ----------------------------------------------------------------
df <- read.csv("./session_06_priors/data/grades.csv")

# betas ------------------------------------------------------------------------
betas <- data.frame(
  alpha = c(2.43, 0.71, 3.81),
  beta = c(2.14, 0.64, 3.33)
)

# calculate density
x_max <- 6
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
  geom_line(data = df_beta, aes(x = x, y = y, color = group), linewidth = 2) +
  xlim(x_max, 10) +
  scale_color_brewer(type = "qual", palette = 2)

ggplot(data = df, aes(x = grade)) +
  geom_density(color = NA, fill = "skyblue") +
  geom_line(data = df_beta, aes(x = x, y = y, color = group), linewidth = 2) +
  xlim(x_max, 10) +
  scale_color_brewer(type = "qual", palette = 2)
