# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(posterior)
library(bayesplot)
library(mcmcse)
library(tidyverse)

# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/linear.stan")

# prepare the data
data <- read.csv("../data/toy.csv")

# prepare input for Stan
stan_data <- list(n = nrow(data), x = data$x, y = data$y)

# fit
fit <- model$sample(
  data = stan_data,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())

# summary
fit$summary()

# analysis ---------------------------------------------------------------------
# lines and uncertainty
df <- as_draws_df(fit$draws())

# params
mcse(df$a)
mcse(df$b)

# plot only 100 random regression lines
df_100 <- sample_n(df, 100)

# visualize data points with regression lines in the background
ggplot() +
  geom_point(
    data = data,
    aes(x = x, y = y),
    shape = 16,
    color = "skyblue"
  ) +
  geom_abline(
    data = df_100,
    aes(slope = b, intercept = a),
    alpha = 0.05,
    linewidth = 1,
    color = "skyblue"
  ) +
  theme_minimal()
