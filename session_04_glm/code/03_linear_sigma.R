# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(posterior)
library(bayesplot)
library(mcmcse)
library(tidyverse)

# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("./session_04_glm/models/simple_linear_sigma.stan")

# generate the data with a variable sigma
set.seed(1)
n <- 100
mu <- 10
b <- 3
x <- seq(1, 10, length.out = n)
y <- mu + rnorm(n, 0, x * b)

# prepare input for Stan
data <- data.frame(x = x, y = y)
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
# params
df <- as_draws_df(fit$draws())
mcse(df$b)

# plot only 100 random regression lines with variable sigma
n_samples <- 100
df_100 <- sample_n(df, n_samples)

# create prediction intervals for each sampled posterior draw
x_seq <- seq(min(data$x), max(data$x), length.out = 100)
pred_intervals <- lapply(1:n_samples, function(i) {
  mu_fixed <- df_100$mu[i]
  sigma <- df_100$b[i] * x_seq
  data.frame(
    x = x_seq,
    mu = mu_fixed,
    lower = mu_fixed - 2 * sigma,
    upper = mu_fixed + 2 * sigma,
    draw = i
  )
})
pred_df <- bind_rows(pred_intervals)

# visualize data points with regression lines and uncertainty bands
ggplot() +
  geom_ribbon(
    data = pred_df,
    aes(x = x, ymin = lower, ymax = upper, group = draw),
    alpha = 0.02,
    fill = "skyblue"
  ) +
  geom_hline(
    data = df_100,
    aes(yintercept = mu),
    alpha = 0.2,
    linewidth = 0.5,
    color = "grey"
  ) +
  geom_point(
    data = data,
    aes(x = x, y = y),
    shape = 16,
    color = "darkblue"
  ) +
  theme_minimal() +
  labs(x = "x", y = "y") +
  xlim(min(data$x), max(data$x))
