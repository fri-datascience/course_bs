# libraries --------------------------------------------------------------------
library(cmdstanr)  # for interfacing Stan
library(mcmcse)
library(posterior)

# modelling and data prep ------------------------------------------------------
# prepare the data
# number of measurements
n <- 20

# data points
y <- rnorm(20, 0, 20)

# compile the model
model <- cmdstan_model("../models/zero_centered.stan")

# prepare input data
stan_data <- list(n = n, y = y)

# fit
fit <- model$sample(
  data = stan_data
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())

# summary
fit$summary()


# analysis ---------------------------------------------------------------------
# convert samples to data frame
df <- as_draws_df(fit$draws())

# sigma
mcse(df$sigma)
