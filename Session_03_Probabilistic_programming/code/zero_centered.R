library(cmdstanr)  # for interfacing Stan
library(mcmcse)
library(posterior)

# prepare the data
# number of measurements
n <- 20

# tall
y <- rnorm(20, 0, 20)

# compile the model
model <- cmdstan_model("../models/zero_centered.stan")

# prepare input data
stan_data <- list(n = n, y = y)

# fit
fit <- model$sample(
  data = stan_data
)

# convert samples to data frame
df <- as_draws_df(fit$draws())

# recover mu
mcse(df$sigma)
