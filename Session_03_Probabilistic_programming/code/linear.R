# libraries --------------------------------------------------------------------
library(cmdstanr)  # for interfacing Stan
library(ggplot2)   # for visualizations
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse)    # for comparing samples and calculating MCSE
library(tidyverse) # for data manipulations

# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/linear.stan")

# prepare the data
data <- read.csv("../data/toy.csv")

# prepare input for Stan
stan_data <- list(n = nrow(data), x = data$x, y = data$y)

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
# lines and confidence in lines
df <- as_draws_df(fit$draws())

# params
mcse(df$a)
mcse(df$b)

# plot only 100 random regression lines
df_100 <- sample_n(df, 100)

# visualize data points with regression lines in the background
ggplot() + 
  geom_point(data = data,
             aes(x = x, y = y),
             shape = 16,
             color = "skyblue") +
  geom_abline(data = df_100,
              aes(slope = b, intercept = a),
              alpha=0.05,
              size=1,
              color="skyblue") +
  theme_minimal()
