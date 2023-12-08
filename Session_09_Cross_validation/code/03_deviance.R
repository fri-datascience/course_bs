# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(HDInterval)
library(loo) # for WAIC and LOOIC calculations

# model ------------------------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/linear_deviance.stan")

# modeling ---------------------------------------------------------------------
# number of observations
n <- 100

# max number of independent variables
m_max <- 6

# generate the data
# predictors
X <- matrix(nrow = n, ncol = m_max + 1)
X[, 1] <- 1

# other terms
for (i in 2:(m_max + 1)) {
  X[, i] <- runif(n, -1, 1)
}

# set seed
set.seed(1)

# generate data
y <- rnorm(n, 1 + 1.5 * X[, 2] - 2.5 * X[, 3], 1)

# stan_data
stan_data <- list(
  n = n,
  m_max = m_max,
  X = X,
  y = y
)

# storages
log_lik <- list()
df_aic <- data.frame(AIC = numeric(), order = factor())

for (m in 0:m_max) {
  # set order
  stan_data$m <- m

  # fit
  fit <- model$sample(
    data = stan_data,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 1
  )

  # uncomment lines below for diagnostic purposes
  # traceplot
  # mcmc_trace(fit$draws(c("b", "sigma")))
  # summary
  # fit$summary(c("b", "sigma"))

  # extract
  log_lik[[m + 1]] <- fit$draws(c("log_lik"))
  df_ll <- as_draws_df(fit$draws(c("log_lik")))

  # remove unwanted columns
  # also cast to regular data frame to avoid some warnings later on
  df_ll <- data.frame(df_ll %>% select(-.chain, -.iteration, -.draw))

  # average per row and store
  df_aic <- rbind(
    df_aic,
    data.frame(AIC = -2 * rowSums(df_ll) + 2 * (m + 1), Order = as.factor(m))
  )
}

# AIC --------------------------------------------------------------------------
df_aic_summary <- df_aic %>%
  group_by(Order) %>%
  summarize(
    mean_AIC = mean(AIC),
    hdi5 = hdi(AIC, credMass = 0.9)[1],
    hdi95 = hdi(AIC, credMass = 0.9)[2]
  )

# plot
ggplot(data = df_aic_summary, aes(x = Order, y = mean_AIC)) +
  geom_point(shape = 16, size = 2) +
  geom_linerange(aes(ymin = hdi5, ymax = hdi95), alpha = 0.3) +
  xlab("Number of predictors") +
  ylab("AIC")

# WAIC -------------------------------------------------------------------------
df_waic <- data.frame(WAIC = numeric(), SE = numeric(), Order = factor())

for (i in 0:m_max) {
  waic <- waic(log_lik[[i + 1]])
  df_waic <- rbind(df_waic, data.frame(
    WAIC = waic$estimates[3, 1],
    SE = waic$estimates[3, 2],
    Order = as.factor(i)
  ))
}

# plot
ggplot(data = df_waic, aes(x = Order, y = WAIC)) +
  geom_point(shape = 16, size = 2) +
  geom_linerange(aes(ymin = (WAIC - SE), ymax = (WAIC + SE)), alpha = 0.3) +
  xlab("Number of predictors") +
  ylab("WAIC")

# LOOIC ------------------------------------------------------------------------
df_looic <- data.frame(looic = numeric(), SE = numeric(), Order = factor())

for (i in 0:m_max) {
  r_eff <- relative_eff(log_lik[[i + 1]])
  loo <- loo(log_lik[[i + 1]], r_eff = r_eff)
  df_looic <- rbind(df_looic, data.frame(
    looic = loo$estimates[3, 1],
    SE = loo$estimates[3, 2],
    Order = as.factor(i)
  ))
}

# plot
ggplot(data = df_looic, aes(x = Order, y = looic)) +
  geom_point(shape = 16, size = 2) +
  geom_linerange(aes(ymin = (looic - SE), ymax = (looic + SE)), alpha = 0.3) +
  xlab("Number of predictors") +
  ylab("LOOIC")

# Akaike weights for model combination -----------------------------------------
# calculate delta_looic
df_looic$delta_looic <- abs(df_looic$looic - min(df_looic$looic))

# calculate weights
df_looic$weight <-
  exp(-0.5 * df_looic$delta_looic) / sum(exp(-0.5 * df_looic$delta_looic))
df_looic$weight <- round(df_looic$weight, 2)

# plot
ggplot(data = df_looic, aes(x = Order, y = weight)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("Number of predictors") +
  ylab("Akaike weight") +
  theme_minimal() +
  ylim(0, 1)

# print
df_looic
