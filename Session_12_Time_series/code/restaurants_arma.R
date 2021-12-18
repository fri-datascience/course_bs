# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(HDInterval)


# data prep and exploratory analysis -------------------------------------------
df <- read.csv("../data/restaurants.csv")

# only last 10 years
df <- df %>% filter(month > (nrow(df) - 120))

# reindex months
df$month <- 1:nrow(df)


# ar ---------------------------------------------------------------------------
model <- cmdstan_model("../models/arma.stan")

model <- cmdstan_model("../models/arma_1_1.stan")
stan_data <- list(y = df$spending, 
                  n = nrow(df))

# prep data for stan
K <- 6
Q <- 6
stan_data <- list(y = df$spending, 
                  n = nrow(df),
                  K = K,
                  Q = Q)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# diagnostics
mcmc_trace(fit$draws(c("mu", "beta", "theta", "sigma")))
fit$summary()

# samples
df_s <- as_draws_df(fit$draws())
df_s <- df_s %>% select(-lp__, -.draw, -.chain, -.iteration)


# plot fit ---------------------------------------------------------------------
# get a subsample of 20 random samples
df_nu <- df_s[(1+Q+K):(1+Q+K+nrow(df)-1)]

hdi5 <- function(x) {
  return(hdi(x, credMass=0.90)[1])
}

hdi95 <- function(x) {
  return(hdi(x, credMass=0.90)[2])
}

df_plot <- data.frame(Month = df$month,
                      Spending = colMeans(df_nu),
                      hdi5 = apply(df_nu, hdi5, MARGIN = 2),
                      hdi95 = apply(df_nu, hdi5, MARGIN = 2))

# plot
ggplot(data=df_plot, aes(x=Month, y=Spending)) +
  geom_line(data=df, aes(x=month, y=spending), color="skyblue") +
  geom_line() +
  theme_minimal() +
  ylim(2, 4.1)
