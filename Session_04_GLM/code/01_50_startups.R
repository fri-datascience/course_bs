# libraries --------------------------------------------------------------------
library(cmdstanr) # for interfacing Stan
library(ggplot2) # for visualizations
library(tidyverse) # for data manipulations
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse) # for comparing samples and calculating MCSE
library(ggdist) # for visualizing distributions

# modelling and data prep ------------------------------------------------------
# model
model <- cmdstan_model("../models/linear_nointercept.stan")

# data
data <- read.csv("../data/50_startups.csv")

# let us focus only on research, administration, marketing => profit
data <- data %>% select(research, administration, marketing, profit)

# prep for Stan
y <- data$profit
X <- data %>% select(research, administration, marketing)
stan_data <- list(n = length(y), k = ncol(X), y = y, X = X)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())

# compare summaries
fit$summary()

# extract samples --------------------------------------------------------------
# convert draws to df
df <- as_draws_df(fit$draws())

# mcse
mcse(df$`b[1]`)
mcse(df$`b[2]`)
mcse(df$`b[3]`)

# analysis ---------------------------------------------------------------------
# calculate ratios
df_ratios <- df %>%
  mutate(
    Research = `b[1]` / (`b[1]` + `b[2]` + `b[3]`),
    Administration = `b[2]` / (`b[1]` + `b[2]` + `b[3]`),
    Marketing = `b[3]` / (`b[1]` + `b[2]` + `b[3]`)
  ) %>%
  select(Research, Administration, Marketing)

# column means
mcse(df_ratios$Research)
mcse(df_ratios$Administration)
mcse(df_ratios$Marketing)

# to long format
df_ratios <- df_ratios %>% gather(
  Variable,
  Value,
  c(Research, Administration, Marketing)
)
# plot
ggplot(data = df_ratios, aes(x = Value, y = Variable)) +
  stat_eye(fill = "skyblue", alpha = 0.75) +
  xlim(0, 1)
