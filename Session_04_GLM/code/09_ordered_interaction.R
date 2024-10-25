# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)

# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/ordered.stan")

# load data and treat strings as factors
data <- read.csv("../data/poverty.csv", stringsAsFactors = TRUE)

# contrasts
contrasts(data$religion) <- contr.treatment(n_distinct(data$religion))
contrasts(data$degree) <- contr.treatment(n_distinct(data$degree))
contrasts(data$country) <- contr.treatment(n_distinct(data$country))
contrasts(data$sex) <- contr.treatment(n_distinct(data$sex))

# note that intercept is part of the model matrix (1st column always equals 1)
X <- model.matrix(~ religion + degree + country + sex, data)
# remove intercept since in ordered regression intercepts are cutpoints
X <- X[, -1]

# add an interaction between the country and religion
X <- data.frame(X)
X$religious_usa <- X$religion2 * X$country2

# show a couple of top rows to check if all is OK
head(X)

# check poverty levels
levels(data$poverty)

# fix ordering
data$poverty <- factor(data$poverty,
  levels = c("Too Little", "About Right", "Too Much")
)

# check poverty levels again
levels(data$poverty)

# dependent variable
y <- data$poverty

# stan_data
stan_data <- list(n = nrow(data), m = ncol(X), k = nlevels(y), x = X, y = y)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot for beta parameters
mcmc_trace(fit$draws("beta"))
mcmc_trace(fit$draws("c"))

# summary of betas
fit$summary("beta")
fit$summary("c")

# analysis ---------------------------------------------------------------------
# extract parameters
df_beta <- as_draws_df(fit$draws("beta"))
df_beta <- df_beta %>% select(-.chain, -.iteration, -.draw)
df_cutpoints <- as_draws_df(fit$draws("c"))
df_cutpoints <- df_cutpoints %>% select(-.chain, -.iteration, -.draw)

# plot betas -------------------------------------------------------------------
# rename for ease of addressing
colnames(df_beta) <- c("Religion", "Degree", "USA", "Male", "Religious_USA")

# to long format
df_beta_long <- df_beta %>% gather(Beta, Value)

# plot
ggplot(data = df_beta_long, aes(x = Value, y = Beta)) +
  stat_eye(fill = "skyblue", alpha = 0.75) +
  xlim(-2.5, 2.5) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1, color = "grey50")
