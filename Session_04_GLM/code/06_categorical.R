# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)

# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/categorical.stan")

# load data
data <- read.csv("../data/shot_types.csv", stringsAsFactors = TRUE)

# contrasts (similar as one hot encoding)
contrasts(data$PlayerType) <- contr.treatment(n_distinct(data$PlayerType))

# display contrasts
contrasts(data$PlayerType)

# the intercept is part of the model matrix (1st column always equals 1)
X <- model.matrix(~ Distance + PlayerType, data)

# show a couple of top rows to check if all is OK
head(X)

# dependent variable
y <- data$ShotType

# show levels of y
levels(y)

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

# summary of betas
fit$summary("beta")

# analysis ---------------------------------------------------------------------
# extract parameters
df_betas <- as_draws_df(fit$draws("beta"))
df_betas <- df_betas %>% select(-.chain, -.iteration, -.draw)

# beta matrix composed of sample means
# working with means from here one for brevity and simplicity purposes
# the true bayesian way would be to work with samples all the way
betas <- matrix(colMeans(df_betas), nrow = 3, ncol = 4)

# helper softmax function ------------------------------------------------------
softmax <- function(x) {
  return(as.vector(exp(x) / sum(exp(x))))
}

# calculate example probabilities ----------------------------------------------
# probabilities for each shot type for a guard shooting from 1.5m
# x <- c(intercept, distance, forward, guard)
x <- c(1, 1.5, 0, 1)

# calculate probabilities
c_probs <- softmax(betas %*% x)
c_probs

# plot probabilities for all player types --------------------------------------
precision <- 100
distance <- seq(0, 10, length.out = precision)

# calculate thetas from distance
df_thetas <- data.frame(
  Distance = numeric(),
  Type = factor(),
  Probability = numeric(),
  Player = factor()
)

# types
types <- levels(data$ShotType)

# iterate over distance and calculate thetas
for (i in 1:precision) {
  # intercept, distance, forward, guard)
  x <- c(1, distance[i], 0, 1)
  thetas <- softmax(betas %*% x)

  df_guard <- data.frame(
    Distance = distance[i],
    Type = types,
    Probability = thetas,
    Player = "Guard"
  )

  # intercept, distance, forward, guard)
  x <- c(1, distance[i], 1, 0)
  thetas <- softmax(betas %*% x)

  df_forward <- data.frame(
    Distance = distance[i],
    Type = types,
    Probability = thetas,
    Player = "Forward"
  )

  # intercept, distance, forward, guard)
  x <- c(1, distance[i], 0, 0)
  thetas <- softmax(betas %*% x)

  df_centre <- data.frame(
    Distance = distance[i],
    Type = types,
    Probability = thetas,
    Player = "Centre"
  )

  df_thetas <- rbind(df_thetas, df_guard, df_forward, df_centre)
}

# plot
ggplot(data = df_thetas, aes(x = Distance, y = Probability, fill = Type)) +
  geom_area(linewidth = 1) +
  ggtitle("Shot selection by player type") +
  scale_fill_brewer(palette = "Blues") +
  facet_grid(Player ~ .) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
