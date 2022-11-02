# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(tidyverse)
library(posterior)
library(bayesplot)
library(mcmcse)
library(shinystan)

# load the data and the model --------------------------------------------------
# compile the model
model <- cmdstan_model("../models/bernoulli_beta.stan")

# prepare the data
data <- read.csv("../data/basketball_shots.csv", sep = ";")

# filter for 1st player, default and special rims
player1_default <- data %>% filter(PlayerID == 1 & SpecialRim == 0)
player1_special <- data %>% filter(PlayerID == 1 & SpecialRim == 1)

# prepare input data
stan_data_default <- list(n = nrow(player1_default), y = player1_default$Made)
stan_data_special <- list(n = nrow(player1_special), y = player1_special$Made)

# fitting and diagnostics ------------------------------------------------------
fit_default <- model$sample(
  data = stan_data_default,
  seed = 1
)

fit_special <- model$sample(
  data = stan_data_special,
  seed = 1
)

# traceplots
mcmc_trace(fit_default$draws("theta"))
mcmc_trace(fit_special$draws("theta"))

# summary
fit_default$summary()
fit_special$summary()

# additional automated diagnostics
fit_default$cmdstan_diagnose()
fit_special$cmdstan_diagnose()

# visual checks
launch_shinystan(fit_default)
launch_shinystan(fit_special)

# analysis ---------------------------------------------------------------------
# convert draws to data frame
df_default <- as_draws_df(fit_default$draws("theta"))
df_special <- as_draws_df(fit_special$draws("theta"))

# compare
mcse(df_default$theta - df_special$theta)
mcse(df_default$theta > df_special$theta)

# set rim
df_default$Rim <- "Default"
df_special$Rim <- "Special"
df_player1 <- rbind(df_default, df_special)

# plot
ggplot(data = df_player1, aes(x = theta, group = Rim, fill = Rim)) +
  geom_density(alpha = 0.5, color = NA) +
  scale_fill_brewer(type = "qual", palette = 3) +
  xlim(0, 1) +
  theme_minimal()

# number of draws
n <- 100000

# draws from the distribution for the default rim
draws_default <- rbinom(n, 1, df_default$theta)

# draws from the distribution for the special rim
draws_special <- rbinom(n, 1, df_special$theta)

# compare
mcse(draws_default > draws_special)
