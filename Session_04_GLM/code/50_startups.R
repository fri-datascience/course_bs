# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(tidyverse)
library(bayesplot)
library(posterior)
library(mcmcse)
library(ggdist)

# modelling and data prep ------------------------------------------------------
# model
model <- cmdstan_model("../models/linear_nointercept.stan")

# data
data <- read.csv("./data/50_startups.csv")

# let us focus only on research, administration, marketing => profit
data <- data %>% select(research, administration, marketing, profit)

# normalized columns
data$r <- (data$research - min(data$research)) / (max(data$research) - min(data$research))
data$a <- (data$administration - min(data$administration)) / (max(data$administration) - min(data$administration))
data$m <- (data$marketing - min(data$marketing)) / (max(data$marketing) - min(data$marketing))
data$p <- (data$profit - min(data$profit)) / (max(data$profit) - min(data$profit))

# fit without normalization
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

# fit with normalization
y <- data$p
X <- data %>% select(r, a, m)
stan_data <- list(n = length(y), k = ncol(X), y = y, X = X)

# fit normalized
fit_normalized <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)


# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())
mcmc_trace(fit_normalized$draws())

# compare summaries
fit$summary()
fit_normalized$summary()


# extract samples --------------------------------------------------------------
# convert draws to df
df <- as_draws_df(fit$draws())
df_normalized <- as_draws_df(fit_normalized$draws())

# mcse
mcse(df$`b[1]`)
mcse(df$`b[2]`)
mcse(df$`b[3]`)
mcse(df_normalized$`b[1]`)
mcse(df_normalized$`b[2]`)
mcse(df_normalized$`b[3]`)


# compare default vs normalized ------------------------------------------------
# calculate ratios
df_ratios_default <- df %>%
  mutate(Research = `b[1]` / (`b[1]` + `b[2]` + `b[3]`),
         Administration = `b[2]` / (`b[1]` + `b[2]` + `b[3]`),
         Marketing = `b[3]` / (`b[1]` + `b[2]` + `b[3]`)) %>%
  select(Research, Administration, Marketing)

df_ratios_normalized <- df_normalized %>%
  mutate(Research = `b[1]` / (`b[1]` + `b[2]` + `b[3]`),
         Administration = `b[2]` / (`b[1]` + `b[2]` + `b[3]`),
         Marketing = `b[3]` / (`b[1]` + `b[2]` + `b[3]`)) %>%
  select(Research, Administration, Marketing)

# column means
mcse(df_ratios_default$Research)
mcse(df_ratios_default$Administration)
mcse(df_ratios_default$Marketing)
mcse(df_ratios_normalized$Research)
mcse(df_ratios_normalized$Administration)
mcse(df_ratios_normalized$Marketing)

# add labels
df_ratios_default$Type <- "Default"
df_ratios_normalized$Type <- "Normalized"

# means
df_ratios <- rbind(df_ratios_default, df_ratios_normalized)

# to long format
df_ratios <- df_ratios %>% gather(Variable,
                                  Value,
                                  c(Research, Administration, Marketing))
# plot
ggplot(data = df_ratios, aes(x = Value, y = Variable)) +
  stat_eye(fill="skyblue", alpha = 0.75) +
  facet_grid(Type ~ .) +
  xlim(0, 1)
