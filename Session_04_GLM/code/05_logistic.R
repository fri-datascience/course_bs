# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(arm) # for logit and inverse logit functions
library(tidyverse)
library(mcmcse)

# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/logistic.stan")

# load data
data <- read.csv("../data/weight_height_sex.csv", stringsAsFactors = TRUE)

# cast sex to 0..1
data$SexNumeric <- as.numeric(data$Sex) - 1

# prep data
n <- nrow(data)
x <- data$Height
y <- data$SexNumeric
stan_data <- list(n = n, x = x, y = y)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  iter_sampling = 2000,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())

# summary
fit$summary()

# analysis ---------------------------------------------------------------------
# extract draws
df <- as_draws_df(fit$draws())

# mcse
mcse(df$alpha)
mcse(df$beta)

# probability that P(male) increases with height
mcse(df$beta > 0)

# visualize posterior and data -------------------------------------------------
# number of lines
n_lines <- 100
# precision
precision <- 100
# heights sequence
height <- seq(
  from = min(data$Height), to = max(data$Height),
  length.out = precision
)
# draw only n_lines
df_subsample <- sample_n(df, n_lines)

# create these lines
lines <- data.frame(x = numeric(), y = numeric(), line = numeric())
for (i in 1:n_lines) {
  # extract values
  beta <- df_subsample$beta[i]
  alpha <- df_subsample$alpha[i]

  # calculate probability
  y <- invlogit(alpha + beta * height)

  # add to lines
  temp <- data.frame(x = height, y = y, line = i)
  lines <- rbind(lines, temp)
}

# plot weight/sex
ggplot() +
  geom_point(
    data = data,
    aes(x = Height, y = SexNumeric),
    alpha = 0.2, size = 3, shape = 16
  ) +
  geom_line(
    data = lines,
    aes(x = x, y = y, group = line),
    color = "skyblue", alpha = 0.2, linewidth = 1
  ) +
  ylab("p(male)")

# beta interpretations ---------------------------------------------------------
# marginal posterior of beta
ggplot(data = df, aes(x = beta)) +
  geom_density(color = NA, fill = "skyblue", alpha = 0.5)

# mean beta
mean_alpha <- mean(df$alpha)
mean_beta <- mean(df$beta)

# logit(theta) = mean_alpha + mean_beta * 183
theta183 <- invlogit(mean_alpha + mean_beta * 183)
theta184 <- invlogit(mean_alpha + mean_beta * 184)

# log odds
log_odds183 <- log(theta183 / (1 - theta183))
log_odds184 <- log(theta184 / (1 - theta184))

# diff == mean_beta! so beta tells us how log_odds will change with predictor
diff <- log_odds184 - log_odds183

# the same
diff
mcse(df$beta)
