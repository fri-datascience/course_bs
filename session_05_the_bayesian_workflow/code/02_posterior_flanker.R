# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)
library(emg) # for exponentially modified Gaussian (normal)
library(HDInterval)

# data prep --------------------------------------------------------------------
# load the data
data <- read.csv("./session_05_the_bayesian_workflow/data/flanker.csv")

# split
df_incongruent <- data %>% filter(congruency == "incongruent")
df_congruent <- data %>% filter(congruency == "congruent")

# the normal model -------------------------------------------------------------
# compile the model
model_normal <- cmdstan_model("./session_05_the_bayesian_workflow/models/normal.stan")

# fit incongruent --------------------------------------------------------------
# prep the data for Stan
n <- nrow(df_incongruent)
y <- df_incongruent$rt
stan_data <- list(n = n, y = y)

# fit
fit_normal <- model_normal$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit_normal$draws())

# summary
fit_normal$summary()

# extract draws
df_normal <- as_draws_df(fit_normal$draws())

# remove unused columns
df_normal <- df_normal %>% select(-lp__, -.chain, -.iteration, -.draw)

# generate 100 distributions
n <- 100
df_normal_100 <- sample_n(df_normal, n)
x <- seq(0, 2, length.out = 1000)

# data frame for storing generated data
df_generated_normal <- data.frame(x = numeric(), y = numeric(), id = numeric())
for (i in 1:100) {
  y <- dnorm(x, mean = df_normal_100$mu[i], sd = df_normal_100$sigma[i])

  # bind
  df_generated_normal <- rbind(
    df_generated_normal,
    data.frame(x = x, y = y, id = i)
  )
}

# posterior check
ggplot() +
  geom_density(
    data = df_incongruent, aes(x = rt),
    fill = "skyblue", alpha = 0.75, color = NA
  ) +
  geom_line(
    data = df_generated_normal,
    aes(x = x, y = y, group = id), alpha = 0.05, linewidth = 1
  ) +
  geom_vline(
    xintercept = 1,
    linewidth = 2,
    color = "grey75",
    linetype = "dashed"
  ) +
  theme_minimal() +
  xlab("Reaction time [s]") +
  ylab("Density")

# the exponentially modified normal model --------------------------------------
# mean <- mu + 1/lambda
# var <- sigma^2 + 1\lambda^2
# compile the model
model_exp <- cmdstan_model("./session_05_the_bayesian_workflow/models/exp_normal.stan")

# fit exponentially modified normal model with incongruent dataset
fit_exp_i <- model_exp$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit_exp_i$draws())

# summary
fit_exp_i$summary()

# analysis ---------------------------------------------------------------------
# extract draws
df_exp_i <- as_draws_df(fit_exp_i$draws())

# remove unused columns
df_exp_i <- df_exp_i %>% select(-lp__, -.chain, -.iteration, -.draw)

# generate 100 distributions
n <- 100
df_exp_i_100 <- sample_n(df_exp_i, n)
x <- seq(0, 2, length.out = 1000)

# data frame for storing generated data
df_generated_exp_i <- data.frame(x = numeric(), y = numeric(), id = numeric())
for (i in 1:100) {
  y <- demg(x,
    mu = df_exp_i_100$mu[i],
    sigma = df_exp_i_100$sigma[i],
    lambda = df_exp_i_100$lambda[i]
  )

  # bind
  df_generated_exp_i <- rbind(
    df_generated_exp_i,
    data.frame(x = x, y = y, id = i)
  )
}

# posterior check
ggplot() +
  geom_density(
    data = df_incongruent,
    aes(x = rt),
    fill = "skyblue",
    alpha = 0.75,
    color = NA
  ) +
  geom_line(
    data = df_generated_exp_i,
    aes(x = x, y = y, group = id),
    alpha = 0.05,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = 1,
    linewidth = 2,
    color = "grey75",
    linetype = "dashed"
  ) +
  theme_minimal() +
  xlab("Reaction time [s]") +
  ylab("Density")

# compare summary statistics ---------------------------------------------------
# means
mean(df_incongruent$rt)
mean(df_normal$mu)
mean(df_exp_i$mu + 1 / df_exp_i$lambda)

# variance
sd(df_incongruent$rt)^2
mean(df_normal$sigma^2)
mean(df_exp_i$sigma^2 + (1 / df_exp_i$lambda^2))

# HDI (high density interval)
hdi(df_incongruent$rt, credMass = 0.90)
hdi(
  rnorm(
    n = 1000,
    mean = mean(df_normal$mu),
    sd = mean(df_normal$sigma)
  ),
  credMass = 0.90
)
hdi(
  remg(
    n = 1000,
    mu = mean(df_exp_i$mu),
    sigma = mean(df_exp_i$sigma),
    lambda = mean(df_exp_i$lambda)
  ),
  credMass = 0.90
)

# what percentage of participants is slower than 1 second
# normal
p_slower_normal <- vector()
for (i in seq_len(nrow(df_normal))) {
  mu <- df_normal[i, ]$mu
  sigma <- df_normal[i, ]$sigma
  p_slower_normal <- c(p_slower_normal, 1 - pnorm(1, mean = mu, sd = sigma))
}

# exp for incongruent
p_slower_exp_i <- vector()
for (i in seq_len(nrow(df_exp_i))) {
  mu <- df_exp_i[i, ]$mu
  sigma <- df_exp_i[i, ]$sigma
  lambda <- df_exp_i[i, ]$lambda
  p_slower_exp_i <- c(
    p_slower_exp_i,
    1 - pemg(1, mu = mu, sigma = sigma, lambda = lambda)
  )
}

# compare
mean_normal <- round(mean(p_slower_normal), 4) * 100
hdi_normal <- round(hdi(p_slower_normal, credmass = 0.90), 4) * 100
cat(paste0(
  "Normal model: ", mean_normal, "% (90% HDI ",
  hdi_normal[1], "-", hdi_normal[2], "%) slower than 1 s"
))

mean_emg <- round(mean(p_slower_exp_i), 4) * 100
hdi_emg <- round(hdi(p_slower_exp_i, credmass = 0.90), 4) * 100
cat(paste0(
  "EMG model: ", mean_emg, "% (90% HDI ",
  hdi_emg[1], "-", hdi_emg[2], "%) slower than 1 s"
))

# sample
cat(paste0(
  "Sample: ",
  round(sum(df_incongruent$rt > 1) / nrow(df_incongruent), 4) * 100,
  "% slower than 1 s"
))

# fit congruent dataset --------------------------------------------------------
# fit exponentially modified normal model with congruent dataset
# prep the data for Stan
n <- nrow(df_congruent)
y <- df_congruent$rt
stan_data <- list(n = n, y = y)

fit_exp_c <- model_exp$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# traceplot
mcmc_trace(fit_exp_c$draws())

# summary
fit_exp_c$summary()

# extract draws
df_exp_c <- as_draws_df(fit_exp_c$draws())

# remove unused column
df_exp_c <- df_exp_c %>% select(-lp__, -.chain, -.iteration, -.draw)

# compare congruent vs incongruent ---------------------------------------------
# incongruent
mean(df_exp_i$mu + 1 / df_exp_i$lambda)
hdi(df_exp_i$mu + 1 / df_exp_i$lambda, credMass = 0.9)

# congruent
mean(df_exp_c$mu + 1 / df_exp_c$lambda)
hdi(df_exp_c$mu + 1 / df_exp_c$lambda, credMass = 0.9)

# compare
mcse(df_exp_c$mu + 1 / df_exp_c$lambda < df_exp_i$mu + 1 / df_exp_i$lambda)

# compare with ROPE
congruent <- NULL
incongruent <- NULL
equal <- NULL

# measurement error 0.15 s
ROPE <- 0.15

for (i in seq_len(nrow(df_exp_i))) {
  mean_i <- df_exp_i[i, ]$mu + 1 / df_exp_i[i, ]$lambda
  mean_c <- df_exp_c[i, ]$mu + 1 / df_exp_c[i, ]$lambda

  diff <- abs(mean_i - mean_c)

  if (diff > ROPE) {
    if (mean_c < mean_i) {
      congruent <- c(congruent, 1)
      incongruent <- c(incongruent, 0)
      equal <- c(equal, 0)
    } else {
      congruent <- c(congruent, 0)
      incongruent <- c(incongruent, 1)
      equal <- c(equal, 0)
    }
  } else {
    congruent <- c(congruent, 0)
    incongruent <- c(incongruent, 0)
    equal <- c(equal, 1)
  }
}

# normalize
mcse(congruent)
mcse(incongruent)
mcse(equal)
