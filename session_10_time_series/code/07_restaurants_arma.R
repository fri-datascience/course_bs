# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(HDInterval)


# data prep and exploratory analysis -------------------------------------------
df <- read.csv("./session_10_time_series/data/restaurants.csv")

# only last 10 years
df <- df %>% filter(month > (nrow(df) - 120))

# reindex months
df$month <- seq_len(nrow(df))


# ar ---------------------------------------------------------------------------
model <- cmdstan_model("./session_10_time_series/models/arma.stan")

# prep data for stan
p <- 13
q <- 12
stan_data <- list(
  y = df$spending,
  n = nrow(df),
  p = p,
  q = q
)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1,
  adapt_delta = 0.95,
  max_treedepth = 15
)

# diagnostics
mcmc_trace(fit$draws(c("mu", "beta", "theta", "sigma")))
fit$summary()

# samples
df_s <- as_draws_df(fit$draws())
df_s <- df_s %>% select(-lp__, -.draw, -.chain, -.iteration)


# plot fit ---------------------------------------------------------------------
# get a subsample of 20 random samples
df_nu <- df_s[(3 + p + q):(3 + p + q + nrow(df) - 1)]

hdi5 <- function(x) {
  return(hdi(x, credMass = 0.90)[1])
}

hdi95 <- function(x) {
  return(hdi(x, credMass = 0.90)[2])
}

df_plot <- data.frame(
  Month = df$month,
  Spending = colMeans(df_nu),
  hdi5 = apply(df_nu, hdi5, MARGIN = 2),
  hdi95 = apply(df_nu, hdi95, MARGIN = 2)
)

# add predictions
df_ss <- df_s[sample(seq_len(nrow(df)), 20, rep = FALSE), ]

# forecast n_f months
n_f <- 12
n_t <- nrow(df)
df_forecast <- data.frame(
  Month = numeric(),
  S = numeric()
)

for (i in seq_len(nrow(df_ss))) {
  # get params
  params <- df_ss[i, 1:(1 + p + q)]
  mu <- params$mu
  betas <- as.numeric(params[2:(2 + p - 1)])
  thetas <- as.numeric(params[(2 + p):(2 + p + q - 1)])

  # spending
  s <- df$spending

  # get epsilons
  epsilons <- as.numeric(df_ss[i, (2 + p + q + n_t + 1):(2 + p + q + 2 * n_t)])

  # forecast next n_f points
  for (j in (n_t + 1):(n_t + n_f)) {
    # ar
    ar <- sum(betas * s[(j - p):(j - 1)])

    # ma
    ma <- sum(thetas * epsilons[(j - q):(j - 1)])

    # store
    s[j] <- mu + ar + ma

    # assume no error (prediction == truth)
    epsilons[j] <- 0
  }

  # store
  df_forecast <- df_forecast %>%
    add_row(
      Month = (n_t + 1):(n_t + n_f),
      S = s[(n_t + 1):(n_t + n_f)]
    )
}

# add forecasts to the plotting data frame
df_plot_forecast <- df_forecast %>%
  group_by(Month) %>%
  summarize(
    Spending = mean(S),
    hdi5 = hdi5(S),
    hdi95 = hdi95(S)
  )

# bind
df_plot <- df_plot %>% add_row(df_plot_forecast)

# plot
ggplot(data = df_plot, aes(x = Month, y = Spending), group = ix) +
  geom_line(data = df, aes(x = month, y = spending), color = "skyblue") +
  geom_line() +
  geom_ribbon(aes(ymin = hdi5, ymax = hdi95), alpha = 0.25) +
  geom_vline(
    xintercept = max(df$month),
    linetype = "dashed",
    linewidth = 1,
    color = "grey75"
  ) +
  theme_minimal()
