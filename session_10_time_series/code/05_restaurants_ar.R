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
model <- cmdstan_model("./session_10_time_series/models/ar.stan")

# use pacf to get the p parameter
pacf(df$spending)

# set p
p <- 13

# prep data for stan
stan_data <- list(
  y = df$spending,
  n = nrow(df),
  p = p
)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# diagnostics
mcmc_trace(fit$draws())
fit$summary()

# samples
df_s <- as_draws_df(fit$draws())
df_s <- df_s %>% select(-lp__, -.draw, -.chain, -.iteration)

# plot fit ---------------------------------------------------------------------
# get a subsample of 20 random samples
df_ss <- df_s[sample(seq_len(nrow(df_s)), 20, rep = FALSE), ]

df_plot <- data.frame(
  idx = character(),
  Month = integer(),
  S = numeric()
)

# forecast n_f months
n_f <- 12
n_t <- nrow(df)
t <- 1:(n_t + n_f)

for (i in seq_len(nrow(df_ss))) {
  # alpha and betas
  alpha <- as.numeric(df_ss[i, ]$alpha)
  betas <- as.numeric(df_ss[i, 2:(2 + p - 1)])

  # init spending
  s <- df$spending

  # model
  for (j in (p + 1):n_t) {
    s[j] <- alpha + sum(betas * df$spending[(j - p):(j - 1)])
  }

  # forecast
  for (j in (n_t + 1):(n_t + n_f)) {
    s[j] <- alpha + sum(betas * s[(j - p):(j - 1)])
  }

  df_plot <- df_plot %>%
    add_row(data.frame(
      idx = as.character(i),
      Month = t,
      S = s
    ))
}

# get mean and HDI
df_plot <- df_plot %>%
  group_by(Month) %>%
  summarize(
    Spending = mean(S),
    hdi5 = hdi(S, credMass = 0.90)[1],
    hdi95 = hdi(S, credMass = 0.90)[2]
  )

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

ggsave(
  "./session_10_time_series/figs/restaurants_ar.png",
  width = 1920,
  height = 1080,
  units = "px",
  dpi = 300
)
