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

# cast to date
df$date <- as.Date(df$date)

# plot
ggplot(df, aes(x=date, y=spending)) +
  geom_line() +
  theme_minimal()


# decomposition with harmonic regression ---------------------------------------
model <- cmdstan_model("../models/harmonic_basic.stan")

# seasonality frequency
omega <- (2 * pi) / 12

# prep data for stan
stan_data <- list(y = df$spending, 
                  t = df$month, 
                  n = nrow(df), 
                  omega = omega)

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

# plot 5 random samples
idx <- sample(1:nrow(df_s), 5, rep = F)

# our time stamps
t <- df$month

# storage data frame
df_decomposed <- data.frame(idx = character(),
                            Type = character(),
                            Month = integer(),
                            Spending = numeric())

for (i in idx) {
  # original
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Original",
                       Month = t, 
                       Spending = df$spending))

  # ssn
  ssn <- df_s$beta_cos[i] * cos(omega * t) + df_s$beta_sin[i] * sin(omega * t)
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Seasonality",
                       Month = t, 
                       Spending = ssn))

  # trend
  trend <- df_s$beta_1[i] * t + df_s$beta_0[i]
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Trend",
                       Month = t, 
                       Spending = trend))

  # reminder
  reminder <- df$spending - ssn - trend
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Reminder",
                       Month = t, 
                       Spending = reminder))
}

# plot
ggplot(df_decomposed, aes(x=Month,
                          y=Spending,
                          group=idx,
                          colour=idx)) +
  geom_path() +
  facet_wrap( ~ Type, ncol=1, scales="free_y")


# plot fit ---------------------------------------------------------------------
df_plot <- data.frame(idx = character(),
                      Month = integer(),
                      S = numeric())

for (i in 1:nrow(df_s)) {
  # seasonality and trend
  ssn <- df_s$beta_cos[i] * cos(omega * t) + df_s$beta_sin[i] * sin(omega * t)
  trend <- df_s$beta_1[i] * t + df_s$beta_0[i]
  
  df_plot <- df_plot %>%
    add_row(data.frame(idx = as.character(i),
                       Month = t, 
                       S = ssn + trend))
}

# get mean and HDI
df_plot <- df_plot %>%
  group_by(Month) %>%
  summarize(Spending=mean(S),
            hdi5=hdi(S, credMass=0.90)[1],
            hdi95=hdi(S, credMass=0.90)[2])

# plot
ggplot(data=df_plot, aes(x=Month, y=Spending), group=ix) +
  geom_line(data=df, aes(x=month, y=spending), color="skyblue") +
  geom_line() +
  geom_ribbon(aes(ymin=hdi5, ymax=hdi95), alpha=0.25) +
  theme_minimal()
