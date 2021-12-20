# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(HDInterval)


# data prep and exploratory analysis -------------------------------------------
df <- read.csv("../data/temperature.csv", sep=";")

# select only newer data so it runs faster
df <- df %>% filter(year > 1980)

# cast to date
df$date <- as.Date(paste0(df$year, "-", df$month, "-01"))

# add month ix
df$ix <- 1:nrow(df)

# plot
ggplot(df, aes(x=date, y=temperature)) +
  geom_line()

# fft can be useful with seasonality to get important frequencies
n <- nrow(df)
I <- abs(fft(df$temperature))^2 / n
P <- (4/n)*I[1:(n/2+1)]
f <- 0:(n/2)/(n)

# merge
df_fft <- data.frame(P=P[-1], f=f[-1])

# power spectrum
ggplot(df_fft, aes(x=f, y=P)) +
  geom_bar(stat="identity") +
  xlim(0, 0.2)


# decomposition with harmonic regression ---------------------------------------
model <- cmdstan_model("../models/harmonic_basic.stan")

# seasonality frequency
omega <- (2 * pi) / 12

stan_data <- list(y = df$temperature, 
                  t = df$ix, 
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
t <- df$ix

# storage data frame
df_decomposed <- data.frame(idx = character(),
                            Type = character(),
                            Month = integer(),
                            Temperature = numeric())

for (i in idx) {
  # original
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Original",
                       Month = t, 
                       Temperature = df$temperature))

  # ssn
  ssn <- df_s$beta_cos[i] * cos(omega * t) + df_s$beta_sin[i] * sin(omega * t)
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Seasonality",
                       Month = t, 
                       Temperature = ssn))

  # trend
  trend <- df_s$beta[i] * t + df_s$alpha[i]
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Trend",
                       Month = t, 
                       Temperature = trend))

  # reminder
  reminder <- df$temperature - ssn - trend
  df_decomposed <- df_decomposed %>%
    add_row(data.frame(idx = as.character(i),
                       Type = "Reminder",
                       Month = t, 
                       Temperature = reminder))
}

# plot
ggplot(df_decomposed, aes(x=Month,
                          y=Temperature,
                          group=idx,
                          colour=idx)) +
  geom_line() +
  facet_wrap( ~ Type, ncol=1, scales="free_y")
