# libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)


# generate data ----------------------------------------------------------------
# 50 years of monthly data
n <- 50 * 12

# data frame
df <- data.frame(t = numeric(), y = numeric(), type = character())

# timepoints
t <- 1:n

# trend
trend <- t * 0.25
df <- df %>% add_row(data.frame(t = t, y = trend, type = "Trend"))

# seasonality
seasonality <- 50 * sin(t / 12)
df <- df %>% add_row(data.frame(t = t, y = seasonality, type = "Seasonality"))

# noise
noise <- rnorm(n, 0, 50)
df <- df %>% add_row(data.frame(t = t, y = noise, type = "Noise"))

# merge
df <- df %>% add_row(data.frame(
  t = t,
  y = trend + seasonality + noise,
  type = "Combined"
))

# plot
ggplot(df, aes(x = t, y = y)) +
  geom_line(color = "grey40") +
  facet_wrap(. ~ type, ncol = 1)


# acf --------------------------------------------------------------------------
df_acf <- data.frame(Lag = numeric(), ACF = numeric(), type = character())

acf <- acf(trend, lag.max = 300)
df_acf <- df_acf %>% add_row(data.frame(
  Lag = acf$lag,
  ACF = acf$acf,
  type = "Trend"
))

acf <- acf(seasonality, lag.max = 300)
df_acf <- df_acf %>% add_row(data.frame(
  Lag = acf$lag,
  ACF = acf$acf,
  type = "Seasonality"
))

acf <- acf(noise, lag.max = 300)
df_acf <- df_acf %>% add_row(data.frame(
  Lag = acf$lag,
  ACF = acf$acf,
  type = "Noise"
))

acf <- acf(trend + seasonality, lag.max = 300)
df_acf <- df_acf %>% add_row(data.frame(
  Lag = acf$lag,
  ACF = acf$acf,
  type = "Trend + Seasonality"
))

acf <- acf(trend + noise, lag.max = 300)
df_acf <- df_acf %>% add_row(data.frame(
  Lag = acf$lag,
  ACF = acf$acf,
  type = "Trend + Noise"
))

acf <- acf(seasonality + noise, lag.max = 300)
df_acf <- df_acf %>% add_row(data.frame(
  Lag = acf$lag,
  ACF = acf$acf,
  type = "Seasonality + Noise"
))

# plot
ggplot(df_acf, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", width = 0.5) +
  facet_wrap(. ~ type, ncol = 2)
