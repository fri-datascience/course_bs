# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(posterior)
library(bayesplot)
library(ggplot2)
library(tidyverse)

# load the data ----------------------------------------------------------------
df <- read.csv("./session_06_priors/data/salaries_2025.csv")

# to thousands
df$salary <- df$salary / 1000

# median
median(df$salary)

# gammas -----------------------------------------------------------------------
gammas <- data.frame(
  k = c(4, 3.2, 2.9),
  t = c(0.8, 0.6, 0.2)
)

# calculate density
x_max <- 20
precision <- 1000
x <- seq(0, x_max, length.out = precision)

df_gamma <- data.frame(x = numeric(), y = numeric(), group = factor())

for (i in seq_len(nrow(gammas))) {
  gamma <- gammas[i, ]
  y <- dgamma(x, gamma$k, rate = gamma$t)
  df_gamma <- df_gamma %>% add_row(x = x, y = y, group = as.factor(i))
}

# plot -------------------------------------------------------------------------
ggplot(data = df, aes(x = salary)) +
  geom_line(data = df_gamma, aes(x = x, y = y, color = group), linewidth = 1) +
  xlim(0, x_max) +
  scale_color_brewer(type = "qual", palette = 2)

ggplot(data = df, aes(x = salary)) +
  geom_density(color = NA, fill = "skyblue") +
  geom_line(data = df_gamma, aes(x = x, y = y, color = group), linewidth = 1) +
  xlim(0, x_max) +
  scale_color_brewer(type = "qual", palette = 2)

# bayesian fit -----------------------------------------------------------------
# model
model <- cmdstan_model("./session_06_priors/models/gamma.stan")

# prepare input data
stan_data <- list(n = nrow(df), y = df$salary)

# fit
fit <- model$sample(
  data = stan_data,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())

# summary
fit$summary()

# analysis ---------------------------------------------------------------------
# convert samples to data frame
df_fit <- as_draws_df(fit$draws())

# mean sample
mean_k <- mean(df_fit$k)
mean_t <- mean(df_fit$t)

# kullback-leibler divergence --------------------------------------------------
kl_divergence_gamma <- function(k_p, t_p, k_q, t_q) {
  # dx
  dx <- x[2] - x[1]

  # densities
  p <- dgamma(x, shape = k_p, rate = t_p)
  q <- dgamma(x, shape = k_q, rate = t_q)

  # kl divergence
  kl <- sum(p * log(p / q) * dx, na.rm = TRUE)

  return(kl)
}

# calculate kl divergence for each gamma model vs the truth
for (i in seq_len(nrow(gammas))) {
  gamma <- gammas[i, ]
  kl <- kl_divergence_gamma(mean_k, mean_t, gamma$k, gamma$t)
  cat(paste0(
    "Group ", i, ": k = ", gamma$k, ", t = ", gamma$t,
    " | KL divergence = ", round(kl, 4), "\n"
  ))
}
