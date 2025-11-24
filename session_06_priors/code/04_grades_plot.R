# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(posterior)
library(bayesplot)
library(tidyverse)

# load the data ----------------------------------------------------------------
df <- read.csv("./session_06_priors/data/grades.csv")

# betas ------------------------------------------------------------------------
betas <- data.frame(
  alpha = c(4, 1, 1.1),
  beta = c(3, 1, 0.5)
)

# calculate density scaled to [x_min, x_max]) ----------------------------------
# calculate density
x_min <- 6
x_max <- 10
precision <- 1000
eps <- 0.01
x <- seq(eps, 1 - eps, length.out = precision)

df_beta <- data.frame(x = numeric(), y = numeric(), group = factor())

for (i in seq_len(nrow(betas))) {
  beta <- betas[i, ]
  y <- dbeta(x, beta$alpha, beta$beta)
  df_beta <- df_beta %>%
    add_row(x = x_min + (x * (x_max - x_min)), y = y / (x_max - x_min), group = as.factor(i))
}

# plot -------------------------------------------------------------------------
ggplot(data = df, aes(x = grade)) +
  geom_line(data = df_beta, aes(x = x, y = y, color = group), linewidth = 1) +
  xlim(x_min, x_max) +
  ylim(0, 0.5) +
  scale_color_brewer(type = "qual", palette = 2)

ggplot(data = df, aes(x = grade)) +
  geom_density(color = NA, fill = "skyblue") +
  geom_line(data = df_beta, aes(x = x, y = y, color = group), linewidth = 1) +
  xlim(x_min, x_max) +
  ylim(0, 0.5) +
  scale_color_brewer(type = "qual", palette = 2)

# bayesian fit -----------------------------------------------------------------
# model
model <- cmdstan_model("./session_06_priors/models/beta.stan")

# prepare input data
n <- nrow(df)
y <- (df$grade - x_min) / (x_max - x_min)
# avoid 0 and 1 values (for Beta likelihood)
y[y <= 0] <- eps
y[y >= 1] <- 1 - eps
stan_data <- list(n = n, y = y)

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
mean_alpha <- mean(df_fit$a)
mean_beta <- mean(df_fit$b)

# kullback-leibler divergence --------------------------------------------------
kl_divergence_beta <- function(a_p, b_p, a_q, b_q) {
  # integrate on (0,1) with endpoints excluded to avoid infinities
  dx <- x[2] - x[1]

  p <- dbeta(x, shape1 = a_p, shape2 = b_p)
  q <- dbeta(x, shape1 = a_q, shape2 = b_q)

  # kl divergence
  kl <- sum(p * log(p / q) * dx, na.rm = TRUE)

  return(kl)
}

# calculate kl divergence for each beta model vs the truth
for (i in seq_len(nrow(betas))) {
  beta <- betas[i, ]
  kl <- kl_divergence_beta(mean_alpha, mean_beta, beta$alpha, beta$beta)
  cat(
    "Model", i, ": alpha =", beta$alpha, ", beta =", beta$beta,
    "| KL divergence =", round(kl, 4), "\n"
  )
}
