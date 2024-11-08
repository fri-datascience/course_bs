# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(posterior)
library(bayesplot)
library(mcmcse)
library(HDInterval

# modelling and data prep ------------------------------------------------------
model <- cmdstan_model("./session_13_miscellaneous/models/mark_recapture.stan")

# prepare the data
# marked on the first day
m <- 8

# caught on the second day
c <- 8

# already marked/remarked
r <- 4

# prepare input data
stan_data <- list(m = m, c = c, r = r)

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
df <- as_draws_df(fit$draws())

# compare mu1 with mu2
ggplot(data = df) +
  geom_density(aes(x = n), color = NA, fill = "skyblue") +
  xlim(0, 100) +
  xlab("n")

# 95% HDI
mean(df$n)
median(df$n)
hdi(df$n, prob = c(0.05, 0.95))
