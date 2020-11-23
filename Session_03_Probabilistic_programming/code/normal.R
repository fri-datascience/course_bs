# libraries --------------------------------------------------------------------
library(cmdstanr)  # for interfacing Stan
library(ggplot2)   # for visualizations
library(ggdist)    # for distribution visualizations
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse)    # for comparing samples and calculating MCSE


# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/normal.stan")

# prepare the data
# number of measurements
n <- 20

# tall
y1 <- rnorm(20, 180, 20)

# not so tall
y2 <- rnorm(20, 170, 20)

# merge
y <- c(y1, y2)

# group markers
g <- c(rep(1,n),rep(2,n))

# prepare input data
stan_data <- list(n = 2*n, y = y, g=g)

# fit
fit <- model$sample(
  data = stan_data
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
  geom_density(aes(x=mu1), color=NA, fill="skyblue", alpha=0.5) +
  geom_density(aes(x=mu2), color=NA, fill="tomato", alpha=0.5) +
  xlab("mu")
               
# diff
mcse(df$diff)

# probability mu1 > mu2
mcse(df$diff < 0)

# 95 CI
q95 <- quantile(df$diff, 0.95)

# plot diff
ggplot(data = df) +
  geom_density(aes(x=diff), color=NA, fill="skyblue", alpha=0.75) +
  xlab("difference") +
  geom_vline(xintercept = q95, linetype="dashed", color = "grey75", size=1.5)
