# libraries --------------------------------------------------------------------
library(cmdstanr) # for interfacing Stan
library(ggplot2) # for visualizations
library(ggdist) # for distribution visualizations
library(tidyverse) # for data prep
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse) # for comparing samples and calculating MCSE

# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/bernoulli_beta.stan")

# prepare the data
y <- c(rep(0, 7), rep(1, 9))
n <- length(y)

# prepare input data
stan_data <- list(n = n, y = y)

# fit
fit <- model$sample(
  data = stan_data,
  seed = 1
)

# additional parameters
# chains - number of chains, default = 4
# parallel_chains - how many chains to run in parallel, default = 1
# refresh - reporting frequency, default = 100
# iter_warmup - number of warmup steps, default = 1000
# iter_sampling - number of sampling steps, default = 1000

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws("theta"))

# summary
fit$summary()

# analysis ---------------------------------------------------------------------
# convert theta draws to data frame
df <- as_draws_df(fit$draws("theta"))

# mean and mcse of theta
mcse(df$theta)

# fairness
bottom_cut <- 0.3
top_cut <- 0.7
df_fair <- df %>% filter(theta > bottom_cut & theta < top_cut)
fairness <- nrow(df_fair) / nrow(df)
cat(paste0("Fairness of the coin is: ", format(fairness, digits = 3), "."))

# plot
ggplot(data = df, aes(x = theta)) +
  stat_slab(aes(fill = after_stat(x < bottom_cut | x > top_cut)),
    alpha = 0.75,
    show.legend = FALSE,
    normalize = "none",
    scale = 1
  ) +
  xlim(0, 1) +
  ylim(0, 4) +
  ylab("density") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "grey90"))
