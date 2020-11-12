# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(bayesplot)
library(posterior)
library(tidyverse)


# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("../models/ordered.stan")

# load data and treat strings as factors
data <- read.csv("../data/poverty.csv", stringsAsFactors=TRUE)

# contrasts
contrasts(data$religion) <- contr.treatment(n_distinct(data$religion))
contrasts(data$degree) <- contr.treatment(n_distinct(data$degree))
contrasts(data$country) <- contr.treatment(n_distinct(data$country))
contrasts(data$gender) <- contr.treatment(n_distinct(data$gender))

# note here that intercept is part of the model matrix (1st column always equals 1)
X <- model.matrix(~ religion + degree + country + gender, data)
# remove intercept since in ordered regression intercepts are cutpoints
X <- X[,-1]

# show a couple of top rows to check if all is OK
head(X)

# check poverty levels
levels(data$poverty)

# fix ordering
data$poverty <- factor(data$poverty, levels = c("Too Little", "About Right", "Too Much"))

# check poverty levels again
levels(data$poverty)

# dependent variable
y <- data$poverty

# stan_data
stan_data <- list(n=nrow(data), m=ncol(X), k=nlevels(y), x=X, y=y)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4
)


# diagnostics ------------------------------------------------------------------
# traceplot for beta parameters
mcmc_trace(fit$draws("beta"))
mcmc_trace(fit$draws("c"))

# summary of betas
fit$summary("beta")
fit$summary("c")


# analysis ---------------------------------------------------------------------
# extract parameters
df_beta <- as_draws_df(fit$draws("beta"))
df_beta <- df_beta %>% select(-.chain, -.iteration, -.draw)
df_cutpoints <- as_draws_df(fit$draws("c"))
df_cutpoints <- df_cutpoints %>% select(-.chain, -.iteration, -.draw)

# plot betas -------------------------------------------------------------------
# rename for ease of addressing
colnames(df_beta) <- c("Religion", "Degree", "USA", "Male")
install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
# to long format
df_beta_long <- df_beta %>% gather(Beta, Value)

# plot
ggplot(data = df_beta_long, aes(x = Value, y = Beta)) +
  stat_eye(fill = "skyblue", alpha = 0.75) +
  xlim(-1.6, 1.6)


# calculate an example probability ---------------------------------------------
mean_beta <- colMeans(df_beta)
mean_c <- colMeans(df_cutpoints)

# x <- (religion, degree, USA, male)
# a non religious educated female from Sweden
x <- c(0, 1, 0, 0)

# Get all three cumulative probabilities ones
f_1 <- 1 / (1 + exp(-(mean_c[1] - as.numeric(mean_beta %*% x))))
f_2 <- 1 / (1 + exp(-(mean_c[2] - as.numeric(mean_beta %*% x))))
f_3 <- 1

# Pr(y = j) = Pr(y <= j) - Pr(y <= j - 1)
Pr_1 = f_1
Pr_2 = f_2 - f_1
Pr_3 = f_3 - f_2
