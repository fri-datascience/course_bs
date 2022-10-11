# Based on code developed by Olivier Gimenez
# https://gist.github.com/oliviergimenez
# https://gist.github.com/oliviergimenez/5ee33af9c8d947b72a39ed1764040bf3


# libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)


# Metropolis-Hastings MCMC based Bayesian inference ----------------------------
# prior
prior <- function(theta) {
  dunif(x = theta, min = 0, max = 1)
}

# likelihood
likelihood <- function(z, n, theta) {
  dbinom(x = z, size = n, prob = theta)
}

# posterior density function (log scale)
posterior <- function(z, n, theta) {
  likelihood(z, n, theta) * prior(theta)
}

# propose candidate value
move_log <- function(x, step = .2) {
  logitx <- log(x / (1 - x))
  logit_candidate <- logitx + rnorm(1, 0, step)
  candidate <- plogis(logit_candidate)
  return(candidate)
}

move <- function(p, step = .2) {
  p_candidate <- p + rnorm(1, 0, step)
  candidate <- plogis(p_candidate)
  return(candidate)
}

# metropolis-hastings
metropolis <- function(steps = 1000, init = 0.5) {
  # theta storage
  theta_post <- rep(NA, steps)

  # start
  theta_post[1] <- init

  for (t in 2:steps) {
    # propose next candidate value
    theta_star <- move(theta_post[t - 1])

    # calculate ratio
    p_star <- posterior(theta_star)
    p_prev <- posterior(theta_post[t - 1])
    log_r <- p_star - p_prev
    r <- exp(log_r)

    # decide to accept candidate value or to keep current value
    accept <- rbinom(1, 1, prob = min(r, 1))
    theta_post[t] <- ifelse(accept == 1, theta_star, theta_post[t - 1])
  }
  theta_post
}

chain1 <- metropolis(init = 0.5)



# fairness calculation ---------------------------------------------------------
bottom_cut <- 0.3 * resolution
top_cut <- 0.7 * resolution
fairness <- sum(posterior[bottom_cut:top_cut])
cat(paste0("Fairness of the coin is: ", format(fairness, digits = 3), "."))


# fairness visualization -------------------------------------------------------
df <- data.frame(x = grid, y = posterior)
ggplot(data = df, aes(x = x, y = y)) +
  geom_bar(stat = "identity", color = "skyblue",
           fill = "skyblue", alpha = 0.75) +
  xlim(0, 1) +
  xlab("") +
  ylab("density") +
  theme_minimal()
