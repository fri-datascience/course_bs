# Based on code developed by Olivier Gimenez
# https://gist.github.com/oliviergimenez
# https://gist.github.com/oliviergimenez/5ee33af9c8d947b72a39ed1764040bf3


# libraries --------------------------------------------------------------------
library(ggdist)
library(ggplot2)
library(tidyverse)


# support functions ------------------------------------------------------------
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
move <- function(theta, step = .2) {
  theta_candidate <- theta + rnorm(1, 0, step)
  theta_candidate <- min(1, max(0, theta_candidate))
  return(theta_candidate)
}

# metropolis-hastings
metropolis <- function(z, n, steps = 1000, init = 0.5) {
  # theta storage
  theta_posterior <- rep(NA, steps)

  # start
  theta_posterior[1] <- init

  for (t in 2:steps) {
    # propose next candidate value
    theta_candidate <- move(theta_posterior[t - 1])

    # calculate ratio
    likelihood_candidate <- posterior(z, n, theta_candidate)
    likelihood_previous <- posterior(z, n, theta_posterior[t - 1])
    likelihood_ratio <- likelihood_candidate / likelihood_previous

    # decide to accept candidate value or to keep current value
    accept <- rbinom(1, 1, prob = min(likelihood_ratio, 1))
    theta_posterior[t] <-
      ifelse(accept, theta_candidate, theta_posterior[t - 1])
  }

  # return the posterior parameter samples
  theta_posterior
}


# MCMC based inference ---------------------------------------------------------
z <- 5
n <- 12

chain1 <- metropolis(z, n)
chain2 <- metropolis(z, n, init = 0.25)
chain3 <- metropolis(z, n, init = 0.75)

# plot chains through time
df_chains <- data.frame(x = seq_len(length(chain1)),
                        theta = chain1, chain = "1")

df_chains <- df_chains %>%
  add_row(data.frame(x = seq_len(length(chain2)), theta = chain2, chain = "2"))

df_chains <- df_chains %>%
  add_row(data.frame(x = seq_len(length(chain3)), theta = chain3, chain = "3"))

ggplot(df_chains, aes(x = x, y = theta, color = chain)) +
  geom_line() +
  scale_color_brewer(type = "qual", palette = "Set1")


# fairness calculation ---------------------------------------------------------
bottom_cut <- 0.3
top_cut <- 0.7
df_chains$fair <- df_chains$theta > bottom_cut & df_chains$theta < top_cut
fairness <- sum(df_chains$fair) / nrow(df_chains)
cat(paste0("Fairness of the coin is: ", format(fairness, digits = 3), "."))


# fairness visualization -------------------------------------------------------
ggplot(data = df_chains, aes(x = theta)) +
  stat_dist_slab(aes(fill = stat(x < bottom_cut | x > top_cut)),
                    alpha = 0.75,
                    show.legend = FALSE,
                    normalize = "none",
                    scale = 1) +
  xlim(0, 1) +
  ylim(0, 3) +
  xlab("") +
  ylab("density") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "grey90"))
