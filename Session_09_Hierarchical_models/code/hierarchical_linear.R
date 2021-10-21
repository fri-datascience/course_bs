# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)
library(cowplot)


# data prep and model compilation ----------------------------------------------
# load data
data <- read.csv("../data/adaptation_level.csv")

# separate groups and parts
group1_part1 <- data %>% filter(group == 1 & part == 1)
group1_part2 <- data %>% filter(group == 1 & part == 2)
group2_part1 <- data %>% filter(group == 2 & part == 1)
group2_part2 <- data %>% filter(group == 2 & part == 2)

# model 
model <- cmdstan_model("../models/hierarchical_linear.stan")


# fit all four models ----------------------------------------------------------
# group1_part1
stan_data <- list(n=nrow(group1_part1),
                  m=max(group1_part1$subject),
                  x=group1_part1$sequence,
                  y=group1_part1$response,
                  s=group1_part1$subject)

fit11 <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# group1_part2
stan_data <- list(n=nrow(group1_part2),
                  m=max(group1_part2$subject),
                  x=group1_part2$sequence,
                  y=group1_part2$response,
                  s=group1_part2$subject)

# fit
fit12 <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# group2_part1
stan_data <- list(n=nrow(group2_part1),
                  m=max(group2_part1$subject),
                  x=group2_part1$sequence,
                  y=group2_part1$response,
                  s=group2_part1$subject)

# fit
fit21 <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# group2_part2
stan_data <- list(n=nrow(group2_part2),
                  m=max(group2_part2$subject),
                  x=group2_part2$sequence,
                  y=group2_part2$response,
                  s=group2_part2$subject)

# fit
fit22 <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# trace plots of top level parameters
mcmc_trace(fit11$draws(c("mu_a", "sigma_a", "mu_b", "sigma_b", "mu_s", "sigma_s")))
mcmc_trace(fit12$draws(c("mu_a", "sigma_a", "mu_b", "sigma_b", "mu_s", "sigma_s")))
mcmc_trace(fit21$draws(c("mu_a", "sigma_a", "mu_b", "sigma_b", "mu_s", "sigma_s")))
mcmc_trace(fit22$draws(c("mu_a", "sigma_a", "mu_b", "sigma_b", "mu_s", "sigma_s")))

# summary of top level parameters
fit11$summary(c("mu_a", "sigma_a", "mu_b", "sigma_b", "mu_s", "sigma_s"))
fit12$summary(c("mu_a", "sigma_a", "mu_b", "sigma_b", "mu_s", "sigma_s"))
fit21$summary(c("mu_a", "sigma_a", "mu_b", "sigma_b", "mu_s", "sigma_s"))
fit22$summary(c("mu_a", "sigma_a", "mu_b", "sigma_b", "mu_s", "sigma_s"))


# visual posterior check -------------------------------------------------------
# number of lines
n_lines <- 20

# use a different fit or data for other parts of the experiment
# e.g. fit22 for group2_part2
fit <- fit12
data <- group1_part2

# extract subject level parameters
df_check <- as_draws_df(fit$draws(c("alpha", "beta")))
df_check <- df_check %>% select(-.draw, -.chain, -.iteration)

df_check <- sample_n(df_check, n_lines)

n_subjects <- ncol(df_check) / 2

# prep for plotting
df_subjects <- data.frame(subject=numeric(),
                          iteration=factor(),
                          alpha=numeric(),
                          beta=numeric())

for (i in 1:n_subjects) {
  # get beta column indexes
  b <- i + n_subjects
  
  # [[1]] because df_check[,i] is a list with values at [[1]]
  # in a way weird but that is how tidyverse tibbles work
  df_subjects <- rbind(df_subjects, data.frame(subject=i,
                                               iteration=as.factor(seq(1,n_lines)),
                                               alpha=df_check[,i][[1]],
                                               beta=df_check[,b][[1]]))
}

# plot
ggplot() +
  geom_jitter(data=data,
             aes(x=sequence, y=response),
             color="skyblue",
             shape=16, alpha=0.6) +
  geom_abline(data=df_subjects,
              aes(slope=beta,
                  intercept=alpha),
              color="skyblue",
              alpha=0.3,
              size=1) +
  facet_wrap(. ~ subject, ncol=5) +
  ylab("Response") +
  xlab("Question index") +
  scale_x_continuous(breaks=seq(1,10)) +
  ylim(0, 10)


# analysis ---------------------------------------------------------------------
# extract group level parameters
df_11_full <- as_draws_df(fit11$draws(c("mu_a", "mu_b")))
df_11_full <- df_11_full %>% select(-.draw, -.chain, -.iteration)
df_12_full <- as_draws_df(fit12$draws(c("mu_a", "mu_b")))
df_12_full <- df_12_full %>% select(-.draw, -.chain, -.iteration)
df_21_full <- as_draws_df(fit21$draws(c("mu_a", "mu_b")))
df_21_full <- df_21_full %>% select(-.draw, -.chain, -.iteration)
df_22_full <- as_draws_df(fit22$draws(c("mu_a", "mu_b")))
df_22_full <- df_22_full %>% select(-.draw, -.chain, -.iteration)

# compare intercepts
mcse(df_12_full$mu_a > df_22_full$mu_a)
df_compare <- data.frame(diff=df_12_full$mu_a-df_22_full$mu_a)

# plot
ggplot(data=df_compare, aes(x=diff)) +
  geom_histogram(bins=100, alpha=0.75) +
  xlim(-4, 4) +
  xlab("Difference")

# subsample
n_lines <- 20
df_11 <- sample_n(df_11_full, n_lines)
df_12 <- sample_n(df_12_full, n_lines)
df_21 <- sample_n(df_21_full, n_lines)
df_22 <- sample_n(df_22_full, n_lines)

# add group and part data
df_11$group <- 1
df_12$group <- 1
df_21$group <- 2
df_22$group <- 2
df_11$part <- 1
df_12$part <- 2
df_21$part <- 1
df_22$part <- 2

# bind together groups
df_1 <- rbind(df_11, df_21)
df_2 <- rbind(df_12, df_22)

# cast to factors
df_1$group <- as.factor(df_1$group)
df_2$group <- as.factor(df_2$group)

# plot
g1 <- ggplot() +
  geom_abline(data=df_1,
              aes(slope=mu_b,
                  intercept=mu_a,
                  colour=group),
              alpha=0.5,
              size=1) +
  ylab("Response") +
  xlab("Question index") +
  scale_x_continuous(breaks=seq(1,10), limits=c(1,10)) +
  ylim(0, 10) +
  scale_color_manual(values=c("skyblue", "tomato")) +
  ggtitle("Part I") +
  theme(legend.position = "none")

# plot
g2 <- ggplot() +
  geom_abline(data=df_2,
              aes(slope=mu_b,
                  intercept=mu_a,
                  colour=group),
              alpha=0.5,
              size=1) +
  ylab("Response") +
  xlab("Question index") +
  scale_x_continuous(breaks=seq(1,10), limits=c(1,10)) +
  ylim(0, 10) +
  scale_color_manual(values=c("skyblue", "tomato")) +
  ggtitle("Part II") +
  theme(legend.position = "none")

# cowplot
plot_grid(g1, g2, scale=0.9, ncol=2)

