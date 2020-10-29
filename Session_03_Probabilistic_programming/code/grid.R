# libraries
library(ggplot2)

# set grid resolution
resolution <- 100

# create the grid
grid <- seq(from = 0, to = 1, length.out = resolution)

# prior
prior <- rep(1, resolution)

# data
n <- 12
z <- 5

# likelihood
likelihood <- dbinom(z, n, prob=grid)

# posterior
posterior <- likelihood * prior

# normalize the posterior so it sums to 1
posterior <- posterior / sum(posterior)

# fairness
bottom_cut <- 0.3 * resolution
top_cut <- 0.7 * resolution
fairness <- sum(posterior[bottom_cut:top_cut])
cat(paste0("Fairness of the coin is: ", format(fairness, digits = 3), "."))

# visualize
df <- data.frame(x = grid, y = posterior)
ggplot(data = df, aes(x = x, y = y)) +
  geom_bar(stat="identity", color="#67a9cf", fill="#67a9cf", alpha = 0.5) +
  xlim(0, 1) +
  xlab("") +
  ylab("density") +
  theme_minimal()
