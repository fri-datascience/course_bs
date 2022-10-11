# libraries --------------------------------------------------------------------
library(ggplot2)
library(ggdist)
library(distributional)


# coin fairness calculator -----------------------------------------------------
# set parameters
alpha <- 6
beta <- 8

# bottom and top cuts for fairness
bottom_cut <- 0.3
top_cut <- 0.7

# fairness
fairness <- pbeta(top_cut, alpha, beta) - pbeta(bottom_cut, alpha, beta)
cat(paste0("Fairness of the coin is: ", format(fairness, digits = 3), "."))


# coin fairness visualization --------------------------------------------------
df <- data.frame(dist = "beta")
ggplot(data = df, aes(y = 0, dist = dist, arg1 = alpha, arg2 = beta)) +
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
