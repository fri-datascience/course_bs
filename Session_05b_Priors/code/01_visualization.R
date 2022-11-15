# libraries --------------------------------------------------------------------
library(ggplot2)

# generate data ----------------------------------------------------------------
n <- 1000
x_max <- 10
x <- seq(-x_max, x_max, length.out = 1000)

df <- data.frame(x = numeric(), Density = numeric(), Distribution = character())
dist <- c("Normal", "Cauchy", "Cauchy - informative", "Uniform")

for (i in 1:n) {
  y_norm <- dnorm(x[i], mean = 0, sd = 2.5)
  y_cauchy <- dcauchy(x[i], location = 0, scale = 2.5)
  y_cauchy_i <- dcauchy(x[i], location = 0, scale = 0.5)
  y_uniform <- 1 / (2 * x_max)
  df <- rbind(df, data.frame(x = x[i],
                             Density = c(y_norm,
                                         y_cauchy,
                                         y_cauchy_i,
                                         y_uniform),
                             Distribution = dist))
}

# plot -------------------------------------------------------------------------
ggplot(data = df, aes(x = x, y = Density, color = Distribution)) +
  geom_line(size = 1) +
  theme_minimal() +
  scale_color_manual(values = c("#9ebcda",
                                "tomato",
                                "#8856a7",
                                "black"))
