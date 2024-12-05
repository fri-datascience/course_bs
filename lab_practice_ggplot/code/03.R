# libraries --------------------------------------------------------------------
library(ggplot2)

# plot -------------------------------------------------------------------------
# generate and plot 100 plausible regression lines
df_100 <- data.frame(alpha = -17, beta = rnorm(100, 0.01, 0.002))

# plot
ggplot(df_100) +
  geom_abline(aes(slope = beta, intercept = alpha),
              color = "skyblue", alpha = 0.2, linewidth = 1) +
  theme_minimal() +
  xlim(0, 2000) +
  ylim(-20, 20)
