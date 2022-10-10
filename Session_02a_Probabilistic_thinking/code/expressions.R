# docs -------------------------------------------------------------------------
#
# Used terms:
#   1. almost no chance
#   2. maybe
#   3. I'm sure
#   4. I doubt it
#   5. unlikely
#   6. unbelievable
#   7. very likely
#   8. almost surely
#   9. good chances
#   10. the chances are small
#   11. I strongly believe
#   12. not sure
#   13. probably

# libraries --------------------------------------------------------------------
library(ggplot2)
library(ggdist)
library(tidyverse)

# load the data-----------------------------------------------------------------
df <- read.csv("../data/expressions.csv")

# set factors
df_mean <- df %>%
    group_by(expression) %>%
    summarise(mean_probability = mean(probability)) %>%
    arrange(desc(mean_probability))

df$expression <- factor(df$expression, levels = df_mean$expression)

# plot -------------------------------------------------------------------------
ggplot(df, aes(x = probability)) +
    geom_density(color = NA, fill = "skyblue", alpha = 0.75) +
    geom_point(aes(y = 0, x = probability), shape = 16, color = "grey25") +
    geom_vline(xintercept = 50, linetype = "dashed") +
    facet_wrap(expression ~ ., ncol = 1) +
    xlim(0, 100) +
    xlab("probability [%]") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
