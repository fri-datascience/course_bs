# libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)

# load the data-----------------------------------------------------------------
df <- read.csv("../data/50_startups.csv")

# plot -------------------------------------------------------------------------
# to long format
df_funds <- df %>%
    select(administration, research, marketing, profit) %>%
    gather(field, funds)

# plot
ggplot(df_funds, aes(x = funds)) +
    geom_density(color = NA, fill = "grey50") +
    facet_wrap(~ field, ncol = 2)
