# libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)

# load the data ----------------------------------------------------------------
df <- read.csv("./session_06_priors/data/salaries_2025.csv")

# unique values
unique(df$salary)

# mappings
salary_mapping <- c(
  "1277.72€ (minimalna BRUTO plača)" = 1278,
  "1277.73€ - 1499€" = 1388,
  "1500€ - 1999€" = 1750,
  "2000€ - 2499€" = 2250,
  "2500€ - 2999€" = 2750,
  "3000€ - 3499€" = 3250,
  "3500€ - 3999€" = 3750,
  "4000€ - 4499€" = 4250,
  "4500€ - 4999€" = 4750,
  "5000€ - 5499€" = 5250,
  "5500€ - 5999€" = 5750,
  "6000€ - 6499€" = 6250,
  "6500€ - 6999€" = 6750,
  "7000€ - 7499€" = 7250,
  "7500€ - 7999€" = 7750,
  "8000€ - 8499€" = 8250,
  "8500€ - 8999€" = 8750,
  "9000€ - 9499€" = 9250,
  "9500€ - 9999€" = 9750,
  "več kot 10000€" = 10000
)

# apply mapping
df$salary <- salary_mapping[df$salary]

# save back to the same file
write.csv(df, "./session_06_priors/data/salaries_2025.csv", row.names = FALSE)
