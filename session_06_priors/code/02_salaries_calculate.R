# libraries --------------------------------------------------------------------
library(tidyverse)

# load the data ----------------------------------------------------------------
# full survey export, we only need column 7:
# "Povprečna mesečna BRUTO plača v zadnjih treh mesecih."
df_raw <- read.csv(
  "./session_06_priors/data/salaries_2026_tmp.csv",
  check.names = FALSE
)
df <- data.frame(salary = df_raw[[7]])

# remove missing values (empty answers)
df <- df %>%
  mutate(salary = trimws(salary)) %>%
  filter(!is.na(salary), salary != "")

# unique values
unique(df$salary)

# mappings ---------------------------------------------------------------------
salary_mapping <- c(
  "1481,88€ (minimalna BRUTO plača)" = 1482,
  "1481,89€ - 1499€" = 1490,
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

# check that every answer has a mapping
unmapped <- setdiff(unique(df$salary), names(salary_mapping))
if (length(unmapped) > 0) {
  stop("Unmapped salary values: ", paste(unmapped, collapse = ", "))
}

# apply mapping
df$salary <- unname(salary_mapping[df$salary])

# save -------------------------------------------------------------------------
write.csv(df, "./session_06_priors/data/salaries_2026.csv", row.names = FALSE)
