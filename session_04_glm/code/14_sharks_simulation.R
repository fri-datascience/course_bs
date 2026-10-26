# data generating process ------------------------------------------------------
# The causal structure (DAG) behind the simulated data:
#   temperature -> ice cream sales
#   temperature -> shark attacks
#   ice cream sales -> emergency interventions
#   shark attacks -> emergency interventions
# Note that ice cream sales have no causal effect on shark attacks!

# seed for reproducibility
set.seed(32)

# number of observations (days)
n <- 2000

# outside air temperature [°C]
temperature <- rnorm(n, mean = 25, sd = 5)

# the number of ice creams sold increases with temperature
icecream_sales <- rpois(n, lambda = exp(-1 + 0.1 * temperature))

# the number of shark attacks increases with temperature,
# higher temperatures bring more swimmers and with that more attacks
shark_attacks <- rpois(n, lambda = exp(-2 + 0.1 * temperature))

# the number of emergency interventions is a common effect (a collider) of
# ice cream sales (allergic reactions) and shark attacks
emergency_interventions <- rpois(
  n,
  lambda = 0.5 * icecream_sales + 3 * shark_attacks
)

# store the data ---------------------------------------------------------------
data <- data.frame(
  temperature = round(temperature, 3),
  icecream_sales = icecream_sales,
  shark_attacks = shark_attacks,
  emergency_interventions = emergency_interventions
)

write.csv(
  data,
  "./session_04_glm/data/sharks.csv",
  row.names = FALSE
)
