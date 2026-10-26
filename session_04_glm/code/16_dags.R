# libraries --------------------------------------------------------------------
library(dagitty) # for defining, analyzing and plotting DAGs

# common causal structures -----------------------------------------------------
# in all three structures we are interested in the effect of X1 (the exposure)
# on Y (the outcome)
confounder <- dagitty("dag {
  X1 [exposure]
  Y [outcome]
  X2 -> X1
  X2 -> Y
}")

collider <- dagitty("dag {
  X1 [exposure]
  Y [outcome]
  X1 -> X2
  Y -> X2
}")

mediator <- dagitty("dag {
  X1 [exposure]
  Y [outcome]
  X1 -> X2
  X2 -> Y
}")

# node positions for plotting (y increases downwards)
positions <- list(
  x = c(X1 = 0, X2 = 1, Y = 2),
  y = c(X1 = 1, X2 = 0, Y = 1)
)
coordinates(confounder) <- positions
coordinates(collider) <- positions
coordinates(mediator) <- positions

# variables we have to adjust for (include in the model) to estimate
# the effect of X1 on Y, an empty set means that no adjustment is needed
adjustmentSets(confounder) # adjust for X2
adjustmentSets(collider) # no adjustment
adjustmentSets(mediator) # no adjustment for the total effect
adjustmentSets(mediator, effect = "direct") # adjust for X2 for the direct effect

# plot the three structures side by side
png("./session_04_glm/figs/causal_structures.png",
  width = 8, height = 3.2, units = "in", res = 120
)
par(mfrow = c(1, 3), mar = rep(0, 4), cex = 1.2)
structures <- list(
  Confounder = confounder,
  Collider = collider,
  Mediator = mediator
)
for (s in names(structures)) {
  plot(structures[[s]], xlim = c(-0.3, 2.3), ylim = c(-1.25, 0.55))
  mtext(s, side = 3, line = -1.6, cex = 1.2)
}
dev.off()

# the shark attacks example ----------------------------------------------------
# T - temperature, I - ice cream sales, S - shark attacks,
# E - emergency interventions
sharks <- dagitty("dag {
  I [exposure]
  S [outcome]
  T -> I
  T -> S
  I -> E
  S -> E
}")
coordinates(sharks) <- list(
  x = c(T = 1, I = 0, S = 2, E = 1),
  y = c(T = 0, I = 1, S = 1, E = 2)
)

# to estimate the effect of ice cream sales on shark attacks,
# we have to adjust for temperature
adjustmentSets(sharks) # adjust for T

# conditional independencies implied by the DAG (these can be tested with data),
# for example, ice cream sales and shark attacks are independent given
# temperature
impliedConditionalIndependencies(sharks)

# plot the DAG
png("./session_04_glm/figs/sharks_dag.png",
  width = 8, height = 4.5, units = "in", res = 120
)
par(mar = rep(0, 4), cex = 1.2)
plot(sharks, node.names = c(
  T = "temperature (T)",
  I = "ice cream sales (I)",
  S = "shark attacks (S)",
  E = "emergency interventions (E)"
))
dev.off()
