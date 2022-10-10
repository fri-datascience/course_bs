data {
  int<lower=0> n; // number of observations
  int<lower=0> m; // number of groups
  vector[n] y;    // observations
  int g[n];       // group indexes
}

parameters {
  real<lower=0> sigma;
  vector[m] mu;
}

model {
  // normal model for each group
  for (i in 1:n) {
      y[i] ~ normal(mu[g[i]], sigma);
  }
}
