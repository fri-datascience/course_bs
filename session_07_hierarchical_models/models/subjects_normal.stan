data {
  int<lower=0> n; // number of observations
  int<lower=0> m; // number of subjects
  vector[n] y;    // observations
  array[n] int s; // subject indexes
}

parameters {
  real<lower=0> sigma;
  vector[m] mu;
}

model {
  // normal model for each group
  for (i in 1:n) {
      y[i] ~ normal(mu[s[i]], sigma);
  }
}
