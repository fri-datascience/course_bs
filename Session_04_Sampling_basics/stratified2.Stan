data {
  int n;
  int k;
  row_vector[k] w;
  int id[n];
  real y[n];
}

parameters {
  vector[k] mu;
  real<lower=0> sigma[k];
}

model {
  sigma ~ uniform(0, 10);
  for (i in 1:n) {
    y[i] ~ normal(mu[id[i]], sigma[id[i]]);
  }
}

generated quantities {
  real mu_est;
  mu_est = w * mu;
}
