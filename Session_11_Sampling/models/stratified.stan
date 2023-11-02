data {
  int n;
  int k;
  row_vector[k] w;
  array[n] int id;
  vector[n] y;
}

parameters {
  vector[k] mu;
  vector<lower=0>[k] sigma;
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
