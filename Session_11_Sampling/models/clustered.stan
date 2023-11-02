data {
  int n;
  int k;
  simplex[k] w;
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
  real mu_est = 0;
  real tot_w = 0;
  
  for (i in 1:k) {
     int idx = categorical_rng(w);
     tot_w += w[idx];
     mu_est += w[idx] * mu[idx];
  }
  
  mu_est /= tot_w;

}
