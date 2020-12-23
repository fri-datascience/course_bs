data {
  int<lower=0> n;    // total number of measurements
  int<lower=0> m;    // number of subjects
  vector[n] x;       // sequence/time
  vector[n] y;       // response
  int<lower=0> s[n]; // subject ids
}

parameters {
  // subject level parameters  
  vector[m] alpha;
  vector[m] beta;
  vector<lower=0>[m] sigma;

  // global parameters
  real mu_a;
  real mu_b;
  real<lower=0> mu_s;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma_s;
}

model {
  // hierarchical links between subjects
  alpha ~ normal(mu_a, sigma_a);
  beta ~ normal(mu_b, sigma_b);
  sigma ~ normal(mu_s, sigma_s);

  // subject level modelling - normal linear regression
  for (i in 1:n)
    y[i] ~ normal(alpha[s[i]] + beta[s[i]] * x[i], sigma[s[i]]);
}