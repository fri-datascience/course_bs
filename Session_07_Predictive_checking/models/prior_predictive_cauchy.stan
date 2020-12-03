data {
  int<lower = 0> n; // number of observations 
  vector[n] x;      // weights
}

generated quantities {
  // priors
  real alpha = cauchy_rng(0, 2.5);
  real beta = normal_rng(0, 1);
  real sigma = uniform_rng(0, 50);
  
  // simulated data
  real y_sim[n] = normal_rng(alpha + beta * x, sigma);
}