data {
  int N;       // number of data points
  int n[N];    // number of puts at each data poing
  vector[N] x; // distance for each data point
  int y[N];    // number of hits at each data point
}

parameters {
  real a;
  real b;
}

model {
  // priors
  a ~ cauchy(0, 2.5);
  b ~ cauchy(0, 2.5);
  
  y ~ binomial_logit(n, a + b*x);
}
