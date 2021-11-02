data {
  int N;       // number of data points
  int n[N];    // number of putts at each data point
  vector[N] x; // distance for each data point
  int y[N];    // number of successful hits at each data point
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
