data {
  int N;          // number of data points
  array[N] int n; // number of putts at each data point
  vector[N] x;    // distance for each data point
  array[N] int y; // number of successful hits at each data point
}

parameters {
  real a;
  real b;
}

model {
  // priors
  b ~ cauchy(0, 2.5);

  y ~ binomial_logit(n, a + x*b);
}
