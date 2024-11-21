data {
  int N;          // number of data points
  array[N] int n; // number of putts at each data point
  vector[N] x;    // distance for each data point
  array[N] int y; // number of successful hits at each data point
}

transformed data {
  // distance parameters
  real d_t = 0.75; // distance tolerance
  real o = 0.2;    // overshot
}

parameters {
  real<lower=0> sigma;
}

model {
  // probabilities
  vector[N] p = Phi((d_t - o) ./ ((x + o)*sigma)) -
                Phi((- o) ./ ((x + o)*sigma));

  // priors
  sigma ~ cauchy(0, 2.5);

  y ~ binomial(n, p);
}

generated quantities {
  // recalculate and return probabilities
  vector[N] p = (Phi((d_t - o) ./ ((x + o)*sigma)) -
                 Phi((- o) ./ ((x + o)*sigma)));
}
