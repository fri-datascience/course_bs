data {
  int<lower=0> n;  // number of observations
  vector[n] t;     // time stamps
  vector[n] y;     // time-series
  int<lower=0> k;  // number of seasonality components
  vector[k] omega; // seasonality frequency
}

parameters {
  // seasonality parameters
  vector[k] beta_cos;
  vector[k] beta_sin;

  // trend parameters
  real alpha;
  real beta;

  // variance
  real<lower=0> sigma;
}

model {
  real ssn;

  for (i in 1:n) {
    // seasonality
    ssn = 0;
    for (j in 1:k) {
      ssn = ssn + beta_cos[j] * cos(omega[j] * t[i]) + beta_sin[j] * sin(omega[j] * t[i]);
    }

    // regression model
    y[i] ~ normal(alpha + beta * t[i] + ssn, sigma);
  }
}
