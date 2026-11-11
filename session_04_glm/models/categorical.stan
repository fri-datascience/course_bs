data {
  int<lower=0> n;                  // number of observations
  int<lower=0> m;                  // number of independent variables
  matrix[n, m] x;                  // independent variables
  int<lower=0> k;                  // number of outcomes
  array[n] int<lower=1,upper=k> y; // dependent variable
}

parameters {
  matrix[k-1, m] beta_raw; // k - 1, since one category is our reference
}

transformed parameters {
  // last category is our reference, its coefficients are fixed at 0
  matrix[k, m] beta = append_row(beta_raw, rep_row_vector(0, m));
}

model {
  // fit
  matrix[n, k] x_beta = x * beta';
  for (i in 1:n)
    y[i] ~ categorical_logit(x_beta[i]');
    // or y[i] ~ categorical(softmax(x_beta[i]'));
}
