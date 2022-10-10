data {
  int<lower=0> n;            // number of observations
  int<lower=0> m;            // number of independent variables
  vector[m] x[n];            // independent variables
  int<lower=0> k;            // number of outcomes
  int<lower=1,upper=k> y[n]; // dependent variables
}

transformed data {
  vector[m] zeros;
  zeros = rep_vector(0, m);
}

parameters {
  matrix[k-1, m] beta_raw; // k - 1, since one category is our reference
}

transformed parameters {
  matrix[k, m] beta;
  beta = append_row(beta_raw, zeros'); // last category is our reference
}

model {
  // fit
  for (i in 1:n)
    y[i] ~ categorical_logit(beta * x[i]);
    // or y[i] ~ categorical(softmax(beta * x[i]));
}
