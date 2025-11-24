data {
  int<lower=1> n;
  vector[n] y;
}

parameters {
  real<lower=0> k;
  real<lower=0> t;
}

model {
  y ~ gamma(k, t);
}
