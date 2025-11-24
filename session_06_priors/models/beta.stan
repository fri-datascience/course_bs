data {
  int<lower=1> n;
  vector[n] y;
}

parameters {
  real<lower=0> a;
  real<lower=0> b;
}

model {
  y ~ beta(a, b);
}
