data {
  int<lower=0> m;
  int<lower=0> c;
  int<lower=0,upper=min(m,c)> r;
}

parameters {
  real<lower=(c - r + m)> n;
}

model {
  r ~ binomial(c, m / n);
}
