functions {
  real zero_centered_lpdf(real x, real sigma) {
    real result = 0.0;
    result = 1 / (sigma * sqrt(2 * pi()));
    result = result * (e() ^ (-1.0/2.0 * ((x / sigma)^2)));
    return log(result);
  }
}

data {
  int<lower=1> n; // total number of data points
  vector[n] y;    // data points
}

parameters {
  real<lower=0> sigma; // stdev
}

model {
  // custom lpdf - zero_centered
  for (i in 1:n) {
    y[i] ~ zero_centered(sigma);
    // OR
    // target += zero_centered_lpdf(y[i] | sigma);
  }
}
