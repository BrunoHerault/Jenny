data {
  int<lower=0> N;
  real ptot[N]; 
  real clay[N];
}

parameters {
  real<lower=1, upper=15> theta_0;
  real<lower=1, upper=50> theta_1;
  real<lower=1, upper=50> theta_2;
  real<lower=0.001, upper=2> sigma;
}

model{
  real mu[N];
  for (i in 1:N)
  {
    mu[i]=theta_0+(theta_1*clay[i])/(theta_2+clay[i]);
  }
  ptot ~ normal(mu, sigma);
  sigma ~ gamma(0.01, 0.01);
}
