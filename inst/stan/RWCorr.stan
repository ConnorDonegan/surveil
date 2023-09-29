// correlated Poisson random walks
data {
  int<lower=1> TT;   // length of time series
  int<lower=1> K;   // J outcomes
  array[TT, K] int y;     // outcome datda
  array[TT, K] int population;  
  array[TT] vector[K] log_E;  // log population at risk
  int is_poisson;
  int is_binomial;
  vector[K] prior_eta_1_location;
  vector[K] prior_eta_1_scale;  
  vector[K] prior_sigma_location;
  vector[K] prior_sigma_scale;
  real<lower=0> prior_omega;
}

parameters {
  vector[K] eta_1;
  array[TT] vector[K] z;             // d.eta  
  vector<lower=0>[K] sigma; // scale per group
  cholesky_factor_corr[K] L_Omega;   // correlation between groups
}

transformed parameters {
  array[TT] vector[K] eta;           // annual risk per group  
  array[is_poisson ? TT : 0] vector[is_poisson ? K : 0] mu_y;  
  matrix[K, K] L;
  L = diag_pre_multiply(sigma, L_Omega);
  eta[1] = eta_1;
  for (t in 2:TT) {
    eta[t] = eta[t-1] + L * z[t];
  }
  if (is_poisson) for (t in 1:TT) mu_y[t] = log_E[t] + eta[t];
}

model {
  target += lkj_corr_cholesky_lpdf(L_Omega | prior_omega);
  target += normal_lpdf(sigma | prior_sigma_location, prior_sigma_scale);
  target += normal_lpdf(eta_1 | prior_eta_1_location, prior_eta_1_scale);
  if (is_poisson) {
  for (t in 1:TT) {
    target += poisson_log_lpmf(y[t] | mu_y[t]);
    target += std_normal_lpdf(z[t]);
  }}
  if (is_binomial) {
  for (t in 1:TT) {
    target += binomial_logit_lpmf(y[t] | population[t], eta[t]);
    target += std_normal_lpdf(z[t]);
  }} 
}

generated quantities {
  matrix[K, K] Omega = tcrossprod(L_Omega); 
  array[K] vector[TT] rate;
  array[K] vector[TT] log_lik;
  if (is_poisson) {
    for (t in 1:TT) {
      for (j in 1:K) {
	rate[j, t] = exp(eta[t, j]);
	log_lik[j, t] = poisson_log_lpmf(y[t, j] | mu_y[t, j]);
      }
    }
  }
  if (is_binomial) {
    for (t in 1:TT) {
      for (j in 1:K) {
	rate[j, t] = inv_logit(eta[t, j]);
	log_lik[j, t] = binomial_logit_lpmf(y[t, j] | population[t, j], eta[t, j]);
      }
    }
  }  
}


