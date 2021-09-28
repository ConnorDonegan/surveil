// correlated Poisson random walks
data {
  int<lower=1> TT;   // length of time series
  int<lower=1> K;   // J outcomes
  int y[TT, K];     // outcome datda
  vector[K] log_E[TT];  // population at risk
  vector[K] prior_eta_1_df;
  vector[K] prior_eta_1_location;
  vector[K] prior_eta_1_scale;  
  vector[K] prior_sigma_df;
  vector[K] prior_sigma_location;
  vector[K] prior_sigma_scale;
  real<lower=0> prior_omega;  
}

parameters {
  vector[K] eta_1;
  vector[K] z[TT];             // d.eta  
  vector<lower=0>[K] sigma; // scale per group
  cholesky_factor_corr[K] L_Omega;   // correlation between groups
}

transformed parameters {
  vector[K] eta[TT];           // annual risk per group  
  vector[K] mu_y[TT];  
  matrix[K, K] L;
  L = diag_pre_multiply(sigma, L_Omega);
  eta[1] = eta_1;
  for (t in 2:TT) {
    eta[t] = eta[t-1] + L * z[t];
  }
  for (t in 1:TT) mu_y[t] = log_E[t] + eta[t];
}

model {
  target += lkj_corr_cholesky_lpdf(L_Omega | prior_omega);
  target += student_t_lpdf(sigma | prior_sigma_df, prior_sigma_location, prior_sigma_scale);
  target += student_t_lpdf(eta_1 | prior_eta_1_df, prior_eta_1_location, prior_eta_1_scale);
  for (t in 1:TT) {
    target += poisson_log_lpmf(y[t] | mu_y[t]);
    target += std_normal_lpdf(z[t]);
  }
}

generated quantities {
  matrix[K, K] Omega = tcrossprod(L_Omega); 
  vector[TT] rate[K];
  vector[TT] log_lik[K];
  for (t in 1:TT) {
    for (j in 1:K) {
      rate[j, t] = exp(eta[t, j]);
      log_lik[j, t] = poisson_log_lpmf(y[t, j] | mu_y[t, j]);
    }
  }
}


