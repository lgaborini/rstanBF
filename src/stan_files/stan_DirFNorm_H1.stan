// Dirichlet - Folded-Normal model
//
// Dirichlet likelihood
// Dirichlet prior parameters are iid samplels from half-Normal (i.e. |X| where X ~ Normal(\mu, \sigma))
//

data {
   int<lower=1> n_ref;        // number of observations
   int<lower=1> p;            // number of turns
   
   // Likelihood
   simplex[p] d_ref[n_ref];   // observations (Dirichlet likelihood)
   
   // Hyperparameters
   vector[p] mu;              // mean
   vector[p] sigma;           // dev.st.
}
parameters {
   // Prior
   vector<lower=0>[p] theta_ref;      // Dirichlet prior
}
model {
   for (i in 1:n_ref) {
      target += dirichlet_lpdf(d_ref[i] | theta_ref);
   }
   for (j in 1:p) {
      // target += normal_lpdf(theta_ref[j] | mu[j], sigma[j]) T[0,];
      // target += normal_lpdf(theta_ref[j] | mu[j], sigma[j]);
      target += normal_lpdf(theta_ref[j] | mu[j], sigma[j]) - normal_lccdf(0 | mu[j], sigma[j]);
   }
   
}
generated quantities {
   // Prior predictive distribution
   simplex[p] sim_d_ref;
   vector<lower=0>[p] sim_theta_ref;
   // Posterior predictive distribution
   simplex[p] pred_d_ref;
   
   // Prior predictive distribution
   for (j in 1:p) {
      sim_theta_ref[j] = fabs(normal_rng(mu[j], sigma[j]));
   }
   sim_d_ref = dirichlet_rng(sim_theta_ref);

   // Posterior predictive distribution
   pred_d_ref = dirichlet_rng(theta_ref);
}
