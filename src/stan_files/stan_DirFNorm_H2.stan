// Dirichlet - Folded-Normal model
//
// Dirichlet likelihood
// Dirichlet prior parameters are iid samplels from half-Normal (i.e. |X| where X ~ Normal(\mu, \sigma))
//

data {
   int<lower=1> n_ref;        // number of observations
   int<lower=1> n_quest;      // number of observations
   int<lower=1> p;            // number of turns
   
   // Likelihood
   simplex[p] d_ref[n_ref];       // observations (Dirichlet likelihood)
   simplex[p] d_quest[n_quest];   // observations (Dirichlet likelihood)
   
   // Hyperparameters
   vector[p] mu;              // mean
   vector[p] sigma;           // dev.st.
}
parameters {
   // Prior
   vector<lower=0>[p] theta_ref;        // Dirichlet prior
   vector<lower=0>[p] theta_quest;      // Dirichlet prior
}
model {
   for (i in 1:n_ref) {
      target += dirichlet_lpdf(d_ref[i] | theta_ref);
   }
   for (i in 1:n_quest) {
      target += dirichlet_lpdf(d_quest[i] | theta_quest);
   }
   for (j in 1:p) {
      target += normal_lpdf(theta_ref[j] | mu[j], sigma[j]) - normal_lccdf(0 | mu[j], sigma[j]);
      target += normal_lpdf(theta_quest[j] | mu[j], sigma[j]) - normal_lccdf(0 | mu[j], sigma[j]);
   }
   
}
generated quantities {
   simplex[p] sim_d_ref;
   simplex[p] sim_d_quest;
   vector<lower=0>[p] sim_theta_ref;
   vector<lower=0>[p] sim_theta_quest;
   
   for (j in 1:p) {
      sim_theta_ref[j] = fabs(normal_rng(mu[j], sigma[j]));
      sim_theta_quest[j] = fabs(normal_rng(mu[j], sigma[j]));
   }
   sim_d_ref = dirichlet_rng(sim_theta_ref);
   sim_d_quest = dirichlet_rng(sim_theta_quest);
}
