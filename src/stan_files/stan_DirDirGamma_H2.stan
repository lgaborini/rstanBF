// Dirichlet - Dirichlet improved model
//
// Dirichlet likelihood
// Prior w/ Dirichlet and Gamma for the sum

data {
   // number of observations
   int<lower=1> n_ref;
   int<lower=1> n_quest;
   // number of turns
   int<lower=1> p;            
   
   // observations (Dirichlet likelihood)
   simplex[p] d_ref[n_ref];
   simplex[p] d_quest[n_quest];
   
   // Dirichlet hyperparameters
   vector<lower=0>[p] alpha;
   real<lower=0> alpha_0;
   real<lower=0> beta_0;  
}
parameters {
   // Dirichlet split prior
   simplex[p] rho_ref;   
   simplex[p] rho_quest;   
   real<lower=0> theta_0_ref;
   real<lower=0> theta_0_quest;
}
transformed parameters {
   // Dirichlet prior
   vector<lower=0>[p] theta_ref = theta_0_ref * rho_ref;      
   vector<lower=0>[p] theta_quest = theta_0_quest * rho_quest;      
}
model {
   for (i in 1:n_ref) {
      target += dirichlet_lpdf(d_ref[i] | theta_ref);
   }
   for (i in 1:n_quest) {
      target += dirichlet_lpdf(d_quest[i] | theta_quest);
   }
   target += dirichlet_lpdf(rho_ref | alpha);
   target += gamma_lpdf(theta_0_ref | alpha_0, beta_0);
   target += dirichlet_lpdf(rho_quest | alpha);
   target += gamma_lpdf(theta_0_quest | alpha_0, beta_0);
}
