# Contains implemented models
#


# Global variables: not exported but visible using rstanBF:::
env_stanBF <- new.env(parent = emptyenv())

# Short names
env_stanBF$stanBF_model_shortnames <- c('DirDir', 'DirFNorm', 'DirDirGamma', 'NormNorm')

# List of Stan modules in each hypothesis
# The name of each module is the name of the Stan file without extension
env_stanBF$stanBF_modules <- list()
env_stanBF$stanBF_modules[['DirDir']] <- list(H1='stan_DirDir_H1', H2='stan_DirDir_H2')
env_stanBF$stanBF_modules[['DirFNorm']] <- list(H1='stan_DirFNorm_H1', H2='stan_DirFNorm_H2')
env_stanBF$stanBF_modules[['DirDirGamma']] <- list(H1='stan_DirDirGamma_H1', H2='stan_DirDirGamma_H2')
env_stanBF$stanBF_modules[['NormNorm']] <- list(H1='stan_NormNorm_H1', H2='stan_NormNorm_H2')
# env_stanBF$stanBF_modules[['logNormNHN']] <- list(H1='stan_logNorm_H1', H2='stan_logNorm_H2')

# Full names for the model
env_stanBF$stanBF_model_names <- list()
env_stanBF$stanBF_model_names[['DirDir']] <- 'Dirichlet-Dirichlet'
env_stanBF$stanBF_model_names[['DirFNorm']] <- 'Dirichlet-FoldedNormal'
env_stanBF$stanBF_model_names[['DirDirGamma']] <- 'Dirichlet-DirichletGamma'
env_stanBF$stanBF_model_names[['NormNorm']] <- 'Normal-Normal'
# env_stanBF$stanBF_model_names[['logNormNHN']] <- 'logNormal-NormalFNormal'

# Required hyperparameters
env_stanBF$stanBF_default_hyperpriors <- list()
env_stanBF$stanBF_default_hyperpriors[['DirDir']] <- c('alpha')
env_stanBF$stanBF_default_hyperpriors[['DirFNorm']] <- c('mu', 'sigma')
env_stanBF$stanBF_default_hyperpriors[['DirDirGamma']] <- c('alpha', 'alpha_0', 'beta_0')
env_stanBF$stanBF_default_hyperpriors[['NormNorm']] <- c('mu_mu0', 'mu_sigma0', 'sigma_mu0', 'sigma_sigma0')
# env_stanBF$stanBF_default_hyperpriors[['logNormNHN']] <- c('mu_0', 'sigma_0', 'sigma_s')


# S3 child classes for stan_BF objects
# Specialize methods (plot_posterior, samples, ...) depending on the likelihood

# 'stanBF_turn' objects share the Dirichlet likelihood
env_stanBF$stanBF_child_class <- list()
env_stanBF$stanBF_child_class[['DirDir']] <- 'stanBF_turn'
env_stanBF$stanBF_child_class[['DirFNorm']] <- 'stanBF_turn'
env_stanBF$stanBF_child_class[['DirDirGamma']] <- 'stanBF_turn'

# Export functions --------------------------------------------------------

#' Get available models in rstanBF
#'
#' Get available models in rstanBF, as short names.
#' @return character vector of models
#' @export
available_models <- function(){
   env_stanBF$stanBF_model_shortnames
}
