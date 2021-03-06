#' ML estimator of Dirichlet parameters
#'
#' From package `sirt`:
#' https://github.com/alexanderrobitzsch/sirt
#'
#' GPL3 license


#' Derivative of digamma function
#'
#' Derivative of digamma function.
#' @param x x
#' @param h dx
#' @keywords internal
digamma1 <- function(x,h = .001){
   ( digamma(x+h) - digamma(x-h) ) / (2*h)
}

#' Maximum Likelihood Estimation of the Dirichlet Distribution
#'
#' Maximum likelihood estimation of the parameters of the Dirichlet distribution
#' @param x x
#' @return A list with following entries
#'
#' `alpha` Vector of α parameters
#' `alpha0` The concentration parameter $α_0=∑_k α_k$
#' `xsi` Vector of proportions $ξ_k=α_k / α_0$
#' @keywords internal
dirichlet.mle <- function(
   x,
   weights = NULL,
   eps = 10^(-5),
   convcrit = .00001,
   maxit = 1000,
   oldfac = .3,
   progress = FALSE){

   #***
   N <- nrow(x)
   K <- ncol(x)
   # compute log pbar
   x <- ( x+eps ) / ( 1 + 2*eps )
   x <- x / rowSums(x)
   N <- nrow(x)

   if ( is.null(weights) ){
      weights <- rep(1,N) }
   weights <- N * weights / sum( weights )
   #    log.pbar <- colMeans( log( x+eps ) )
   log.pbar <- colMeans( weights * log( x ) )
   # compute inits
   #    alphaprob <- colMeans( x  )
   #    p2 <- mean( x[,1]^2  )
   alphaprob <- colMeans( x * weights )
   p2 <- mean( x[,1]^2 * weights )
   xsi <- ( alphaprob[1] - p2 ) / ( p2 - ( alphaprob[1] )^2 )
   alpha <- xsi * alphaprob
   K1 <- matrix(1,K,K)
   conv <- 1
   iter <- 1
   #******************************
   # BEGIN iterations
   while( ( conv > convcrit ) & (iter < maxit) ){
      alpha0 <- alpha
      g <- N * digamma( sum(alpha ) ) - N * digamma(alpha) + N * log.pbar
      z <- N * digamma1( sum(alpha ))
      H <- diag( -N*digamma1( alpha ) ) + z
      alpha <- alpha0 - solve(H, g )
      alpha[ alpha < 0 ] <- 10^(-10)
      alpha <- alpha0 + oldfac*( alpha - alpha0 )
      conv <- max( abs( alpha0 - alpha ) )
      if (progress){     print( paste( iter, sum(alpha), conv) ) }
      iter <- iter+1
      utils::flush.console()
   }
   alpha0 <- sum(alpha)
   xsi <- alpha / alpha0
   res <- list( "alpha"=alpha, "alpha0"=alpha0, "xsi"=xsi )
   return(res)
}
