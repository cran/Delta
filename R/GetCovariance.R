#' Calculate Covariance function
#'
#' This function calculate covariance for combinations Cov(Delta,Delta), Cov(Delta,Pi) and Cov(Pi,Pi).
#' 
#' @param mx Matrix. Modified matrix to have a solution Usually GetMx$M2.
#' @param Delta Vector. Each element indicate the probability of recognize an element i.
#' @param Pi Vector. Each element indicate the probability of classify at random an element in category i.
#' @param B Double. Numerical solution to the equation given by the model.
#' @keywords Delta mx Pi B k Covarianze
#' @export
#' @examples
#' GetCovariance(mx = matrix(c(1.5,0.5,0.5,0.5,2.5,0.5,0.5,0.5,3.5),3,3), 
#'				 Delta = c(0.4,0.5714286,0.666667), 
#'               Pi = c(0.3333,0.333333,0.33333),B = 4.5)
#' GetCovariance(mx = matrix(c(60,0,3,2,50,1,3,2,79),3,3), 
#'				Delta = c( 0.8945724, 0.9522836, 0.8962094), 
#'               Pi = c( 0.2703707, 0.1939561, 0.5356732), B = 17.94867)


GetCovariance <- function(mx,Delta,Pi,B){
  #Calculate auxiliar params
  #if(k == 2){
  #  mx = mx[-3,-3]
  #}
  k = dim(mx)[1]
  R.marg   	  = margin.table(mx,1)
  diag.matrix = diag(mx)
  if (is.null(Pi)){
    Pi		= rep(0,k)
  }
  if (is.null(Delta)){
    Delta		= rep(1,k)
  }
  if (is.null(B)){
    B		= 1
  }
  v = (1-Delta)/(1-Pi)
  E = Pi/(B - R.marg*v)
  Sum.E = sum(E)
  
  #Calculate covariances
  #Cov(Delta,Delta)
  Cov_Delta = -((v*E)%*%t(v*E))/Sum.E
  diag(Cov_Delta) = diag(Cov_Delta) + v*(diag.matrix/R.marg^2 + v*E)
  #Cov(Delta,Pi)
  Cov_mix = ((v*E)%*%t(E))/Sum.E
  diag(Cov_mix) = diag(Cov_mix) - v*E
  #Cov(Pi,Pi)
  Cov_Pi = -(E%*%t(E))/Sum.E
  diag(Cov_Pi) = diag(Cov_Pi) + E
  
  res = list("Cov_Delta" = Cov_Delta, "Cov_mix" = Cov_mix, "Cov_Pi" = Cov_Pi)
  return(res)
}
