#' Calculate Delta related parameters variance function
#'
#' This function perform all needed calculations to get all Delta related parameters variance. For do the exact calculations some variables previously calculated are needed.
#' 
#' @param mx Matrix. Agreement contingency table to perform calculations
#' @param fixedrows Boolean. Indicate if sample rows are fixed beforehand. 
#' @param Delta Vector. Each element indicate the probability of recognize an element i.
#' @param Pi Vector. Each element indicate the probability of classify at random an element in category i.
#' @param k Integer. Dimension of the problem.
#' @param Cov Matrix. Covariance matrix of Delta. 
#' @param E Double. Value calculated for Cov matrix derivation. 
#' @keywords Delta  mx fixedrows Pi B k Cov
#' @export
#' @examples
#' GetDeltaParamsVar(mx = matrix(c(60,0,3,2,50,1,3,2,79),3,3), 
#'		fixedrows = FALSE,Delta = c( 0.8945724, 0.9522836, 0.8962094), 
#'      Pi = c( 0.2703707, 0.1939561, 0.5356732), k = 3, 
#'      Cov = matrix(c(0.002736490,  0.000004188, -0.001074704,
#'                 0.000004188, 0.001141059, -0.000181746,
#'                -0.001074704, -0.000181746,  0.004912131),3,3),
#'		E = c(0.03159824,  0.01304313, -0.88650011))

GetDeltaParamsVar <- function(mx,fixedrows = FALSE, Delta, Pi, k, Cov, E){

  #Calculate auxiliar params
  if(k == 2){
    mx = mx[-3,-3]
  }
  R.marg   	  = margin.table(mx,1)
  C.marg   	  = margin.table(mx,2)
  n			  = sum(R.marg)
  diag.matrix = diag(mx)
  diag.Cov 	  = diag(Cov)
  if (is.null(Pi)){
    Pi		= rep(0,k)
  }
  if (is.null(Delta)){
    Delta		= rep(1,k)
  }
  Sum.E = sum(E)
  Delta.total = sum(R.marg * Delta)/n
  
  
  #Variance calculations
  F.cov = sqrt(diag.Cov)
  #Sampling type 1
  if (fixedrows == FALSE){
    P.cov = sqrt((R.marg/C.marg)^2 * (diag.Cov + (C.marg - R.marg)/(R.marg*C.marg)*Delta ^2))
	A.cov = sqrt((R.marg/n)^2 * (diag.Cov + (n - R.marg)/(R.marg*n)*Delta ^2))
	S.cov = sqrt((2 * R.marg/(R.marg + C.marg))^2*(diag.Cov + Delta^2/(R.marg + C.marg)*(C.marg/R.marg - 2 + 2 * diag.matrix/(R.marg + C.marg))))
	if (k != 2){
	  Delta.total.cov = sqrt(1/n^2 * (n - 1/Sum.E - n * Delta.total^2))
	}
	else if (k == 2){
	  Delta.total.cov = sqrt(1/n^2 * (sum((R.marg %*%t(R.marg)) * Cov) + sum(R.marg * Delta^2) - n * Delta.total^2))
	}
  }#Sampling type 2
  else if (fixedrows == TRUE){
    P.cov = NULL
    A.cov = sqrt((R.marg/n)^2 * diag.Cov)
	S.cov = NULL
	if (k != 2){
	  Delta.total.cov = sqrt(1/n^2 * (n - 1/Sum.E - sum(R.marg * Delta^2)))
	}
	else if (k == 2){
	  Delta.total.cov = sqrt(1/n^2 * sum((R.marg %*%t(R.marg)) * Cov) )
	}
  }
  res = list("Delta.total.cov" = Delta.total.cov, "F.cov" = F.cov, "P.cov" = P.cov, "A.cov" = A.cov, "S.cov" = S.cov)
  return(res)
}

