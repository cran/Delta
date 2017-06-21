#' Calculate Delta related parameters function
#'
#' This function perform all needed calculations to get all Delta related parameters. For do the exact calculations some variables previously calculated are needed.
#' 
#' @param mx Matrix. Agreement contingency table to perform calculations
#' @param Delta Vector. Each element indicate the probability of recognize an element i.
#' @param Pi Vector. Each element indicate the probability of classify at random an element in category i.
#' @param k Integer. Dimension of the problem.
#' @keywords Delta  mx fixedrows Pi B k Cov
#' @export
#' @examples
#' GetDeltaParams(mx = matrix(c(60,0,3,2,50,1,3,2,79),3,3), 
#'				Delta = c( 0.8945724, 0.9522836, 0.8962094), 
#'               Pi = c( 0.2703707, 0.1939561, 0.5356732), k = 3)

GetDeltaParams <- function(mx, Delta, Pi, k){
  #Calculate auxiliar params
  if(k == 2){
    mx = mx[-3,-3]
  }
  R.marg   	  = margin.table(mx,1)
  C.marg   	  = margin.table(mx,2)
  n			  = sum(R.marg)
  diag.matrix = diag(mx)
  if (is.null(Pi)){
    Pi		= rep(0,k)
  }
  if (is.null(Delta)){
    Delta		= rep(1,k)
  }
  
  #Estimators calculations
  F = Delta
  P = R.marg*Delta/C.marg
  A = R.marg*Delta/n
  S = 2*R.marg*Delta/(R.marg + C.marg)
  Delta.total = sum(R.marg * Delta)/n
  
  
  res = list("Delta.total" = Delta.total, "F" = F, "P" = P, "A" = A, "S" = S)
  #class(res) <- "GetDeltaParams"
  return(res)
}

