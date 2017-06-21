#' Calculate Cohen's Kappa coefficient function
#'
#' This function perform Cohen's Kappa coefficient calculations. The function provide the Kappa coefficient and SE.
#' 
#' @param mx Matrix. Agreement contingency table to perform calculations
#' @keywords Kappa Cohen mx
#' @export
#' @examples
#' GetKappa(matrix(c(50,10,10,20),2,2))

GetKappa <-function(mx){  
  #Calculate Observed
  Observed = sum(diag(mx))  

  #Calculate marginals for expected
  R.marg   = margin.table(mx,1)
  C.marg   = margin.table(mx,2)
  dim.matrix = dim(mx)[1]
  n        = sum(R.marg)

  Expected = sum(R.marg * C.marg)/n

  #Calculate kappa
  if (Expected == n){
    kappa	= 1
  }
  else{
    kappa    = (Observed - Expected)/(n - Expected)
  }
  #Add for aux calculations some matrixs
  aux_calc	= R.marg/n + C.marg/n
  Observed	= Observed/n
  Expected	= Expected/n
  Copy_matrix = mx/n
  diag(Copy_matrix) = 0
  
  Rmat = matrix(rep(R.marg/n,dim.matrix ),dim.matrix ,dim.matrix )
  Cmat = matrix(rep(C.marg/n,dim.matrix ),dim.matrix ,dim.matrix,byrow=TRUE )
  
  #Calculate SE
  a = sum(diag(mx)/n * (1 - aux_calc * (1-kappa))^2)
  b = (1-kappa)^2 * sum(Copy_matrix * (Rmat + Cmat)^2)
  c = (kappa - Expected * (1-kappa))^2
  SE = 1/(1-Expected) * sqrt((a+b-c)/n)
  #SE_alt = 1/(1-Expected)/sqrt(n) * sqrt(Expected + Expected^2 - sum(R.marg/n*C.marg/n*aux_calc))
  res = list("kappa" = kappa, "SE" = SE)
  class(res) <- "GetKappa"
  return(res)
}

#' @return \code{NULL}
#'
#' @rdname GetKappa
#' @param x List produced by GetKappa
#' @param ... Other print options
#' @export
#' @method print GetKappa 
print.GetKappa<- function(x,...){
   cat("Kappa \u00b1 S.E. = ",x$kappa, " \u00b1 ",x$SE,'\n')
}
