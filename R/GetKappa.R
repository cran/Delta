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
  dim.matrix = dim(mx)
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
  aux_rows =  margin.table(Copy_matrix, 1)
  
  #Calculate SE
  a = sum(diag(mx)/n*(1 - aux_calc * (1-kappa))^2)
  b = (1-kappa)^2*sum(aux_rows * aux_calc^2)
  c = (kappa - Expected * (1-kappa))^2
  SE = 1/(1-Expected) * sqrt((a+b-c)/n)
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
