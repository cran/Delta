#' Calculate Asintotic Delta related parameters function
#'
#' This function perform all needed calculations to get all Delta related parameters, for a 2x2 matrix. All calculations are asintotics.
#' 
#' @param mx Matrix. Agreement contingency table to perform calculations
#' @param fixedrows Boolean. Indicate if sample rows are fixed beforehand. 
#' @keywords Delta Asintotic mx fixedrows
#' @export
#' @examples
#' GetAsinDeltaParams(matrix(c(60,10,10,20),2,2),TRUE)

GetAsinDeltaParams <- function(mx,fixedrows = TRUE){
   #Calculate auxiliar params
  R.marg   	  = margin.table(mx,1)
  C.marg   	  = margin.table(mx,2)
  n			  = sum(R.marg)
  diag.matrix = diag(mx)
  
  Delta = (diag.matrix - sqrt(mx[3] * mx[2]))/R.marg
  Pi 	= c(sqrt(mx[2])/(sqrt(mx[3]) + sqrt(mx[2])),
			sqrt(mx[3])/(sqrt(mx[3]) + sqrt(mx[2])))
			
  diag.Cov 	  = 1/R.marg^2*(diag.matrix * (1-Delta) + 1/4*(mx[3] + mx[2] - n * mx[3] * mx[2]/(R.marg[1] * R.marg[2])))
    #Estimators calculations
  F = Delta
  P = R.marg*Delta/C.marg
  A = R.marg*Delta/n
  S = 2*R.marg*Delta/(R.marg + C.marg)
  Delta.total = sum(R.marg * Delta)/n
  
  #SE
  F.cov = diag.Cov
  #Sampling type 1
  if (fixedrows == FALSE){
    P.cov = 1/n^2 * (diag.matrix * (1-P) + 1/4*(mx[3] + mx[2]))
	A.cov = 1/n^2 * (diag.matrix + 1/4*(mx[3] + mx[2]) - n * A)
	S.cov = (n * (1 - Delta.total)/(R.marg + C.marg)^2)*(2 - (n * (1 - Delta.total) * (mx[3] + mx[2]))/(R.marg + C.marg)^2)
	Delta.total.cov = 1/n * (1 - Delta.total) * (1 + Delta.total)
  }#Sampling type 2
  else if (fixedrows == TRUE){
    P.cov = NULL
    A.cov = (R.marg/n)^2 * diag.Cov
	S.cov = NULL
	Delta.total.cov = (1 - Delta.total)/n * sum(diag.matrix/R.marg) 
  }
  
  res = list("Delta" = Delta, "Pi" = Pi,
	  "Delta.total" = Delta.total, "F" = F, "P" = P, "A" = A, "S" = S,
	  "Delta.total.cov" = Delta.total.cov, "F.cov" = F.cov, "P.cov" = P.cov, "A.cov" = A.cov, "S.cov" = S.cov)
  class(res) <- "GetAsinDeltaParams"	 
  return(res)
}

#' @return \code{NULL}
#'
#' @rdname GetAsinDeltaParams
#' @param x List produced by GetAsinDeltaParams
#' @param ... Other print options
#' @export
#' @method print GetAsinDeltaParams 
print.GetAsinDeltaParams <- function(x,...){
#Delta 
  cat('','Delta for each category','\n')
  print(x$Delta)
#Pi 
  cat('','Pi for each category','\n')
  print(x$Pi)
#Overall Delta 
   cat('\n','Overall Agreement, Delta ','\n')
   Delta = paste(x$Delta.total," \u00b1 ",x$Delta.total.cov)
   Encoding(Delta) = "UTF-8"
   print(Delta)
 # F: conformity
   cat('\n','Conformity, F ','\n')
   Conformity = paste(x$F," \u00b1 ",x$F.cov)
   Encoding(Conformity) = "UTF-8"
   print(Conformity)
 # P: Predictivity
   cat('\n','Predictivity, P ','\n')
   if(!is.null(x$P.cov)){
	Predictivity = paste(x$P," \u00b1 ",x$P.cov)
   }
   else{
	Predictivity = paste(x$P)
   }
   Encoding(Predictivity) = "UTF-8"
   print(Predictivity)
 # A: Agreement
   cat('\n','Agreement, A ','\n')
   Agreement = paste(x$A," \u00b1 ",x$A.cov)
   Encoding(Agreement) = "UTF-8"
   print(Agreement)
 # S: sensitivity
   cat('\n','Sensitivity, S ','\n')
   if(!is.null(x$S.cov)){
	sensitivity = paste(x$S," \u00b1 ",x$S.cov)
   }
   else{
	sensitivity = paste(x$S)
   }
   Encoding(sensitivity) = "UTF-8"
   print(sensitivity)
}
