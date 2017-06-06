#' Calculate Delta and Pi parameters function
#'
#' This function provide an estimation of Pi and Delta for each category. To do so, it is needed to solve the non-linear 
#' equation of B, given by the function GetB.
#'
#' In some type of problems, such as "2.1" and "3.1", problem has a unique solution for Delta and infinite for Pi. For this reason
#' we calculate the solution for Delta and get Pi as NULL.
#' 
#' @param mx Matrix. Modified matrix to have a solution Usually GetMx$M2.
#' @param tp String. Type of problem.
#' @param maxits Whole number. Indicate the maximum number of iterations of the numeric method to calculate B. Expected to be 100 <= maxits <= 5000. Default is 1000.
#' @param tol Double number. Indicate the precision of the numeric method to calculate B. Expected to be 1e-6 <= tol <= 1e-15.Default is 1e-12.
#' @keywords Delta mx tp tol maxits
#' @export
#' @examples
#' GetDeltaPi(mx = matrix(c(1,0,0,0,2,0,0,0,3),3,3), tp = "3.1", tol = 1e-12, maxits = 1000)
#' GetDeltaPi(mx = matrix(c(1.5,2.5,0.5,3.5,4.5,0.5,0.5,0.5,1.5),3,3), tp = "3.2", 
#'            tol = 1e-12, maxits = 1000)


GetDeltaPi <-function(mx,tp,tol = 1e-12,maxits = 1000){
  
  if (tp != "3.1" & tp != "2.1"){
    #Checks are done on GetB, so no need to do it again
    res 	= GetB(mx,tol,maxits)

    #Keep all the needed info from the result
    B 		      = res$B
    term	      = res$term
  
    #Derivate other parameters
    R.marg   	  = margin.table(mx,1)
    C.marg   	  = margin.table(mx,2)
    diag.matrix   = diag(mx)
    n        	  = sum(R.marg)
  
    #Calculate parameters
    Pi 	    = (B + (C.marg - R.marg) + 
              term * sqrt(round((B + (C.marg - R.marg))^2 - 4 * B * (C.marg - diag.matrix),10)))/(2*B)
    Delta   = (diag.matrix - R.marg * Pi)/(R.marg * (1 - Pi))

    #if (tp == "2.0" | tp == "2.2"){
    #  Pi = Pi[-3]
    #  Delta = Delta[-3]
    #}
  } #WARNING: This case is not in specs, susceptible to change
  else if (tp == "2.1"){
    Pi 	    = rep(NULL,3)
    Delta 	= rep(1, 3)
    term 	= rep(-1, 3)
    B 	    = 0
  }
  else {
    dim.matrix  = dim(mx)[1]
    Pi 	        = rep(NULL, dim.matrix)
    Delta 	    = rep(1, dim.matrix)
    term 	    = rep(-1, dim.matrix)
    B 	    	= 0
  }
  
  res = list("B" = B, "Pi" = Pi, "Delta" = Delta)
  return(res)

}
