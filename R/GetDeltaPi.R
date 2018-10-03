#' Calculate Delta and Pi parameters function
#'
#' This function provide an estimation of Pi and Delta for each category. To do so, it is needed to solve the non-linear 
#' equation of B, given by the function GetB.
#'
#' In some type of problems, the solution is on the borderline, and in those situation we may have not solutions at all, only one or 
#' infinity of them. 
#' 
#' @param mx Matrix. Modified matrix to have a solution. Usually GetMx$M1 for k>2 and GetMx$M2 in case of k = 2.
#' @param dtp String. Type of delta problem.
#' @param maxits Whole number. Indicate the maximum number of iterations of the numeric method to calculate B. Expected to be 100 <= maxits <= 5000. Default is 1000.
#' @param tol Double number. Indicate the precision of the numeric method to calculate B. Expected to be 1e-6 <= tol <= 1e-15.Default is 1e-12.
#' @param original.mx Boolean. Indicate if the dtp parameter correspond to the current mx parameter. By default TRUE.
#' @keywords Delta mx dtp tol maxits original.mx
#' @export
#' @examples
#' GetDeltaPi(mx = matrix(c(1,0,0,0,2,0,0,0,3),3,3), dtp = "DN0", tol = 1e-12, maxits = 1000)
#' GetDeltaPi(mx = matrix(c(1.5,2.5,0.5,3.5,4.5,0.5,0.5,0.5,1.5),3,3), dtp = "DN2", 
#'            tol = 1e-12, maxits = 1000, original.mx = FALSE)


GetDeltaPi <-function(mx,dtp,tol = 1e-12,maxits = 1000, original.mx = TRUE){
  
   #Derivate other parameters
   dim.matrix  = dim(mx)[1]
   diag.matrix  = diag(mx)
   sum.diag = sum(diag.matrix)
   R.marg     = margin.table(mx,1)
   C.marg     = margin.table(mx,2)
   n     = sum(R.marg)
   
  if (sum.diag != n){
    #Checks are done on GetB, so no need to do it again
    res 	= GetB(mx,tol,maxits)
    if (dtp == "DN1" && original.mx == TRUE) {
      res$B = Inf
    }

    #Keep all the needed info from the result
    B 		      = res$B
    term	      = res$term
  
    #Calculate parameters
    
    if (B == Inf ){
      Pi = ifelse( (1:length(dim.matrix)) == 1, 1, 0 )
      Delta   = ifelse( (1:length(dim.matrix)) == 1, -Inf, diag.matrix/n )
    }
    else {
      Pi 	    = (B + (C.marg - R.marg) + 
                  term * sqrt(round((B + (C.marg - R.marg))^2 - 4 * B * (C.marg - diag.matrix),10)))/(2*B)
      Delta   = (diag.matrix - R.marg * Pi)/(R.marg * (1 - Pi))    
    }

    #if (tp == "2.0" | tp == "2.2"){
    #  Pi = Pi[-3]
    #  Delta = Delta[-3]
    #}
  } 
  else {
    Pi 	        = rep(NULL, dim.matrix)
    Delta 	    = rep( 1,   dim.matrix)
    term 	    = rep(-1,   dim.matrix)
    B 	    	= 0
  }
  
  res = list("B" = B, "Pi" = Pi, "Delta" = Delta)
  return(res)

}
