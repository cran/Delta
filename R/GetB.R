#' Calculate B function
#'
#' This function solve numericaly the non lineal inequation of the Delta system. Also return the s(i) values of the equation.
#' 
#' @param mx Matrix. Modified matrix to have a solution Usually GetMx$M2.
#' @param maxits Whole number. Indicate the maximum number of iterations of the numeric method to calculate B. Expected to be 100 <= maxits <= 5000. Default is 1000.
#' @param tol Double number. Indicate the precision of the numeric method to calculate B. Expected to be 1e-6 <= tol <= 1e-15.Default is 1e-12.
#' @keywords Delta B mx tol maxits
#' @export
#' @examples
#' GetB(mx = matrix(c(1,0,0,0,2,0,0,0,3),3,3),tol = 1e-12, maxits = 1000)
#' GetB(mx = matrix(c(1,2,0,3,4,0,0,0,1),3,3),tol = 1e-12, maxits = 1000)

GetB <-function(mx, tol = 1e-12, maxits = 1000){
  
  #Calculate needed parameters
  dim.matrix = dim(mx)
  R.marg   = margin.table(mx,1)
  C.marg   = margin.table(mx,2)
  n        = sum(R.marg)
  diag.matrix = diag(mx)
  Sum_xii  = sum(diag.matrix)
  
  #Initialize B0 and B0_i
  B0	     = 0
  B0_i       = 0

  #Calculate max position and value
  calc = (sqrt(C.marg - diag.matrix) + sqrt(R.marg - diag.matrix))^2
  B0_i = which.max(round(calc,10))
  B0   = calc[B0_i]

  #Create general function
  term = rep(-1, dim.matrix[1])

  radical = function(B){
    radicals = sqrt(pmax((B + (C.marg - R.marg))^2 - 4 * B * (C.marg - diag.matrix),0))
    return(radicals)
  }
  y = function(B){
    yB = (dim.matrix[1] - 2) * B + sum(term * radical(B))
    return(yB)
  }

  yB0=y(B0)
  #If its negative, should be updated to this function
  if (yB0 < 0) {
    term[B0_i]=1
  }

  #Since B = n(1-Delta)>=B0, B is between B0 and 2n, since the min value of delta is -1, 
  #be confused between levels
  root =  stats::uniroot(y,c(B0,2*n), tol = tol, maxiter = maxits)$root

  res = list("B" = root, "term" = term)
  # "yB0" = yB0, "B0_i" = B0_i not needed
  return(res)
}