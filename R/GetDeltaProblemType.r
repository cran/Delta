#' Get Delta problem type function
#'
#' This function apply Test to identify where are the solution located. We have mainly 3 situations for k > 2 and 2 for k = 2. For k > 2 we have:
#' DN2 = No estimators in the boundary. 
#' DN1 = Some estimator in the boundary and global agreement imaginary. 
#' DN0 = Any other case 
#' For k = 2 we have:
#' DA0 = Some estimator in the boundary. 
#' DA1 = Any other case 

#' @param Mx Matrix. Matrix reduced.
#' @keywords Mx
#' @export
#' @examples
#' GetDeltaProblemType(matrix(c(1,2,0,3,4,0,0,0,1),3,3))
#' GetDeltaProblemType(matrix(c(1,0,0,0,2,0,0,0,3),3,3))

GetDeltaProblemType <- function(Mx){
  #Mx matrix without insignificant rows and columns
  k  = dim(Mx)[1]
  Xr     = margin.table(Mx,1)
  Xc     = margin.table(Mx,2)
  Xt     = sum(Xr)
  diag.Mx  = diag(Mx)
  sum.diag = sum(diag.Mx)

  #r (c) indicate when diagonal is equal to marginal by rows (columns)
  r = sum(Xr == diag.Mx)
  c = sum(Xc == diag.Mx)
  
  #B = 0 condition
    #if there is no disagreement
  B_0 = (sum.diag == Xt)
  
  #B = inf condition 
  # B inf condition is trickier. It need some B parameter calculations
    #Aux calculations
      B0	   = 0
      B0_i     = 0

      #Calculate max position and value
      calc = (sqrt(Xc - diag.Mx) + sqrt(Xr - diag.Mx))^2
      B0_i = which.max(round(calc,10))
      B0   = calc[B0_i]

      radical = function(B){
        radicals = sqrt(pmax((B + (Xc - Xr))^2 - 4 * B * (Xc - diag.Mx),0))
        return(radicals)
      }
      y = function(B){
        yB = (k - 2) * B + sum(-1 * radical(B))
        return(yB)
      }

      yB0=y(B0)
    #if all disagreement is only in one category
    B_inf = ((max((Xt - sum.diag) == (Xr + Xc - 2 * diag.Mx))==1) && (yB0 < 0))
  

  if (k > 2){
    if (B_0 == 0 && B_inf == 0 && r == 0 && c == 0) {
      dtp = "DN2"
      return(dtp)
    }
    else if (B_inf == 1) {
      dtp = "DN1"
      return(dtp)
    }
    else {
      dtp = "DN0"
      return(dtp)
    }
  }
  else if (k == 2){
    if ((sum.diag == Xt) || (any(Xr == 0) | any(Xc == 0))) {
      dtp = "DA0"
      return(dtp)
    } 
    else {
      dtp = "DA1"
      return(dtp)
    }
  }
}
#GetDeltaProblemType(matrix(c(2,0,3,5),2,2))