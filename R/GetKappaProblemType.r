#' Get Kappa problem type function
#'
#' This function apply Test to identify where kappa solutions are placed
#' K0 = Full agreement (diagonal matrix)
#' K1 = Any other case 

#' @param Mx Matrix. Matrix reduced.
#' @keywords Mx
#' @export
#' @examples
#' GetKappaProblemType(matrix(c(1,2,0,3,4,0,0,0,1),3,3))
#' GetKappaProblemType(matrix(c(1,0,0,0,2,0,0,0,3),3,3))

GetKappaProblemType <- function(Mx){
  #Mx matrix without insignificant rows and columns

  Xr     = margin.table(Mx,1)
  Xc     = margin.table(Mx,2)
  Xt     = sum(Xr)
  diag.Mx  = diag(Mx)
  sum.diag = sum(diag.Mx)

  if (sum.diag == Xt) {
    ktp = "K0"
    return(ktp)
  }
  else if (sum.diag < Xt) {
    ktp = "K1"
    return(ktp)
  }
}
#GetKappaProblemType(matrix(c(2,0,3,5),2,2))