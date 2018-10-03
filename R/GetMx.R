#' Get matrix of the problem (Mx) function
#'
#' This function produce 3 new auxiliar matrix. Those matrix are always defined, but depending of the problem they will be used or not.
#' All the auxiliar matrices are created to be able to solve the problem and avoid issues with solutions in the boundary or not completly defined.
#' 
#' The matrix are defined as follows
#'  - M2: Extended M1 with $c_33$ of 1 and increased by 0.5
#'  - M3: M1 increased by 0.5
#'  - M4: M1 increased by 1
#' 
#' In case of M1 is not 2x2, M2 and M3 are the same matrix.
#' 
#' @param M1 Matrix. Initial matrix without missing categories.
#' @keywords M1 Mx
#' @export
#' @examples
#' GetMx(matrix(c(1,2,0,3,4,0,0,0,1),3,3))


GetMx <- function(M1){
#List of matrix M1,...,M4 defined for all problems
  # Get size of M1
  k  = dim(M1)[1]
  
  # In case of M1 2x2 we need to create an extended version of M1 to define M2
  if (k == 2){
    M1_new = rbind(cbind(M1,c(0,0)),c(0,0,1))
  }
  else {
    M1_new = M1
  }
  #Create matrix Mx
  M2 = M1_new + 0.5
  M3 = M1 + 0.5
  M4 = M1 + 1
  

  res= list("M1" = M1, "M2" = M2, "M3" = M3, "M4" = M4)
  return(res)
}
