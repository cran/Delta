#' Get problem type function
#'
#' This function apply TestType function multiple times, to be able of determine the type of problems between following
#' 1.0 = Matrix size 0
#' 1.1 = Matrix size 1
#' 2.0 = Matrix size 2 where a marginal row or column equal 0
#' 2.1 = Matrix size 2 and diagonal
#' 2.2 = Matrix size 2 not diagonal
#' 3.0 = Matrix size greater than 2 with a marginal row or column equal 0
#' 3.1 = Matrix size greater than 2 and diagonal
#' 3.2 = Matrix size greater than 2 with k-1 row or columns with marginal equal to a_{ii} or k-2 where marginals of columns and rows equals to a_{ii}
#' 3.3 = Matrix size greater than 2 with h where Xt - sum(diag(M1)) = Xr[h] + Xc[h] - 2 * M1[h,h]
#' 3.4 = Matrix size greater than 2 with i where a_{ii} = 0 or equl to marginals by columns or row 
#' 3.5 = Matrix size greater than 2 without a_{ii} = 0 or without marginals by columns or row equals to zero

#' @param M1 Matrix. Matrix reduced.
#' @param k Integer. Size of the problem.
#' @keywords M1 k
#' @export
#' @examples
#' GetProblemType(matrix(c(1,2,0,3,4,0,0,0,1),3,3), 3)
#' GetProblemType(matrix(c(1,0,0,0,2,0,0,0,3),3,3), 3)

GetProblemType <- function(M1,k){
  #M1 matrix without insignificant rows and columns
  #k  size of the matrix
  if (k ==0 | k == 1){
    # M1 is numeric or NULL
    Xr     = M1
    Xc     = M1
    Xt     = M1
  }
  else {
    Xr     = margin.table(M1,1)
    Xc     = margin.table(M1,2)
    Xt     = sum(Xr)
  }

  if (k == 0){
    if (TestType("1.0", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "1.0"
	  return(tp)
	}
	else {
	  stop("Unexpected matrix")
	}
  }
  else if (k == 1){
    if (TestType("1.1", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "1.1"
	  return(tp)
	}
	else {
	  stop("Unexpected matrix")
	}
  }
  else if (k == 2){
    if (TestType("2.0", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "2.0"
	  return(tp)
	}
	else if (TestType("2.1", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "2.1"
	  return(tp)
	}
	else if (TestType("2.2", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "2.2"
	  return(tp)
	}
	else {
	  stop("Unexpected matrix")
	}
  }
  else if (k >= 3){
    if (TestType("3.0", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "3.0"
	  return(tp)
	}
	else if (TestType("3.1", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "3.1"
	  return(tp)
	}
	else if (TestType("3.2", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "3.2"
	  return(tp)
	}
	else if (TestType("3.3", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "3.3"
	  return(tp)
	}
	else if (TestType("3.4", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "3.4"
	  return(tp)
	}
	else if (TestType("3.5", M1, k, Xr, Xc, Xt) == TRUE) {
	  tp = "3.5"
	  return(tp)
	}
	else {
	  stop("Unexpected matrix")
	}
  }
}
#GetProblemType(matrix(c(2,0,3,5),2,2),2)