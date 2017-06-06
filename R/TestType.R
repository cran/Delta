#' Test problem type function
#'
#' This function calculate for the parameters given, the type of problem. Problems could be
#' 1.0 = Matrix size 0
#' 1.1 = Matrix size 1
#' 2.0 = Matrix size 2 where a marginal row or column equal 0
#' 2.1 = Matrix size 2 and diagonal
#' 2.2 = Matrix size 2 not diagonal
#' 3.0 = Matrix size greater than 2 with a marginal row or column equal 0
#' 3.1 = Matrix size greater than 2 and diagonal
#' 3.2 = Matrix size greater than 2 with k-1 row or columns with marginal equal to a_{ii} or k-2 where marginals of columns and rows equals to a_{ii}
#' 3.3 = Matrix size greater than 2 with h where Xt - sum(diag(M1)) = Xr[h] + Xc[h] - 2 * M1[h,h]
#' 3.4 = Matrix size greater than 2 with i where a_{ii} = 0 or with marginals by columns or row equals to zero
#' 3.5 = Matrix size greater than 2 without a_{ii} = 0 or without marginals by columns or row equals to zero

#' @param tp String. Type of problem to check of the previous list.
#' @param M1 Matrix. Matrix reduced.
#' @param k Integer. Size of the problem.
#' @param Xr Vector. Marginals by row.
#' @param Xc Vector. Marginals by column.
#' @param Xt Double. Sum of all elements in the matrix.
#' @keywords tp M1 k Xr Xc Xt
#' @export
#' @examples
#' TestType("1.0", matrix(c(1,2,0,3,4,0,0,0,1),3,3), 3, c(4,5,1), c(3,7,1), 11)

TestType <- function(tp, M1, k, Xr, Xc, Xt){
#tipo a contrastar (2.0, 2.1,..., 3.5), matriz de datos y sus marginales por filas, columnas y total 
#(evita tener que hacer el calculo en cada comparacion)
  #tp string containing type of case to be checked
  #M1, normaly M1
  #k  size of M1
  #Xr row marginals
  #Xc column marginals
  #Xt total of the M1 matrix

#r_i or c_i eq 0 could be translated on det = 0
  diag.M1  = diag(M1)
  sum.diag = sum(diag.M1)

  r = sum(Xr == diag.M1)
  c = sum(Xc == diag.M1)
  
  res = FALSE
  if (tp == "1.0") {
	if (k == 0){
		res = TRUE
	}
	return(res)
  }
  else if (tp == "1.1") {
	if (k == 1){
		res = TRUE
	}
	return(res)
  }
  else if (tp == "2.0") {
	if (any(Xr == 0) | any(Xc == 0)) {
		res = TRUE
	}
	return(res)
  }
  else if (tp == "2.1") {
	if (sum.diag == Xt) {
		res = TRUE
	}
	return(res)
  }
  else if (tp == "2.2") {
	if (sum.diag != Xt) {
		res = TRUE
	}
	return(res)
  }
  else if (tp == "3.0") {
	if (all(Xr != 0) == FALSE | all(Xc != 0) == FALSE) {
		res = TRUE
	}
	return(res)
  }
  else if (tp == "3.1") {
	if (sum.diag == Xt & k == r) {
		res = TRUE
	}
	return(res)
  }
  else if (tp == "3.2") {
	if (k == (c + 1) | k == (r + 1) | (k == (r + 2) & k == (c + 2))) {
		res = TRUE
	}
	return(res)
  }
  else if (tp == "3.3") {
	if (sum((Xt - sum.diag) == (Xr + Xc - 2 * diag.M1))>=1) {
		res = TRUE
	}
	return(res)
  }
  else if (tp == "3.4") {
	if (all(Xr != 0) == FALSE | all(Xc != 0) == FALSE | all(diag.M1 != 0) == FALSE) {
		res = TRUE
	}
	return(res)
  }
  else if (tp == "3.5") {
	if (all(Xr != 0) == TRUE | all(Xc != 0) == TRUE | all(diag.M1 != 0) == TRUE) {
		res = TRUE
	}
	return(res)
  }
  else {
	stop("Unexpected tp value")
  }
}
#TestType("1.0", M1, k, Xr, Xc, Xt)
