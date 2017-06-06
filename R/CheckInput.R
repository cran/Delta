#' Main Check Input function
#'
#' This function perform multiple tasks. First of all, check the parameters specified by the user.  Also asign default values to some parameters not defined by the user. Finally it generates error  messages and halt the execution in case it is needed.
#' @param datatable Matrix. Expected to be square matrix with at least 2 rows (columns), non negative values and at least an element different of zero.
#' @param fixedrows Boolean. Indicate if sample rows are fixed beforehand. Default is TRUE.
#' @param gstandard Text. Indicate if there are a Gold Standard by Rows or columns. Only first letter matter without Case sensitivity. Options are: "N" for None, "R" for in Rows and "C" for in Columns. Default is "N".
#' @param maxits Whole number. Indicate the maximum number of iterations of the numeric method to calculate B. Expected to be 100 <= maxits <= 5000. Default is 1000.
#' @param tol Double number. Indicate the precision of the numeric method to calculate B. Expected to be 1e-6 <= tol <= 1e-15.Default is 1e-12.
#' @param dplaces Whole number. Decimal placed to be shown in the result. Expected to be 1 <= dplaces <0 6. Default 4.
#' @param showall Boolean. Indicate if all output should be shown. If TRUE also shown hidden results. If FALSE shown only main output. By default is FALSE.
#' @keywords check datatable fixedrows gstandard maxits tol dplaces
#' @export
#' @examples
#' CheckInput(matrix(c(1,2,3,4),2,2),fixedrows=FALSE,gstandard="No",maxits=100,tol=1e-12,dplaces=4)

CheckInput <-function(datatable,fixedrows=FALSE,gstandard="No",maxits=1000,tol=1e-12,dplaces=4, showall = FALSE){
  #Check matrix
  CheckInputData(datatable)
  #Checks of fixedrows and gstandard and compatibility between them
  delta.samplingtype = CheckSampling(fixedrows,gstandard)
  if (delta.samplingtype == -1) {
	stop("This combination of fixedrows and gstandard do not have sense")
  }
  #Checks all other numeric parameters
  if (is.numeric(maxits) == FALSE){
	stop("maxits must be a whole number")
  }
  if (round(maxits,0) != maxits){
	stop("maxits must be a whole number")
  }
  if (maxits<100 | maxits>5000){
  	stop("maxits must be greater than 100 and lower than 5000")
  }
  if (is.numeric(tol) == FALSE){
	stop("tol must be numeric")
  }
  if (tol>1e-6 | tol<1e-15){
  	stop("tol must be greater than 1e-15 and lower than 1e-6")
  }
  if (is.numeric(dplaces) == FALSE){
	stop("dplaces must be an integer number")
  }
  if (round(dplaces,0) != dplaces){
	stop("dplaces must be a whole number")
  }
  if (dplaces<1 | dplaces>6){
  	stop("dplaces must be greater than 1 and lower than 6")
  }
  if (is.null(showall) | (showall != FALSE & showall != TRUE)){
	stop("showall must be a boolean")
  }
  return(delta.samplingtype)
}

