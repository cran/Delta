#' Check Input Matrix function
#'
#' This function checks that matrix introduced is as expected. Should be a matrix, squared, with a dimension greater or equal to two, without negative entries and at least an entry  different of 0.
#' @param datatable Matrix. Expected to be square matrix with at least 2 rows (columns), non negative values and at least an element different of zero.
#' @keywords check datatable 
#' @export
#' @examples
#' CheckInputData(matrix(c(1,2,3,4),2,2))
CheckInputData <- function(datatable){
  
  #Check object is a matrix
  if (is.matrix(datatable) == FALSE){
	stop("datatable argument must be a matrix")
  }  
  #Get it dimension and check if is square and ge than 2
  dim.matrix = dim(datatable)
  if (dim.matrix[1] != dim.matrix[2]){
	stop("datatable argument must be a square matrix")
  }
  if (dim.matrix[1] == 1){
	stop("datatable argument must have at least two rows")
  }
  #Check that all entries are positive
  if (all(datatable>=0) == FALSE){
	stop("At least datatable entry is negative")
  }
  if (all(datatable==0) == TRUE){
	stop("All datatable entries are equal to 0")
  }
  #return(invisible(NULL))
}

