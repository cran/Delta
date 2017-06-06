#' Check Sampling type function
#'
#' This function checks that fixedrows and gstandard parameters are correct. Also return a value indicating if it is or not valid and what kind of output should be shown at the end of the execution.
#' @param fixedrows Boolean. Indicate if sample rows are fixed beforehand. Default is TRUE.
#' @param gstandard Text. Indicate if there are a Gold Standard by Rows or columns. Only first letter matter without Case sensitivity. Options are: "N" for None, "R" for in Rows and "C" for in Columns. Default is "N".
#' @keywords check fixedrows gstandard 
#' @export
#' @examples
#' CheckSampling(TRUE,"rows")
#' CheckSampling(TRUE,"Columns")

CheckSampling <- function(fixedrows,gstandard){
  #fixedrows boolean indicate if rows are fixed
  #gstandard text, only first character in lowcase matter, n for "No", r for "Rows", c for "Columns"
  
  # Check that is a boolean
  if (is.null(fixedrows) | (fixedrows != FALSE & fixedrows != TRUE)){
	stop("fixedrows must be a boolean")
  }
  #Check that gstandard is a string
  if (is.character(gstandard) == FALSE){
	stop("gstandard must be a string")
  }
  #Check that is one of the three valid values
  first_char = tolower(substr(gstandard,1,1))
  if (first_char != 'n' & first_char != 'r' & first_char != 'c'){
	stop("gstandard must be 'N' for 'None', 'R' for 'Rows', or 'C' for 'Columns'.")
  }
  #Introduce values on delta.samplingtype
  if (fixedrows == TRUE){
	if (first_char== 'r'){
	  delta.samplingtype = 1
	}
	else if (first_char== 'c'){
	  delta.samplingtype = 2
	}
      else if (first_char== 'n'){
	  delta.samplingtype = 3
      }
  }
  if (fixedrows == FALSE){
	if (first_char== 'r'){
	  delta.samplingtype = 4
	}
	else if (first_char== 'c'){
	  delta.samplingtype = -1
	}
      else if (first_char== 'n'){
	  delta.samplingtype = 5
      }  
  }
  return(delta.samplingtype)
}
