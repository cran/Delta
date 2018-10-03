#' @return \code{NULL}
#'
#' @rdname GetM1
#' @param x List produced by GetM1
#' @param ... Other print options
#' @export
#' @method print GetM1 
print.GetM1<- function(x,...){
  cat("Simplified matrix is:\n")
  print(x$M1)
  cat("The problem has ", x$k, " categories.\n")
  if(!is.null( x$Deleted)){
	 cat("Category(ies) ", x$Deleted, " had been deleted.\n")
  }
}