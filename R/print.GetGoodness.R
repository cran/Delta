#' @return \code{NULL}
#'
#' @rdname GetGoodness
#' @param x List produced by GetGoodness
#' @param ... Other print options
#' @export
#' @method print GetGoodness 
print.GetGoodness<- function(x,...){
   cat('X-squared =', x$X.squared, ', df =', x$df,', p-value =',x$p.value,'\n')
   cat('Expected matrix:','\n')
   print(x$E.matrix)
}
