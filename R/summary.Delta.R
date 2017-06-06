#' @return \code{NULL}
#'
#' @rdname Delta
#' @param object List produced by Delta
#' @export
#' @method summary Delta 

summary.Delta <- function(object,...) {
  temp = object$Summary
  temp$Summary <- as.character(temp$Summary)
  if(!is.null(object$Summary_AN)){
     temp = rbind(temp, 
            paste(as.character(unlist(object$Summary_AN)[2]),"#") ,
            paste(as.character(unlist(object$Summary_AE)[2]),"##"))
    cat('\n',"SUMMARY: Goodness of Fit + Kappa + Delta (Normal)",'\n',
             "+ Delta (Asintotic Normal) + Delta (Asintotic Extra)",'\n')

              print(temp, row.names = FALSE)
	# Usar strsplit("test +- lo que sea = 3", "=")[[1]][2] para quedarnos 2a palabra
	#Separada por = y cambiar delta +- SE y añadir algo de (AN) (AE)
    cat ("#:  Asintotic normal Delta","\n")
    cat ("##: Asintotic extra Delta","\n")
  }
  else {
    cat('\n',"SUMMARY: Goodness of Fit + Kappa + Delta",'\n')
      print(temp, row.names = FALSE)
  }
    #Note under some cases;
	tp = object$tp
    if (tp == "2.0" | tp == "3.0") {
      cat("* A total of rows or column is equal to zero: Results obtained adding 0.5 to all cells. ",'\n')
    }
    else if (tp == "2.1" | tp == "3.1") {
      cat("*(Since R(i)=C(i)=x(i,i) for all i, S.E.(kappa) has been obtained adding 0.5 to the original data)",'\n')
    }
}