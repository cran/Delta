#' @return \code{NULL}
#'
#' @rdname Delta
#' @param object List produced by Delta
#' @export
#' @method summary Delta 

summary.Delta <- function(object,...) {
  temp = object$Summary
  dtp  = object$dtp
  dtp2  = object$dtp2
  ktp  = object$ktp
  temp$Summary <- as.character(temp$Summary)
  if(object$k == 2){
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
    if (dtp2 == "DA0" || dtp =="DN0" || dtp =="DN1" || ktp == "K0") {
      cat("* This value has been obtained adding 0.5 to the original data)",'\n')
    }
}