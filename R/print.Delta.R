
#' @return \code{NULL}
#'
#' @rdname Delta
#' @param x List produced by Delta
#' @param ... Other print options
#' @export
#' @method print Delta 
print.Delta<- function(x,...){
   # Information about deleted rows;
   dtp = x$dtp
   dtp2 = x$dtp2
   ktp = x$ktp
   samplingtype = x$samplingtype
   
    #1th output;
   cat("               RESULTS","\n")
   if (x$k != x$k0){
	if (x$k == x$k0-1){
	   cat("Category i = ",x$Del_rows, " had been deleted from the problem because r_i = c_i = 0",'\n')
	}
	else{
	   cat("Categories i = ",x$Del_rows, " had been deleted from the problem because r_i = c_i = 0",'\n')
	}
   }
    #2th output;
   #Information about raw matrix and M1
   cat('\n',"Original data",'\n')
   print(x$M1)
    #If k = 2 show M2;
    if (x$k == 2){
    cat("The problem has infinite solutions. The following solution has been obtained by creating an ",'\n',
        "extra (and fictitious) class where C(3)=R(3)=x(3,3)=1 and by adding 0.5 to all the ",'\n',
        "observations. ",'\n'); 
        print(x$M2)
    }
   #No more results for cases 1.0 and 1.1
   if (x$k >= 2){#note and M2 shown in some cases;
    if (dtp == "DN0" || dtp == "DN1") {  
      if (dtp == "DN0") {  
        cat("Some of the delta model parameters are at the border of the parametric space, so SE can not be ",'\n',
        "estimated. That is why the following table only provides the parameters estimators of the",'\n',
        "model and the measures of agreement.",'\n')
      }
      else if (dtp == "DN1") {
        cat("At least one estimation falls on the boundary of the parametric space (for this reason the values",'\n',
        "of SE have not been obtained) and the global agreement is not representative.",'\n')
      }
    
      if (samplingtype<=3){
        cat("Totals in rows are prefixed",'\n')
      }
      else {
        cat("Totals in rows and columns are at random",'\n')
      }
      if (samplingtype==1 | samplingtype==4){
        cat("Row observer is standard",'\n')
      } 
      else if (samplingtype==2){
        cat("Row observer is in columns",'\n')
      } 
      else {
        cat("There isn't a standard observer",'\n')
      }
      # All model parameters
      temp = paste("Model Parameters (Delta and Pi) and All Concordance Measures ")
      Encoding(temp) = "UTF-8"
      cat('\n',temp,'\n')
      print(x$Paramstable, row.names = FALSE)
      temp = paste("Delta \u00b1 S.E. = ", x$Deltaoverall," \u00b1 ",x$Deltaoverall_SE)
      Encoding(temp) = "UTF-8"
      cat(temp,'\n')
      
      cat("The results that follow are based on the original data increased by 0.5.",'\n')
      print(x$M3)
      # All model parameters
      temp = paste("Model Parameters (Delta and Pi) and All Concordance Measures \u00b1 S.E.")
      Encoding(temp) = "UTF-8"
      cat('\n',temp,'\n')
      print(x$Paramstable2, row.names = FALSE)
      temp = paste("Delta \u00b1 S.E. = ", x$Deltaoverall2," \u00b1 ",x$Deltaoverall_SE2)
      Encoding(temp) = "UTF-8"
      cat(temp,'\n')
    }
    else {
      if (samplingtype<=3){
        cat("Totals in rows are prefixed",'\n')
      }
      else {
        cat("Totals in rows and columns are at random",'\n')
      }
      if (samplingtype==1 | samplingtype==4){
        cat("Row observer is standard",'\n')
      } 
      else if (samplingtype==2){
        cat("Row observer is in columns",'\n')
      } 
      else {
        cat("There isn't a standard observer",'\n')
      }
      # All model parameters
      temp = paste("Model Parameters (Delta and Pi) and All Concordance Measures \u00b1 S.E.")
      Encoding(temp) = "UTF-8"
      cat('\n',temp,'\n')
      print(x$Paramstable, row.names = FALSE)
      temp = paste("Delta \u00b1 S.E. = ", x$Deltaoverall," \u00b1 ",x$Deltaoverall_SE)
      Encoding(temp) = "UTF-8"
      cat(temp,'\n')
    }   

	#Expected quantities
    #3th output;
    cat('\n',"Expected quantities",'\n')
    print(x$E_matrix)

    #Little summary
    #4th output;
    cat('\n',"               SUMMARY: Goodness of Fit + Kappa + Delta",'\n')
    print(x$Summary, row.names = FALSE)
    #Note under some cases;
    if (ktp == "K0" || dtp == "DN0" || dtp == "DN1") {
    #(este valor ha sido calculado para los datos originales incrementados en 0.5)
      cat("* This value has been obtained adding 0.5 to the original data. ",'\n')
    }
	
    #5th output;
	if (x$showall == TRUE){
      #HIDDEN RESULTS: Cov matrix
      cat('\n',"Covariance Matrix = Cov(Delta,Pi)",'\n')
      cat('Cov(Delta,Delta)','\n')
      print(x$Cov_Delta)
      cat('\n','Cov(Delta,Pi)','\n')
      print(x$Cov_mix)
      cat('\n','Cov(Pi,Pi)','\n')
      print(x$Cov_Pi)
    }
		

	
	if (x$k == 2){
	  ### ASINTOTIC NORMAL SOLUTION ###;
      # 6th output;
	  cat('\n','\n',"          ASINTOTIC SOLUTION (based in raw data)",'\n')
      cat("(These solutions have been obtained like in previous cases, but adding c insteed of 0.5 and let c tend 0)",'\n')
      cat('\n',"Data analyzed by Delta model",'\n')
	  print(x$M1)
      
	  if (dtp2 == "DA0") {  
        cat("Some of the delta model parameters are at the border of the parametric space, so SE can not be ",'\n',
        "estimated. That is why the following table only provides the parameters estimators of the",'\n',
        "model and the measures of agreement.",'\n')
        
        if (samplingtype<=3){
          cat("Totals in rows are prefixed",'\n')
        }
        else {
          cat("Totals in rows and columns are at random",'\n')
        }
        if (samplingtype==1 | samplingtype==4){
          cat("Row observer is standard",'\n')
        } 
        else if (samplingtype==2){
          cat("Row observer is in columns",'\n')
        } 
        else {
          cat("There isn't a standard observer",'\n')
        }
        # All model parameters
        temp = paste("Model Parameters (Delta and Pi) and All Concordance Measures ")
        Encoding(temp) = "UTF-8"
        cat('\n',temp,'\n')
        print(x$Paramstable_AN, row.names = FALSE)
        temp = paste("Delta = ", x$Deltaoverall_AN)
        Encoding(temp) = "UTF-8"
        cat(temp,'\n')
        
        cat("The results that follow are based on the original data increased by 0.5.",'\n')
        print(x$M3)
        # All model parameters
        temp = paste("Model Parameters (Delta and Pi) and All Concordance Measures \u00b1 S.E.")
        Encoding(temp) = "UTF-8"
        cat('\n',temp,'\n')
        print(x$Paramstable_AN2, row.names = FALSE)
        temp = paste("Delta \u00b1 S.E. = ", x$Deltaoverall_AN2," \u00b1 ",x$Deltaoverall_SE_AN2)
        Encoding(temp) = "UTF-8"
        cat(temp,'\n')
      }
      else {
        if (samplingtype<=3){
          cat("Totals in rows are prefixed",'\n')
        }
        else {
          cat("Totals in rows and columns are at random",'\n')
        }
        if (samplingtype==1 | samplingtype==4){
          cat("Row observer is standard",'\n')
        } 
        else if (samplingtype==2){
          cat("Row observer is in columns",'\n')
        } 
        else {
          cat("There isn't a standard observer",'\n')
        }
        # All model parameters
        temp = paste("Model Parameters (Delta and Pi) and All Concordance Measures ")
        Encoding(temp) = "UTF-8"
        cat('\n',temp,'\n')
        print(x$Paramstable_AN, row.names = FALSE)
        temp = paste("Delta \u00b1 S.E. = ", x$Deltaoverall_AN," \u00b1 ",x$Deltaoverall_SE_AN)
        Encoding(temp) = "UTF-8"
        cat(temp,'\n')
      }
        
      # 7th output;
      cat('\n',"               SUMMARY: Kappa + Delta",'\n')
	  print(x$Summary_AN, row.names = FALSE)
	  
	  if (dtp2 == "DA0" || ktp == "K0" ) {
		cat("* This value has been obtained adding 0.5 to the original data) ",'\n')
	  }
	  
	  ### ASINTOTIC EXTRA SOLUTION ###;
      # 8th output;
	  cat('\n',"        ASINTOTIC SOLUTION (based in raw data incremented by 1)",'\n')
      cat("(These solutions have been obtained like in previous cases, but for original data incremented in 1)",'\n')
      cat('\n',"Data analyzed by Delta model",'\n')
	  print(x$M4)
      
      #Model parameters selected
	  temp = paste("Model Parameters (Delta and Pi) and Concordance Measures \u00b1 S.E. with selected conditions",'\n',
					"(Asintotic Extra)")
	  Encoding(temp) = "UTF-8"
	  cat('\n',temp,'\n')
	  if (samplingtype<=3){
	    cat("Totals in rows are prefixed",'\n')
	  }
	  else {
	    cat("Totals in rows and columns are at random",'\n')
	  }
  	  if (samplingtype==1 | samplingtype==4){
	    cat("Row observer is standard",'\n')
	  } 
  	  else if (samplingtype==2){
	    cat("Row observer is in columns",'\n')
	  } 
	  else {
	    cat("There isn't a standard observer",'\n')
	  }
	  print(x$Paramstable_AE, row.names = FALSE)
	  temp = paste("Delta \u00b1 S.E. = ", x$Deltaoverall_AE," \u00b1 ",x$Deltaoverall_SE_AE)
	  Encoding(temp) = "UTF-8"
	  cat(temp,'\n')
      
      # 9th output;
      cat('\n',"               SUMMARY: Kappa + Delta",'\n')
	  print(x$Summary_AE, row.names = FALSE)
	}
  }
   cat("End of the problem: K = ", x$k,'\n')
}
