#' Delta coefficient function
#'
#' This function provides an analysis of the matrix provided, returning all all the parameters estimations
#' and SE calculations that have sense with the fixedrows and gstandard provided.
#'
#' This function study the matrix provided by the user. This function modify the matrix deleting missing rows and columns and 
#' if it is needed for the estimation, adding 0.5 to each cell.  
#' 
#' Also calculate Cohen's Kappa coefficient and the goodness of fit for the Delta model.
#' @param datatable Matrix. Expected to be square matrix with at least 2 rows (columns), non negative values and at least an element different of zero.
#' @param fixedrows Boolean. Indicate if sample rows are fixed beforehand. Default is TRUE.
#' @param gstandard Text. Indicate if there are a Gold Standard by Rows or columns. Only first letter matter without Case sensitivity. Options are: "N" for None, "R" for in Rows and "C" for in Columns. Default is "N".
#' @param maxits Whole number. Indicate the maximum number of iterations of the numeric method to calculate B. Expected to be 100 <= maxits <= 5000. Default is 1000.
#' @param tol Double number. Indicate the precision of the numeric method to calculate B. Expected to be 1e-6 <= tol <= 1e-15.Default is 1e-12.
#' @param dplaces Whole number. Decimal placed to be shown in the result. Expected to be 1 <= dplaces <0 6. Default 4.
#' @param showall Boolean. Indicate if all output should be shown. If TRUE also shown hidden results. If FALSE shown only main output. By default is FALSE.
#' @keywords Delta datatable fixedrows gstandard maxits tol dplaces
#' @export
#' @examples
#' Delta(matrix(c(1,2,3,4),2,2))
#' Delta(matrix(c(65,5,10,20),2,2),fixedrows=TRUE,gstandard="Row")

Delta <- function(datatable,fixedrows = FALSE, gstandard = "No",
         maxits=1000,tol=1e-12,dplaces=4,showall = FALSE){

  # Check input variables
  delta.samplingtype = CheckInput(datatable,fixedrows ,gstandard ,maxits,tol,dplaces,showall)

  #Keep original size
  k_0 	= dim(datatable)[1]

  #Calculate matrix deleting Unnecessary rows
  res 		= GetM1(datatable)
  M1  		= res$M1
  k   		= res$k
  Del_rows 	= res$Deleted
  
  #For type 1, no extra matrix are needed
  if (k >= 2){
    Mxs = GetMx(M1)
    M2  = Mxs$M2
    M3  = Mxs$M3
    M4  = Mxs$M4
    
    #Get type of problem
    ktp = GetKappaProblemType(M1)
    if (k > 2) {
      dtp = GetDeltaProblemType(M1)
    }
    else {
      dtp  = GetDeltaProblemType(M2)
      dtp2 = GetDeltaProblemType(M1)
    }
    
   	#mat will be the matrix to be used for calcs of covariance
    if (k > 2){
      mat = M1
      if (dtp == "DN0" || dtp == "DN1"){
        mat.SE = M3
      }
      else if (dtp == "DN2"){
        mat.SE = M1
      }
    }
    else if (k == 2){
      mat = M2
      mat.SE = M2
    }
    
	#NO asintotics calculation 
	#Calculations: Part 2, derivate Kappa and SE
	kappa.res = GetKappa(M1)
    # Kappa estimation done with M1
	kappa.val = kappa.res$kappa
	if (ktp == "K0"){     
	  kappa.res = GetKappa(M3)
	}
    # Kappa SE done with M1 if K1 or M3 if K0
	kappa.SE 	= kappa.res$SE
	
	#Calculations: Part 2, calculate pi_i, delta_i
	res.Pi 	= GetDeltaPi(mat,dtp,tol,maxits)
	Pi 		= res.Pi$Pi
	Delta 	= res.Pi$Delta
	B 		= res.Pi$B
    
	#Also Delta_cov, Pi_cov and B_cov
    if (dtp == "DN0" || dtp == "DN1"){
	  res.Pi.M3 = GetDeltaPi(M3,dtp,tol,maxits, original.mx = FALSE)
      Pi_cov 	= res.Pi.M3$Pi
      Delta_cov = res.Pi.M3$Delta
      B_cov	    = res.Pi.M3$B
    }
    else {
	  Pi_cov 	= res.Pi$Pi
	  Delta_cov = res.Pi$Delta
	  B_cov	    = res.Pi$B
    }
  
    #Goodnes of fit to the model if possible
    if (dtp == "DN0" ){
      res = GetGoodness(M3,Pi_cov,Delta_cov)
    }
    else {
      res = GetGoodness(mat,Pi_cov,Delta_cov)
    }
    X.sq 	= res$X.squared
    df 		= res$df
    p.val 	= res$p.value
    E.matrix = res$E.matrix
	
    Covar = GetCovariance(mat.SE,Delta_cov,Pi_cov,B_cov)
    Cov_Delta 	= Covar$Cov_Delta
    Cov_mix 	= Covar$Cov_mix
    Cov_Pi 		= Covar$Cov_Pi
    E			= Covar$E
		 
	#Update Delta and Pi to correct size in cases k = 2
	if (k == 2){
	  Pi 	= Pi[-3]
	  Pi_cov= Pi_cov[-3]
	  Delta = Delta[-3]
	  Delta_cov = Delta_cov[-3]
	  Cov_Delta = Cov_Delta[-3,-3]
	  Cov_mix = Cov_mix[-3,-3]
	  Cov_Pi = Cov_Pi[-3,-3]
	}
	
	res.Params    = GetDeltaParams(mat, Delta, Pi, k)
	res.Params2   = GetDeltaParams(mat.SE, Delta_cov, Pi_cov, k)
    res.ParamsVar = GetDeltaParamsVar(mat.SE,fixedrows, Delta_cov, Pi_cov, k, Cov_Delta, E)
	
	#Calculations Part 3: Asintotic calculations
	if (k == 2){
	  res.M1 = GetAsinDeltaParams(M1,fixedrows)
      if (dtp2 == "DA0") {
	    res.M3 = GetAsinDeltaParams(M3,fixedrows)
      }
	  res.M4 = GetAsinDeltaParams(M4,fixedrows)
	  kappa.res.asin = GetKappa(M4)
	}	
  }

  if (k > 2){
    res = list("Raw.matrix" = datatable, "M1" = M1,"M3" = M3)
  }
  else {
    res = list("Raw.matrix" = datatable, "M1" = M1,"M2" = M2,"M3" = M3,"M4" = M4)
  }
  
  res$Del_rows = Del_rows
  res$dtp = dtp
  if (k == 2){
    res$dtp2 = dtp2
  }
  res$ktp = ktp
  res$k = k
  res$k0 = k_0
  res$showall = showall
  res$samplingtype = delta.samplingtype
  
  if (is.null(res.ParamsVar$P.cov)){
    res.ParamsVar$P.cov		= rep(NaN,k)
  }
  if (is.null(res.ParamsVar$S.cov)){
    res.ParamsVar$S.cov		= rep(NaN,k)
  }
  #First table 1
  if (dtp != "DN1" && dtp != "DN0") {
    Agreement     = paste(round(res.Params$A,dplaces)," \u00b1 ",round(res.ParamsVar$A.cov,dplaces))
    Conformity    = paste(round(res.Params$F,dplaces)," \u00b1 ",round(res.ParamsVar$F.cov,dplaces))
    Predictivity  = paste(round(res.Params$P,dplaces)," \u00b1 ",round(res.ParamsVar$P.cov,dplaces))
    Consistency   = paste(round(res.Params$S,dplaces)," \u00b1 ",round(res.ParamsVar$S.cov,dplaces))
  }
  else{
    Agreement     = paste(round(res.Params$A,dplaces))
    Conformity    = paste(round(res.Params$F,dplaces))
    Predictivity  = paste(round(res.Params$P,dplaces))
    Consistency   = paste(round(res.Params$S,dplaces))
  }

  Encoding(Agreement)     = "UTF-8"
  Encoding(Conformity)    = "UTF-8"
  Encoding(Predictivity)  = "UTF-8"
  Encoding(Consistency)   = "UTF-8"

  if (is.null(Pi)){
    Pi		= rep(NaN,k)
  }
  
  if (dtp != "DN1" && dtp != "DN0"){
    Delta_tab 	= paste(round(Delta,dplaces), "(" ,round(Delta_cov,dplaces) , ")")
    Pi_tab 		= paste(round(Pi,dplaces), "(" , round(Pi_cov,dplaces), ")")
    Table 		= data.frame(1:k,Delta_tab,Pi_tab,Agreement,Conformity,Predictivity,Consistency)
    colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")
  } 
  else {
    Table = data.frame(1:k,round(Delta,dplaces),round(Pi,dplaces),Agreement,Conformity,Predictivity,Consistency)
    colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")
  }

  res$Fullparamstable = Table
  res$Deltaoverall = round(res.Params$Delta.total,dplaces)
  res$Deltaoverall_SE  = round(res.ParamsVar$Delta.total.cov,dplaces)
  
  if (delta.samplingtype == 1) {
    Table = Table[,c(-6,-7)]
  }
  else if (delta.samplingtype == 2) {
    Table = Table[,c(-5,-6,-7)]
  }
  else if (delta.samplingtype == 3) {
    Table = Table[,c(-5,-6,-7)]
  }
  else if (delta.samplingtype == 4) {
    Table = Table[,-7]
  }
  else if (delta.samplingtype == 5) {
    Table = Table[,c(-5,-6)]
  }

  res$Paramstable = Table 
  
  if (dtp == "DN1" || dtp == "DN0"){
    #First table with M3
      Agreement     = paste(round(res.Params2$A,dplaces)," \u00b1 ",round(res.ParamsVar$A.cov,dplaces))
      Conformity    = paste(round(res.Params2$F,dplaces)," \u00b1 ",round(res.ParamsVar$F.cov,dplaces))
      Predictivity  = paste(round(res.Params2$P,dplaces)," \u00b1 ",round(res.ParamsVar$P.cov,dplaces))
      Consistency   = paste(round(res.Params2$S,dplaces)," \u00b1 ",round(res.ParamsVar$S.cov,dplaces))

      Encoding(Agreement)     = "UTF-8"
      Encoding(Conformity)    = "UTF-8"
      Encoding(Predictivity)  = "UTF-8"
      Encoding(Consistency)   = "UTF-8"

      Delta_tab 	= paste(round(Delta_cov,dplaces))
      Pi_tab 		= paste(round(Pi_cov,dplaces))
      Table 		= data.frame(1:k,Delta_tab,Pi_tab,Agreement,Conformity,Predictivity,Consistency)
      colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")

      res$Fullparamstable2 = Table
      res$Deltaoverall2 = round(res.Params2$Delta.total,dplaces)
      res$Deltaoverall_SE2  = round(res.ParamsVar$Delta.total.cov,dplaces)
        
      if (delta.samplingtype == 1) {
        Table = Table[,c(-6,-7)]
      }
      else if (delta.samplingtype == 2) {
        Table = Table[,c(-5,-6,-7)]
      }
      else if (delta.samplingtype == 3) {
        Table = Table[,c(-5,-6,-7)]
      }
      else if (delta.samplingtype == 4) {
        Table = Table[,-7]
      }
      else if (delta.samplingtype == 5) {
        Table = Table[,c(-5,-6)]
      }

      res$Paramstable2 = Table 
  } 


  res$Cov_Delta	= round(Cov_Delta,dplaces)
  res$Cov_mix	= round(Cov_mix,dplaces)
  res$Cov_Pi	= round(Cov_Pi,dplaces)
  
  if (k >= 2){
	  res$E_matrix 	= round(E.matrix,dplaces)
	  Chisq 		=     paste("Chi-squared =", round(X.sq,dplaces), "(d.f.=",df,"); p=", round(p.val,dplaces))
	  if (ktp == "K0") {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), " \u00b1 ",round(kappa.SE,dplaces), "*")
	  }
	  else {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), " \u00b1 ",round(kappa.SE,dplaces))
	  } 
	  if (dtp == "DN0" || dtp == "DN1") {
	    Delta.results	= paste("Delta \u00b1 S.E. = ",round(res.Params2$Delta.total,dplaces), " *\u00b1 ",round(res.ParamsVar$Delta.total.cov,dplaces), "*")
      }
      else {
      	Delta.results	= paste("Delta \u00b1 S.E. = ",round(res.Params$Delta.total,dplaces), " \u00b1 ",round(res.ParamsVar$Delta.total.cov,dplaces))
      }
	  
	  Encoding(Chisq) = "UTF-8"
	  Encoding(Kappa.results) = "UTF-8"
	  Encoding(Delta.results) = "UTF-8"

	  Summary1 = data.frame(c(Chisq,Kappa.results,Delta.results))
	  names(Summary1)	= "Summary"
	  res$Summary 		= Summary1


	  #Asintotic solutions
	  # res.m4 and res.M5
	  if (k == 2){
		
		if (ktp == "K0" ) {
		  Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), " \u00b1 ",round(kappa.SE,dplaces), "*")
		}
		else {
		  Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), " \u00b1 ",round(kappa.SE,dplaces))
		}

		if (dtp2 == "DA0" ) {
		  Delta.results	= paste("Delta \u00b1 S.E. = ",round(res.M1$Delta.total,dplaces), " \u00b1 ",round(res.M3$Delta.total.cov,dplaces),"*")
        }
        else {
		  Delta.results	= paste("Delta \u00b1 S.E. = ",round(res.M1$Delta.total,dplaces), " \u00b1 ",round(res.M1$Delta.total.cov,dplaces))
        }
		
		Encoding(Kappa.results) = "UTF-8"
		Encoding(Delta.results) = "UTF-8"

		Summary2 = data.frame(c(Kappa.results,Delta.results))
		names(Summary2)	= "Summary"
		res$Summary_AN = Summary2
		  
        if (is.null(res.M1$P.cov)){
          res.M1$P.cov		= rep(NaN,k)
        }
        if (is.null(res.M1$S.cov)){
          res.M1$S.cov		= rep(NaN,k)
        }
          
		if (dtp2 == "DA0"){
	      Agreement = paste(round(res.M1$A,dplaces))
	      Conformity = paste(round(res.M1$F,dplaces))
	      Predictivity = paste(round(res.M1$P,dplaces))
	      Consistency = paste(round(res.M1$S,dplaces))
        }
        else {
		  Agreement = paste(round(res.M1$A,dplaces)," \u00b1 ",round(res.M1$A.cov,dplaces))
	      Conformity = paste(round(res.M1$F,dplaces)," \u00b1 ",round(res.M1$F.cov,dplaces))
	      Predictivity = paste(round(res.M1$P,dplaces)," \u00b1 ",round(res.M1$P.cov,dplaces))
	      Consistency = paste(round(res.M1$S,dplaces)," \u00b1 ",round(res.M1$S.cov,dplaces))
        }
	  
		Encoding(Agreement) = "UTF-8"
		Encoding(Conformity) = "UTF-8"
		Encoding(Predictivity) = "UTF-8"
		Encoding(Consistency) = "UTF-8"

		Table = data.frame(1:k,round(res.M1$Delta,dplaces),round(res.M1$Pi,dplaces),Agreement,Conformity,Predictivity,Consistency)
		colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")
		
		res$Fullparamstable_AN = Table
		res$Deltaoverall_AN = round(res.M1$Delta.total,dplaces)
        if (dtp2 == "DA1"){
		  res$Deltaoverall_SE_AN  = round(res.M1$Delta.total.cov,dplaces)
        }		  
		  
		if (delta.samplingtype == 1) {
		  Table = Table[,c(-6,-7)]
		}
		else if (delta.samplingtype == 2) {
		  Table = Table[,c(-5,-6,-7)]
		}
		else if (delta.samplingtype == 3) {
		  Table = Table[,c(-5,-6,-7)]
		}
		else if (delta.samplingtype == 4) {
		  Table = Table[,-7]
		}
		else if (delta.samplingtype == 5) {
		  Table = Table[,c(-5,-6)]
		}

		res$Paramstable_AN = Table
        
        if (dtp2 == "DA0") {
        	  
          if (is.null(res.M3$P.cov)){
            res.M3$P.cov		= rep(NaN,k)
          }
          if (is.null(res.M3$S.cov)){
            res.M3$S.cov		= rep(NaN,k)
          }
          
          Agreement = paste(round(res.M3$A,dplaces)," \u00b1 ",round(res.M3$A.cov,dplaces))
	      Conformity = paste(round(res.M3$F,dplaces)," \u00b1 ",round(res.M3$F.cov,dplaces))
	      Predictivity = paste(round(res.M3$P,dplaces)," \u00b1 ",round(res.M3$P.cov,dplaces))
	      Consistency = paste(round(res.M3$S,dplaces)," \u00b1 ",round(res.M3$S.cov,dplaces))
          
          Encoding(Agreement) = "UTF-8"
          Encoding(Conformity) = "UTF-8"
          Encoding(Predictivity) = "UTF-8"
          Encoding(Consistency) = "UTF-8"

          Table = data.frame(1:k,round(res.M3$Delta,dplaces),round(res.M3$Pi,dplaces),Agreement,Conformity,Predictivity,Consistency)
          colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")
          
          res$Fullparamstable_AN2 = Table
          res$Deltaoverall_AN2 = round(res.M3$Delta.total,dplaces)
          res$Deltaoverall_SE_AN2  = round(res.M3$Delta.total.cov,dplaces)
            
            
          if (delta.samplingtype == 1) {
            Table = Table[,c(-6,-7)]
          }
          else if (delta.samplingtype == 2) {
            Table = Table[,c(-5,-6,-7)]
          }
          else if (delta.samplingtype == 3) {
            Table = Table[,c(-5,-6,-7)]
          }
          else if (delta.samplingtype == 4) {
            Table = Table[,-7]
          }
          else if (delta.samplingtype == 5) {
            Table = Table[,c(-5,-6)]
          }

          res$Paramstable_AN2 = Table
        }
		  
		#Extra 
        ## Adding 1
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.res.asin$kappa,dplaces), " \u00b1 ",round(kappa.res.asin$SE,dplaces))
		Delta.results	= paste("Delta \u00b1 S.E. = ",round(res.M4$Delta.total,dplaces), " \u00b1 ",round(res.M4$Delta.total.cov,dplaces))
		
		Encoding(Kappa.results) = "UTF-8"
		Encoding(Delta.results) = "UTF-8"

		Summary2 = data.frame(c(Kappa.results,Delta.results))
		names(Summary2)	= "Summary"
		res$Summary_AE = Summary2
		
        	  
        if (is.null(res.M4$P.cov)){
          res.M4$P.cov		= rep(NaN,k)
        }
        if (is.null(res.M4$S.cov)){
          res.M4$S.cov		= rep(NaN,k)
        }
        
		Agreement = paste(round(res.M4$A,dplaces)," \u00b1 ",round(res.M4$A.cov,dplaces))
		Conformity = paste(round(res.M4$F,dplaces)," \u00b1 ",round(res.M4$F.cov,dplaces))
		Predictivity = paste(round(res.M4$P,dplaces)," \u00b1 ",round(res.M4$P.cov,dplaces))
		Consistency = paste(round(res.M4$S,dplaces)," \u00b1 ",round(res.M4$S.cov,dplaces))
		

		Encoding(Agreement) = "UTF-8"
		Encoding(Conformity) = "UTF-8"
		Encoding(Predictivity) = "UTF-8"
		Encoding(Consistency) = "UTF-8"
		
		Table = data.frame(1:k,round(res.M4$Delta,dplaces),round(res.M4$Pi,dplaces),Agreement,Conformity,Predictivity,Consistency)
		colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")
		res$Fullparamstable_AE = Table
		res$Deltaoverall_AE = round(res.M4$Delta.total,dplaces)
		res$Deltaoverall_SE_AE  = round(res.M4$Delta.total.cov,dplaces)
		
		
		if (delta.samplingtype == 1) {
		  Table = Table[,c(-6,-7)]
		}
		else if (delta.samplingtype == 2) {
		  Table = Table[,c(-5,-6,-7)]
		}
		else if (delta.samplingtype == 3) {
		  Table = Table[,c(-5,-6,-7)]
		}
		else if (delta.samplingtype == 4) {
		  Table = Table[,-7]
		}
		else if (delta.samplingtype == 5) {
		  Table = Table[,c(-5,-6)]
		}

		res$Paramstable_AE = Table
	  }
	}
  class(res) <- "Delta"
  return(res)

}
#Delta(matrix(c(10,0,0,0,10,0,0,0,10),3,3),fixedrows=FALSE,gstandard="No",maxits=100,tol=1e-12,dplaces=4)