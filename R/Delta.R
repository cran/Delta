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

  #Get type of problem
  tp = GetProblemType(M1,k)
  #For type 1, no extra matrix are needed
  if (tp != "1.0" & tp != "1.1"){
    Mxs = GetMx(tp, M1)
    M2  = Mxs$M2
    M3  = Mxs$M3
    M4  = Mxs$M4
    M5  = Mxs$M5
	#NO asintotics calculation 
	#Calculations: Part 2, derivate Kappa and SE
	kappa.res = GetKappa(M4)
	if (tp == "2.1" | tp == "3.1"){
		kappa.res$kappa = 1
	}
	
	kappa.val 	= kappa.res$kappa
	kappa.SE 	= kappa.res$SE
	
	#Calculations: Part 2, calculate pi_i, delta_i
	res.Pi 	= GetDeltaPi(M2,tp,tol,maxits)
	Pi 		= res.Pi$Pi
	Delta 	= res.Pi$Delta
	B 		= res.Pi$B
   	#mat will be the matrix to be used for calcs of covariance
	#Also Delta_cov, Pi_cov and B_cov
	mat 	= M2
	Pi_cov 	= res.Pi$Pi
	Delta_cov = res.Pi$Delta
	B_cov	= res.Pi$B
  
	#Goodnes of fit to the model
    res = GetGoodness(mat,Pi,Delta)
	X.sq 	= res$X.squared
	df 		= res$df
	p.val 	= res$p.value
	E.matrix = res$E.matrix
	  
	#Update Delta and Pi to correct size in cases 2.X
	#if (k==2){
	#  Pi 	= Pi[-3]
	#  Delta = Delta[-3]
	#}
	  
	#For 3.1 and 3.4, redo calcs with M3 for delta and pi
	#To avoid issues with tp = 3.1, tp will be fixed to 3.4
	if (tp == "3.1" | tp == "3.4"){
	  res.Pi 	= GetDeltaPi(M3,"3.4",tol,maxits)
	  Pi_cov 	= res.Pi$Pi
	  Delta_cov = res.Pi$Delta
	  B_cov 	= res.Pi$B
	  mat 		= M3
	}
	
	Covar = GetCovariance(mat,Delta_cov,Pi_cov,B_cov)
	Cov_Delta 	= Covar$Cov_Delta
	Cov_mix 	= Covar$Cov_mix
	Cov_Pi 		= Covar$Cov_Pi
	E			= Covar$E
		  
	#Update Delta and Pi to correct size in cases 2.X
	if (k==2){
	  Pi 	= Pi[-3]
	  Pi_cov= Pi_cov[-3]
	  Delta = Delta[-3]
	  Delta_cov = Delta_cov[-3]
	  Cov_Delta = Cov_Delta[-3,-3]
	  Cov_mix = Cov_mix[-3,-3]
	  Cov_Pi = Cov_Pi[-3,-3]
	}
	
	res.Params = GetDeltaParams(M2, Delta, Pi, k)
	res.ParamsVar = GetDeltaParamsVar(mat,fixedrows, Delta_cov, Pi_cov, k, Cov_Delta, E)
	
	#Calculations Part 3: Asintotic calculations
	if (tp == "2.0" | tp == "2.1" | tp == "2.2"){
	  res.M4 = GetAsinDeltaParams(M4,fixedrows)
	  res.M5 = GetAsinDeltaParams(M5,fixedrows)
	}	
  }
  
  if (tp == "2.0" | tp == "2.1" | tp == "2.2" | tp == "3.0" | tp == "3.2" | tp == "3.3"){
    res = list("Raw.matrix" = datatable, "M1" = M1,"M2" = M2)
  }
  else {
    res = list("Raw.matrix" = datatable, "M1" = M1)
  }
  
  res$Del_rows = Del_rows
  res$tp = tp
  res$k = k
  res$k0 = k_0
  res$showall = showall
  res$samplingtype = delta.samplingtype
  
  if (k >= 2){
	  res$E_matrix 	= round(E.matrix,dplaces)
	  Chisq 		=     paste("Chi-squared =", round(X.sq,dplaces), "(d.f.=",df,"); p=", round(p.val,dplaces))
	  if (tp == "2.0" | tp == "3.0") {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), "* \u00b1 ",round(kappa.SE,dplaces), "*")
	  }
	  else if (tp == "2.1" | tp == "3.1") {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), " \u00b1 ",round(kappa.SE,dplaces), "*")
	  } 
	  else {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), " \u00b1 ",round(kappa.SE,dplaces))
	  }
	  Delta.results	= paste("Delta \u00b1 S.E. = ",round(res.Params$Delta.total,dplaces), " \u00b1 ",round(res.ParamsVar$Delta.total.cov,dplaces))
	  
	  Encoding(Chisq) = "UTF-8"
	  Encoding(Kappa.results) = "UTF-8"
	  Encoding(Delta.results) = "UTF-8"

	  Summary1 = data.frame(c(Chisq,Kappa.results,Delta.results))
	  names(Summary1)	= "Summary"
	  res$Summary 		= Summary1
	  res$Cov_Delta		= round(Cov_Delta,dplaces)
	  res$Cov_mix		= round(Cov_mix,dplaces)
	  res$Cov_Pi		= round(Cov_Pi,dplaces)

	  if (!is.null(res.ParamsVar$A.cov)) {
		Agreement = paste(round(res.Params$A,dplaces)," \u00b1 ",round(res.ParamsVar$A.cov,dplaces))
	  }
	  else{
	    Agreement = paste(round(res.Params$A,dplaces))
	  }
	  if (!is.null(res.ParamsVar$F.cov)) {
	    Conformity = paste(round(res.Params$F,dplaces)," \u00b1 ",round(res.ParamsVar$F.cov,dplaces))
	  }
	  else{
	    Conformity = paste(round(res.Params$F,dplaces))
	  }
	  if (!is.null(res.ParamsVar$P.cov)) {
	    Predictivity = paste(round(res.Params$P,dplaces)," \u00b1 ",round(res.ParamsVar$P.cov,dplaces))
	  }
	  else{
	    Predictivity = paste(round(res.Params$P,dplaces))
	  }
	  if (!is.null(res.ParamsVar$S.cov)) {
	    Consistency = paste(round(res.Params$S,dplaces)," \u00b1 ",round(res.ParamsVar$S.cov,dplaces))
	  }
	  else{
	    Consistency = paste(round(res.Params$S,dplaces))
	  }
	  Encoding(Agreement) = "UTF-8"
	  Encoding(Conformity) = "UTF-8"
	  Encoding(Predictivity) = "UTF-8"
	  Encoding(Consistency) = "UTF-8"

	  
	  if (is.null(Pi)){
		Pi		= rep(0,k)
	  }
	  if (tp == "3.1" | tp == "3.4"){
	    Delta_tab 	= paste(round(Delta,dplaces), "(" ,round(Delta_cov,dplaces) , "*)")
	    Pi_tab 		= paste(round(Pi,dplaces), "(" , round(Pi_cov,dplaces), "*)")
	    Table 		= data.frame(1:k,Delta_tab,Pi_tab,Agreement,Conformity,Predictivity,Consistency)
		colnames(Table) = c("Class","Delta (*)","Pi (*)","Agreement","Conformity","Predictivity","Consistency")
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

	  #Asintotic solutions
	  # res.m4 and res.M5
	  if (k==2){
		#Salida 8
		if (tp == 2.0){
		  res$M_AN = M4
		}
		else {
		  res$M_AN = M1
		}
		
		if (tp == "2.0" ) {
		  Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), "* \u00b1 ",round(kappa.SE,dplaces), "*")
		}
		else if (tp == "2.1" ) {
		  Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), " \u00b1 ",round(kappa.SE,dplaces), "*")
		}
		else {
		  Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), " \u00b1 ",round(kappa.SE,dplaces))
		}

		Delta.results	= paste("Delta \u00b1 S.E. = ",round(res.M4$Delta.total,dplaces), " \u00b1 ",round(res.M4$Delta.total.cov,dplaces))
		
		Encoding(Kappa.results) = "UTF-8"
		Encoding(Delta.results) = "UTF-8"

		Summary2 = data.frame(c(Kappa.results,Delta.results))
		names(Summary2)	= "Summary"
		res$Summary_AN = Summary2
		
		
	  if (!is.null(res.M4$A.cov)) {
		Agreement = paste(round(res.M4$A,dplaces)," \u00b1 ",round(res.M4$A.cov,dplaces))
	  }
	  else{
	    Agreement = paste(round(res.M4$A,dplaces))
	  }
	  if (!is.null(res.M4$F.cov)) {
	    Conformity = paste(round(res.M4$F,dplaces)," \u00b1 ",round(res.M4$F.cov,dplaces))
	  }
	  else{
	    Conformity = paste(round(res.M4$F,dplaces))
	  }
	  if (!is.null(res.M4$P.cov)) {
	    Predictivity = paste(round(res.M4$P,dplaces)," \u00b1 ",round(res.M4$P.cov,dplaces))
	  }
	  else{
	    Predictivity = paste(round(res.M4$P,dplaces))
	  }
	  if (!is.null(res.M4$S.cov)) {
	    Consistency = paste(round(res.M4$S,dplaces)," \u00b1 ",round(res.M4$S.cov,dplaces))
	  }
	  else{
	    Consistency = paste(round(res.M4$S,dplaces))
	  }
		Encoding(Agreement) = "UTF-8"
		Encoding(Conformity) = "UTF-8"
		Encoding(Predictivity) = "UTF-8"
		Encoding(Consistency) = "UTF-8"
		
		if (tp == "2.1" ){
	      Delta_tab = paste(round(Delta,dplaces), "(" ,round(res.M4$Delta,dplaces) , "*)")
	      Pi_tab = paste(round(Pi,dplaces), "(" , round(res.M4$Pi,dplaces), "*)")
		  Table = data.frame(1:k,Delta_tab,Pi_tab,Agreement,Conformity,Predictivity,Consistency)
		  colnames(Table) = c("Class","Delta (*)","Pi (*)","Agreement (*)","Conformity (*)","Predictivity (*)","Consistency (*)")
		} 
		else {
		  Table = data.frame(1:k,round(res.M4$Delta,dplaces),round(res.M4$Pi,dplaces),Agreement,Conformity,Predictivity,Consistency)
		  colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")
		}
		res$Fullparamstable_AN = Table
		res$Deltaoverall_AN = round(res.M4$Delta.total,dplaces)
		res$Deltaoverall_SE_AN  = round(res.M4$Delta.total.cov,dplaces)
		  
		  
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
		  
		  #Extra 
		
		  res$M_AE = M5
		 
		
		  if (tp == "2.0" ) {
			Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), "* \u00b1 ",round(kappa.SE,dplaces), "*")
		  }
		  else if (tp == "2.1" ) {
			Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), " \u00b1 ",round(kappa.SE,dplaces), "*")
		  }
		  else {
			Kappa.results	= paste("Kappa \u00b1 S.E. = ",round(kappa.val,dplaces), " \u00b1 ",round(kappa.SE,dplaces))
		  }

		  Delta.results	= paste("Delta \u00b1 S.E. = ",round(res.M5$Delta.total,dplaces), " \u00b1 ",round(res.M5$Delta.total.cov,dplaces))
		  
		  Encoding(Kappa.results) = "UTF-8"
		  Encoding(Delta.results) = "UTF-8"

		  Summary2 = data.frame(c(Kappa.results,Delta.results))
		  names(Summary2)	= "Summary"
		  res$Summary_AE = Summary2
		  
		  if (!is.null(res.M5$A.cov)) {
			Agreement = paste(round(res.M5$A,dplaces)," \u00b1 ",round(res.M5$A.cov,dplaces))
		  }
		  else{
			Agreement = paste(round(res.M5$A,dplaces))
		  }
		  if (!is.null(res.M5$F.cov)) {
			Conformity = paste(round(res.M5$F,dplaces)," \u00b1 ",round(res.M5$F.cov,dplaces))
		  }
		  else{
			Conformity = paste(round(res.M5$F,dplaces))
		  }
		  if (!is.null(res.M5$P.cov)) {
			Predictivity = paste(round(res.M5$P,dplaces)," \u00b1 ",round(res.M5$P.cov,dplaces))
		  }
		  else{
			Predictivity = paste(round(res.M5$P,dplaces))
		  }
		  if (!is.null(res.M5$S.cov)) {
			Consistency = paste(round(res.M5$S,dplaces)," \u00b1 ",round(res.M5$S.cov,dplaces))
		  }
		  else{
			Consistency = paste(round(res.M5$S,dplaces))
		  }

		  Encoding(Agreement) = "UTF-8"
		  Encoding(Conformity) = "UTF-8"
		  Encoding(Predictivity) = "UTF-8"
		  Encoding(Consistency) = "UTF-8"
		  
		  Table = data.frame(1:k,round(res.M5$Delta,dplaces),round(res.M5$Pi,dplaces),Agreement,Conformity,Predictivity,Consistency)
		  colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")
		  res$Fullparamstable_AE = Table
		  res$Deltaoverall_AE = round(res.M5$Delta.total,dplaces)
		  res$Deltaoverall_SE_AE  = round(res.M5$Delta.total.cov,dplaces)
		  
		  
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
