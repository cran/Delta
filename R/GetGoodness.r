#' Calculate Goodness of fit function
#'
#' This function provide an Chi-square test for the given matrix, Delta and Pi provided.
#' 
#' @param mx Matrix. Modified matrix to have a solution. Usually GetMx$M1 for k>2 and GetMx$M2 in case of k = 2.
#' @param Delta Vector. Each element indicate the probability of recognize an element i.
#' @param Pi Vector. Each element indicate the probability of classify at random an element in category i.
#' @keywords Delta mx Pi Delta Goodness
#' @export
#' @examples
#' GetGoodness(mx = matrix(c(1,0,0,0,2,0,0,0,3),3,3), Delta = c(1,1,1), Pi = NULL)
#' GetGoodness(mx = matrix(c(1.5,2.5,0.5,3.5,4.5,0.5,0.5,0.5,1.5),3,3), 
#'             Delta = c(-0.2662395,  0.2047577,  0.5664672), 
#'             Pi = c(0.42564365, 0.49700867, 0.07734769))
#' GetGoodness(mx = matrix(c(60,0,3,2,50,1,3,2,79),3,3), 
#'             Delta = c( 0.8945724, 0.9522836, 0.8962094), 
#'             Pi = c( 0.2703707, 0.1939561, 0.5356732))

GetGoodness <- function(mx,Pi,Delta){
  #Calculate aux variables
  dim.matrix  = dim(mx)[1]
  R.marg   	  = margin.table(mx,1)

  #In case of Null pi, set to 0
  if (is.null(Pi)){
    Pi		= rep(0,dim.matrix)
    Delta	= rep(1,dim.matrix)
  }
  
  #In case tp == "2.X", delta and pi should have 3 elements;
  #if (length(Delta) == 2){
  #  Delta[3] = 1
  #  Pi[3] = 0
  #}

  #Define matrix
  E.matrix		= (R.marg * (1 - Delta)) %*% t(Pi)
  diag(E.matrix) 	= diag(mx)

  #Calculate numerator of chi-square
  chi.matrix	= (mx - E.matrix)^2
  diag(chi.matrix)= 0

  #To avoid undefinition in case of divide by 0, we will set E_{ij} = 0 to 1
  E.matrix_aux = E.matrix
  E.matrix_aux[(E.matrix == 0)] = 1

  # Calculate chi-squared
  chi.squared = sum(chi.matrix/E.matrix_aux)

  #Degree of freedoms
  df = (dim.matrix-1) * (dim.matrix - 2) - 1

  pval = stats::pchisq(chi.squared, df, lower.tail = FALSE)

  # Standard output for chisq.test is X-squared = 3.2328, df = 3, p-value = 0.3571
  res = list("X.squared" = chi.squared, "df" = df, "p.value" = pval, "E.matrix" = E.matrix)
  class(res) <- "GetGoodness"
  return(res)
}

