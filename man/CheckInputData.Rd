% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CheckInputData.R
\name{CheckInputData}
\alias{CheckInputData}
\title{Check Input Matrix function}
\usage{
CheckInputData(datatable)
}
\arguments{
\item{datatable}{Matrix. Expected to be square matrix with at least 2 rows (columns), non negative values and at least an element different of zero.}
}
\description{
This function checks that matrix introduced is as expected. Should be a matrix, squared, with a dimension greater or equal to two, without negative entries and at least an entry  different of 0.
}
\examples{
CheckInputData(matrix(c(1,2,3,4),2,2))
}
\keyword{check}
\keyword{datatable}
