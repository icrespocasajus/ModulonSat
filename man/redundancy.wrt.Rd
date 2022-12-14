% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModulonSat_functions.R
\name{redundancy.wrt}
\alias{redundancy.wrt}
\title{Redundancy with respect to a Reference
Calculate redundancy of a vector with respect to another vector.}
\usage{
redundancy.wrt(a, b)
}
\arguments{
\item{a}{A query vector}

\item{b}{A reference vector}
}
\value{
Redundancy calculated as the ratio between the intersection and the length of the reference vector
}
\description{
Redundancy with respect to a Reference
Calculate redundancy of a vector with respect to another vector.
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
redundancy(c('A','B','C'),c('A','B','C','D','E'))
 }
}
}
