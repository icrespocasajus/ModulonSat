% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModulonSat_functions.R
\name{Find.Sat}
\alias{Find.Sat}
\title{Find satellite transcription factors}
\usage{
Find.Sat(data, feature = "Redundancy", threshold = 0, quant.prob = NULL)
}
\arguments{
\item{data}{A list object where each element contains a dataframe  including the 'Similarity','Redundancy','Overlap' values of the modulon constituent elements not included within a given modulon connected component.}

\item{feature}{Feature to be considered for the satellite detection. Possible values: c('Similarity','Redundancy','Overlap').}

\item{threshold}{Cutoff value for either similarity, redundancy or overlap (number of common targets).}
}
\value{
List object with as many elements as the modulon connected components provided as the input. Each element of the list contains a character vector with the transcription factors above the threshold for the feature considered.
}
\description{
Find, among the constituent elements of a given modulon, transcription factors sharing targets with a modulon connected component above a given threshold of similarity, redundancy or overlap (see jaccard(),redundancy.wrt() and overlap() functions)
}
\details{
This function collects the modulon constituent elements sharing targets with a given modulon above a given threshold of similarity, redundancy or overlap.
}
\examples{
\dontrun{
if(interactive()){
target.analysis.modulon.wrt.cc.manual.query(net = network.TILs,
mod = modulons.TILs,
cc = cc.TILs,
query.mod = '3',
query.cc= 'cc.3') \%>\% Find.Sat()
}
}
}
