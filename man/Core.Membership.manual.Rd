% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModulonSat_functions.R
\name{Core.Membership.manual}
\alias{Core.Membership.manual}
\title{Calculate modulon regulatory core membership}
\usage{
Core.Membership.manual(data, mod, core)
}
\arguments{
\item{data}{Matrix of dataframe with regulon activity.}

\item{mod}{A character vector with a given modulon constituent elements.}

\item{core}{A character vector with a given modulon regulatory core constituent elements}
}
\value{
List object with as many elements as modulons. Each element of the list contains a character vector with the corresponding modulon constituent elements.
}
\description{
Calculation of the correlation (Spearman) between the first principal component (PC1) of a subset of the regulon activity matrix with only modulon regulatory core constituent elements and each of these elements.
}
\details{
This function subsets the regulon activity matrix to include only a given modulon regulatory core constituent elements. After centering and scaling the resulting matrix, a PCA is performed; the PC1 is then correlated with the regulon activity of each modulon constituent element.
}
\examples{
\dontrun{
if(interactive()){
Core.Membership.manual(data = RegAUC.TILs, mod = modulons.TILs[['3']],core=cc.TILs[['3']][['cc.3']])
}
}
}
