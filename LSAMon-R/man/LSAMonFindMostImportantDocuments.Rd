% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LSAMon.R
\name{LSAMonFindMostImportantDocuments}
\alias{LSAMonFindMostImportantDocuments}
\title{Finds most important documents.}
\usage{
LSAMonFindMostImportantDocuments(lsaObj, nTop = 5)
}
\arguments{
\item{lsaObj}{A LSAMon object}

\item{nTop}{The number of top most important texts.}
}
\value{
A LSAMon object
}
\description{
Finds the most important documents in the monad.
}
\details{
The result is a data frame with columns \code{c("Score", "ID")};
it is assigned to \code{lsaObj$Value}.
}
