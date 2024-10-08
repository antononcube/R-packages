% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LSAMon.R
\name{LSAMonExtractStatisticalThesaurus}
\alias{LSAMonExtractStatisticalThesaurus}
\title{Statistical thesaurus derivation.}
\usage{
LSAMonExtractStatisticalThesaurus(
  lsaObj,
  searchWords,
  numberOfNearestNeighbors = 12,
  fixedQ = TRUE
)
}
\arguments{
\item{lsaObj}{A LSAMon object.}

\item{searchWords}{A character vector with words to find statistical
thesauri for. The words can be patterns.}

\item{numberOfNearestNeighbors}{Number of words in each thesaurus entry.}

\item{fixedQ}{Should \code{searchWords} be considered fixed or pattern strings?}
}
\value{
A LSAMon object.
}
\description{
Derive a statistical thesaurus for specified words.
}
\details{
This function calls
\code{\link{NonNegativeMatrixFactorization::NearestWords}}.
The obtained list of thesaurus entries is assigned to \code{lsaObj$Value}.
}
