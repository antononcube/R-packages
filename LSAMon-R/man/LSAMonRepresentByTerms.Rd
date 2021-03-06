% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LSAMon.R
\name{LSAMonRepresentByTerms}
\alias{LSAMonRepresentByTerms}
\title{Represent by terms.}
\usage{
LSAMonRepresentByTerms(lsaObj, query, applyTermWeightFunctionsQ = TRUE)
}
\arguments{
\item{lsaObj}{A LSAMon object.}

\item{query}{A character vector or a sparse matrix to be represented
in the space of monad's document-term matrix.}

\item{applyTermWeightFunctionsQ}{Should the weight term functions
be applied to the result matrix or not?}
}
\value{
A LSAMon object.
}
\description{
Find the terms representation of a matrix or a document.
}
\details{
This function applies the global weights computed in the
function \code{\link{LSAMonApplyTermWeightFunctions}} and stored in
the monad object together with the names of the local weight function,
and the normalizer function.
}
