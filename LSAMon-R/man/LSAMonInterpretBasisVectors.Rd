% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LSAMon.R
\name{LSAMonInterpretBasisVectors}
\alias{LSAMonInterpretBasisVectors}
\title{Basis vectors interpretation.}
\usage{
LSAMonInterpretBasisVectors(
  lsaObj,
  basisVectorIndexes = NULL,
  numberOfTerms = 12,
  orderBySignificanceQ = TRUE
)
}
\arguments{
\item{lsaObj}{A LSAMon object.}

\item{basisVectorIndexes}{Basis vectors to be interpreted.
If NULL all indexes are taken.}

\item{numberOfTerms}{Number of terms per topic.
(I.e. number of top coordinates per basis vector.)}

\item{orderBySignificanceQ}{Should the basis vectors be ordered by their significance?}
}
\value{
A LSAMon object.
}
\description{
Interpret specified basis vectors.
}
\details{
This function is based on
\code{\link{NonNegativeMatrixFactorization::NNMFNormalizeMatrixProduct}} and
\code{\link{NonNegativeMatrixFactorization::NNMFBasisVectorInterpretation}}.
The obtained long form data frame is assigned to \code{lsaObj$Value}.
Wide form can be produced with
\code{reshape2::dcast( data = lsaObj$Value, formula = TermRank ~ TopicRank, value.var = "Term" )}.
(Note that some columns are dropped.)
}
