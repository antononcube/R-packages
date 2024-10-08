% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SparseMatrixRecommender.R
\name{SMRAnnexSubMatrix}
\alias{SMRAnnexSubMatrix}
\title{Annex a sub-matrix}
\usage{
SMRAnnexSubMatrix(
  smr,
  newSubMat,
  newTagType,
  imposeSameRowNamesQ = TRUE,
  addTagTypesToColumnNamesQ = FALSE,
  sep = ":"
)
}
\arguments{
\item{smr}{A sparse matrix recommender.}

\item{newSubMat}{The new sub-matrix to be annexed.}

\item{newTagType}{The tag type associated with the new sub-matrix.}

\item{imposeSameRowNamesQ}{Should the row names of \code{smr$M} be imposed on the matrix to be annexed?}

\item{addTagTypesToColumnNamesQ}{Should the tag type \code{newTagType} be added as a prefix
to the column names of the matrix to be annexed?}

\item{sep}{Separator for the prefixes of the columns names.}
}
\value{
Sparse matrix recommender.
}
\description{
Annex a sub-matrix to the metadata matrix of an SMR object.
}
\seealso{
Other SMR modification functions: 
\code{\link{SMRJoin}()},
\code{\link{SMRRowBindMatrix}()}
}
\concept{SMR modification functions}
