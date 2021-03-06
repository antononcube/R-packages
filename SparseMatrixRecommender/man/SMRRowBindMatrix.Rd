% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SparseMatrixRecommender.R
\name{SMRRowBindMatrix}
\alias{SMRRowBindMatrix}
\title{Row bind a matrix with a sparse matrix recommender}
\usage{
SMRRowBindMatrix(smr, smat)
}
\arguments{
\item{smr}{A sparse matrix recommender.}

\item{smat}{A sparse matrix.}
}
\value{
A sparse matrix recommender.
}
\description{
Row-binds the matrix of a sparse matrix recommender with a sparse matrix.
}
\seealso{
Other SMR modification functions: 
\code{\link{SMRAnnexSubMatrix}()},
\code{\link{SMRJoin}()}
}
\concept{SMR modification functions}
