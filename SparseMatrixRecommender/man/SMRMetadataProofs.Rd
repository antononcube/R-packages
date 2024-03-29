% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SparseMatrixRecommender.R
\name{SMRMetadataProofs}
\alias{SMRMetadataProofs}
\title{Metadata proofs}
\usage{
SMRMetadataProofs(
  smr,
  toBeLovedItem,
  profile,
  normalizeScores = TRUE,
  style = "intersection"
)
}
\arguments{
\item{smr}{A sparse matrix recommender.}

\item{toBeLovedItem}{an ID of a item or its index in \code{smr$M}.}

\item{profile}{a data frame that is the profile of the customer with columns "Score", "Index", "Tag".}

\item{normalizeScores}{Should the scores be normalized with \code{max(res$Score)}.}

\item{style}{Proof style derivation; one of "intersection", "multiplication".}
}
\value{
A data frame with columns names "Score", "Index", "Tag".
}
\description{
Find the metadata tags that would explain or justify the recommendations.
}
\seealso{
Other Proof functions: 
\code{\link{SMRHistoryProofs}()}
}
\concept{Proof functions}
