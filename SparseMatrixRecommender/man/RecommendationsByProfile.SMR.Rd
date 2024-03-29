% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SparseMatrixRecommender.R
\name{RecommendationsByProfile.SMR}
\alias{RecommendationsByProfile.SMR}
\title{Specialization of \code{RecommendationsByProfile} for SMR objects}
\usage{
\method{RecommendationsByProfile}{SMR}(x, profileTags, profileTagScores, nrecs, ...)
}
\arguments{
\item{x}{A recommender object.}

\item{profileTags}{A list of profile tags.}

\item{profileTagScores}{A list of scores corresponding to the profile tags.}

\item{nrecs}{Number of required recommendations.}
}
\value{
A data frame with the columns columns c("Score", "Item").
}
\description{
Specialization of \code{RecommendationsByProfile} for SMR objects.
}
