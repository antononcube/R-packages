% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HIRMon.R
\name{HIRMonRecommend}
\alias{HIRMonRecommend}
\title{Take Items.}
\usage{
HIRMonRecommend(hirObj, profile, nrecs = 12, alpha = 0.8, ...)
}
\arguments{
\item{hirObj}{An HIRMon object.}

\item{profile}{Profile specification.
A data frame with columns \code{c("Score", "Tag")};
a numeric vector named elements, the names being tags;
a character vector, the correspond scores assumed all to be 1.}

\item{nrecs}{Number of recommendations.
If NULL all non-zero scores recommendations are returned.}

\item{alpha}{The significance of \code{hirObj$M} in the Power Method iteration formula.}

\item{...}{Additional arguments for \code{\link{PowerMethod}}.}
}
\value{
A HIRMon object.
}
\description{
Takes Items from the monad object.
}
\concept{Recommender functions}
