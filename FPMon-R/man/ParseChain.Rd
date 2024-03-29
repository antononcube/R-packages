% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FunctionalParsers.R
\name{ParseChain}
\alias{ParseChain}
\title{Parsing a chain.}
\usage{
ParseChain(p, sepParser, right = FALSE)
}
\arguments{
\item{p}{A parser.}

\item{sepParser}{A separator parser.}

\item{right}{A logical; gives the direction in which the chain is constructed.}
}
\value{
A function that is a parser.
}
\description{
Parsing a chain over a certain operation.
}
\seealso{
Other Next parsers.: \code{\link{ParseChainLeft}},
  \code{\link{ParseChainRight}}, \code{\link{ParseListOf}},
  \code{\link{ParseMany1}}, \code{\link{ParseMany}},
  \code{\link{ParseOption1}}, \code{\link{ParseOption}}
}
\concept{Next parsers.}
