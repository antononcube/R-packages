% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JSONConversionFunctions.R
\name{GetRandomIdentifiers}
\alias{GetRandomIdentifiers}
\title{Get random identifiers}
\usage{
GetRandomIdentifiers(n, length = 12, pattern = "[A-Za-z0-9]", prefix = "")
}
\arguments{
\item{n}{Number of identifiers to be generated.}

\item{length}{Number of characters per identifier.}

\item{pattern}{Character vector specifying character classes to draw elements from;
see \code{\link{stringi-search-charclass}}.}

\item{prefix}{A prefix string.}
}
\value{
A character vector
}
\description{
Makes a character vector with random unique string of specified length.
}
\details{
Uses internally \code{\link{stringi::stri_rand_strings}}.
}
