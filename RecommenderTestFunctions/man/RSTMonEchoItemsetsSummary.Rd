% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSTMon.R
\name{RSTMonEchoItemsetsSummary}
\alias{RSTMonEchoItemsetsSummary}
\title{Echo itemsets summary.}
\usage{
RSTMonEchoItemsetsSummary(
  rstObj,
  f = function(x) summary(as.data.frame(unclass(x), stringsAsFactors = T))
)
}
\arguments{
\item{rstObj}{An RSTMon object.}

\item{f}{Summarization function.}
}
\value{
A RSTMon object.
}
\description{
Echoes itemsets dimensions and data frame summary.
}
