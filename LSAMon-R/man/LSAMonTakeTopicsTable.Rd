% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LSAMon.R
\name{LSAMonTakeTopicsTable}
\alias{LSAMonTakeTopicsTable}
\title{Take topics table.}
\usage{
LSAMonTakeTopicsTable(
  lsaObj,
  numberOfTerms = 12,
  wideFormQ = FALSE,
  numberOfTableColumns = NA
)
}
\arguments{
\item{lsaObj}{A LSAMon object.}

\item{numberOfTerms}{Number of terms per topic.}

\item{wideFormQ}{Should the topics table be in wide form or not?}

\item{numberOfTableColumns}{Number of table columns.
(Dummy argument at this point.)}

\item{echoQ}{Should the result be echoed or not?}
}
\value{
A LSAMon object.
}
\description{
Takes a table of (the calculated) topics.
}
