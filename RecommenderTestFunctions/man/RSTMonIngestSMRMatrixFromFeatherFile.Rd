% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSTMon.R
\name{RSTMonIngestSMRMatrixFromFeatherFile}
\alias{RSTMonIngestSMRMatrixFromFeatherFile}
\title{IngestSMRMatrixFromFeatherFile.}
\usage{
RSTMonIngestSMRMatrixFromFeatherFile(rstObj, fileName, ...)
}
\arguments{
\item{rstObj}{An RSTMon object.}

\item{fileName}{A string that is file name.}

\item{...}{Additional parameters for \code{\link{SMRCreateFromLongForm}}.}
}
\value{
An RSTMon object or \code{RSTMonFailureSymbol}.
}
\description{
Ingests the SMR matrix data in a given feather file.
}
\details{
The obtained recommender object is assigned to \code{rstObj$SMR}.
}
\seealso{
Other Ingest functions: 
\code{\link{RSTMonIngestSMRMatrixFromCSVFile}()}
}
\concept{Ingest functions}
