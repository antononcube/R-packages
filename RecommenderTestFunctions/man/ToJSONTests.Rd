% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JSONConversionFunctions.R
\name{ToJSONTests}
\alias{ToJSONTests}
\title{Convert data frame to JSON test specifications}
\usage{
ToJSONTests(
  dfTests,
  minIntersectionCount,
  minCorrelation = 0.7,
  correlationMethod = "Pearson",
  testType = "Comparison",
  recommenderType = "ItemRecommender",
  testIdentifierColumns = c("Domain", "QueryTagType", "Query"),
  collapseQ = FALSE
)
}
\arguments{
\item{dfTests}{A data frame with tests.}

\item{minIntersectionCount}{Number for the expected min intersection counts per test.}

\item{minCorrelation}{Number for the expected min correlation per test.}

\item{correlationMethod}{String for the correlation method.
It is expected to be one if "Pearson", "Spearman", or "Kendall",
but it is not enforced.}

\item{testType}{String for the test type.
It is expected to be one "ComparisonTest" or "ConditionTest",
but that is not enforced.}

\item{recommenderType}{String for the recommender type.
It is expected to be one of "MetadataRecommender" or "ItemRecommender",
but it is not enforced.}

\item{testIdentifierColumns}{A character vector with the column names that
the unique values of which correspond to unique tests.}
}
\value{
A JSON character vector
}
\description{
Converts a given data frame to JSON test specifications.
}
