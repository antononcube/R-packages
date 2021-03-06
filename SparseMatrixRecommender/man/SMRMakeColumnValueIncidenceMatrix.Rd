% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SparseMatrixRecommender.R
\name{SMRMakeColumnValueIncidenceMatrix}
\alias{SMRMakeColumnValueIncidenceMatrix}
\title{Converts an integer sparse matrix to incidence matrix.}
\usage{
SMRMakeColumnValueIncidenceMatrix(mat, rowNamesQ = TRUE, colNamesQ = TRUE)
}
\arguments{
\item{mat}{an integer matrix to be converted to column value incidence matrix.}

\item{rowNamesQ}{Should the argument row names be the result matrix row names?}

\item{colNamesQ}{Should the column names derived from the argument matrix 
column names be assigned to the result matrix?}
}
\description{
Replaces each a column of a integer matrix with number of columns
corresponding to the integer values.
The matrix [[2,3],[1,2]] is converted to [[0,0,1,0,0,0,0,1],[0,1,0,0,0,0,1,0]] .
}
