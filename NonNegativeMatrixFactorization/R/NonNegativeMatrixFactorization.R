##=======================================================================================
## Implementation of a Non-Negative Matrix Factorization algorithm in R
## Copyright (C) 2015  Anton Antonov
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## Written by Anton Antonov,
## ʇǝu˙oǝʇsod@ǝqnɔuouoʇuɐ,
## Windermere, Florida, USA.
##
##=======================================================================================
##
## This code has NNMF functions re-programmed from the Mathematica package
## NonNegativeMatrixFactorization.m,
## "Implementation of the Non-Negative Matrix Factorization algorithm in Mathematica",
## https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m .
##
##=======================================================================================
##
## The implementation follows the description of the hybrid algorithm
## GD-CLS (Gradient Descent with Constrained Least Squares) in the article:
##
## Shahnaz, F., Berry, M., Pauca, V., Plemmons, R., 2006.
## Document clustering using nonnegative matrix factorization. Information Processing & Management 42 (2), 373-386.
##
##=======================================================================================
##
## Version 1.0
## This package contains definitions for the application of
## Non-Negative Matrix Factorization (NNMF).
##
## The functions is based on the sparse matrix library in R.
## (The reason I wrote it is there was no R library for NNMF using sparse matrices.)
##
##=======================================================================================

#' @import Matrix
#' @import purrr
NULL


##===========================================================
## SparseMatrixQ
##===========================================================

#' Sparse matrix test.
#' @description Tests is the argument one of sparse matrix classes in
#' the base package \code{Matrix}.
#' @param object An object.
#' @return A logical value.
#' @details These class names are considered sparse matrix classes:
#' \code{names(getClass("Matrix")@subclasses)}.
#' If \code{class(object)} belongs to that list then \code{object}
#' is considered to be a "sparse matrix".
#' @export
SparseMatrixQ <- function(object) {
  nms <- names(getClass("Matrix")@subclasses)
  sum( nms %in% class(object) ) > 0
}


##===========================================================
## NNMF
##===========================================================

#' Non-Negative Matrix Factorization (NNMF).
#' @description Returns the pair of matrices {W,H} such that V = W H and
#' the number of the columns of W and the number of rows of H are k.
#' The method used is Gradient Descent with Constrained Least Squares.
#' @param V A sparse matrix.
#' @param k Rank for the factorization.
#' @param initialW Initial left factor W. If NULL a random matrix is used.
#' @param initialH Initial right factor H. If NULL a constant matrix of zeroes is used.
#' @param maxSteps Maximum steps for the iteration process
#' @param nonNegativeQ Should the factorization be non-negative or not?
#' @param epsilon Denominator offset.
#' @param regularizationParameter Regularization parameter (diagonal matrix product).
#' @param tolerance Tolerance for the factorization approximation.
#' @param profilingQ Should profiling information be printed during execution or not?
#' @return A list with named elements corresponding to the matrix factors.
#' @export
NNMF <- function( V, k,
                  initialW = NULL, initialH = NULL,
                  maxSteps = 200, nonNegativeQ = TRUE, epsilon = 10^-9., regularizationParameter = 0.01, tolerance = 0.0001, profilingQ = FALSE ) {

  ## Initial values
  nSteps <- 0
  lbd <- regularizationParameter
  m <- nrow(V)
  n <- ncol(V)

  ## Initialization
  if( is.null(initialW) ) {
    W <- matrix( runif( n = m*k, min = 0, max = 1 ), nrow = m, ncol = k )
  } else if( is.matrix(initialW) || SparseMatrixQ(initialW) ) {
    W <- initialW
  } else {
    stop( "The argument initialW is expected to be NULL, a matrix, or a sparse matrix.", call. = TRUE )
  }

  if( is.null(initialH) ) {
    H <- matrix( rep( 0, k*n ), nrow = k, ncol = n )
  } else if( is.matrix(initialH) || SparseMatrixQ(initialH) ) {
    H <- initialH
  } else {
    stop( "The argument initialH is expected to be NULL, a matrix, or a sparse matrix.", call. = TRUE )
  }

  normV <- norm( V, "F"); diffNorm <- 10 * normV;

  while ( nSteps < maxSteps && normV > 0 && ( diffNorm / normV > tolerance ) ) {

    nSteps <- nSteps + 1

    A <- t(W) %*% W + lbd * diag(k)
    W2 <- t(W)
    bMat <- W2 %*% V
    H <- solve( A, bMat )
    H <- as( H, "sparseMatrix" )

    if ( nonNegativeQ ) {
      H@x[ H@x < 0 ] <- 0
    }

    H <- Matrix::drop0(H)

    W = W * ( V %*% t(H) ) / ( W %*% ( H %*% t(H) ) + epsilon )

    diffNorm <- norm( V - W %*% H, "F" )

    if( profilingQ && ( nSteps < 100  || ( nSteps %% 100 == 0) ) ) {
      cat( "\n\tStep = ", nSteps, ", diffNorm = ", diffNorm )
    }
  }

  if ( nonNegativeQ ) {
     W@x[ W@x < 0 ] <- 0
  }

  W <- as( W, "sparseMatrix" )
  W <- Matrix::drop0(W)

  ## Sometimes colnames(H) is NULL. Here we make sure they are same as colnames(V).
  rownames(W) <- rownames(V)
  colnames(H) <- colnames(V)

  list( W = W, H = H )
}


##===========================================================
## NNMFNormalizeMatrixProduct
##===========================================================

#' Normalization of a matrix product.
#' @description Returns a pair of matrices (W1,H1) such that \code{W1 %*% H1 = W %*% H}
#' and the norms of the columns of W1 are 1 if \code{normalizeLeftQ = TRUE};
#' if \code{normalizeLeftQ = FALSE} the norms of the rows of H1 are 1.
#' @param W The right matrix factor.
#' @param H The left matrix factor.
#' @param normalizeLeftQ Is the left factor normalized?
#' @return A list with named elements corresponding to the matrix factors.
#' @export
NNMFNormalizeMatrixProduct <- function( W, H, normalizeLeftQ = TRUE ) {

  if ( normalizeLeftQ ) {

    d <- sapply( 1:ncol(W), function(i) sqrt( W[,i] %*% W[,i] ) )
    S <- Diagonal( x = d )
    dinv <- 1 / ifelse( d == 0, 1, d ); dinv[ d==0 ] = 0
    SI <- Diagonal( x = dinv )

    H2 <- S %*% H
    rownames(H2) <- rownames(H)

    W2 <- W %*% SI
    colnames(W2) <- colnames(W)

    list( W = W2, H = H2 )

  } else {

    d <- sapply( 1:nrow(H), function(i) sqrt( H[i,] %*% H[i,] ) )
    S <- Diagonal( x = d )
    dinv <- 1 / ifelse( d == 0, 1, d ); dinv[ d==0 ] = 0
    SI <- Diagonal( x = dinv )

    H2 <- SI %*% H
    rownames(H2) <- rownames(H)

    W2 <- W %*% S
    colnames(W2) <- colnames(W)

    list( W = W2, H = H2 )
  }
}


##===========================================================
## NNMFBasisVectorInterpretation
##===========================================================

#' Basis vector interpretation.
#' @description Takes the \code{n} largest coordinates of \code{vec},
#' finds the corresponding elements in a list of interpretation terms,
#' corresponding to the vector coordinates.
#' By default the interpretation terms are the names of the vector elements.
#' @param n Number of elements to be interpreted.
#' @param interpretationTerms A character vector of interpretation terms.
#' @return A numeric vector with named elements.
#' @export
NNMFBasisVectorInterpretation <- function( vec, n, interpretationTerms = NULL ) {
  if ( is.null(interpretationTerms) ) { interpretationTerms <- names(vec) }
  inds = rev( order( vec ) )[1:n]
  setNames( vec[inds], interpretationTerms[inds] )
}


##===========================================================
## ColumnCentralizeSparseMatrix
##===========================================================

#' Sparse matrix column centralization.
#' @description Centralizes a sparse matrix around the mean of the non-zero elements in each column.
#' The sparse matrix zero entries are also zeroes in the centralized matrix.
#' @param smat a sparse item-tag (document-term) matrix
#' @param centerFinder a function used to find the center of a numerical vector
#' @param spreadFinder a function to find the spread of each vector and divide its the centered elements
#' @details If the argument \code{spreadFinder} is NULL no scaling of the centered elements is done.
#' It would be nice the argument \code{spreadFinder} can take as argument also the strings
#' "interQuartile" and "standardDeviation".
#' @export
ColumnCentralizeSparseMatrix <- function( smat, centerFinder = median, spreadFinder = NULL ) {

  dtMatDF <- summary( smat )
  dtMatDF <- data.frame( i=dtMatDF$i, j=dtMatDF$j, x=dtMatDF$x )


  meanDF <-
    purrr::map_df( split( dtMatDF, dtMatDF$j ), function(df) {

      sp <- 0
      if ( !is.null(spreadFinder) ) { sp <- spreadFinder( df$x ) }

      df$x <- df$x - centerFinder( df$x )

      if ( !is.na(sp[1]) && is.numeric(sp) && sp[1] > 0 ) { df$x <- df$x / sp[1] }

      df
    } )

  dtMatCentered <- sparseMatrix( i = meanDF$i, j = meanDF$j, x = meanDF$x, dims = dim(smat) )

  rownames(dtMatCentered) <- rownames(smat)
  colnames(dtMatCentered) <- colnames(smat)

  dtMatCentered
}


##===========================================================
## RowCentralizeSparseMatrix
##===========================================================

#' Sparse matrix row centralization.
#' @description Centralizes a sparse matrix around the mean of the non-zero elements in each row
#' The zero entries are also zeroes in the centralized matrix.
#' @param smat A sparse item-tag (document-term) matrix.
#' @param centerFinder A function used to find the center of a numerical vector.
#' @param spreadFinder A function to find the spread of each vector and divide its the centered elements.
#' @return A sparse matrix.
#' @export
RowCentralizeSparseMatrix <- function( smat, centerFinder = median, spreadFinder = NULL ) {
  ## This can be done without transposing just using the appropriate splitting variable in ddply.
  ## I assume that sparse matrix transposing is fast enough.
  t( ColumnCentralizeSparseMatrix( t( smat ), centerFinder, spreadFinder ) )
}


##===========================================================
## NearestWords
##===========================================================

#' Statistical thesaurus entry calculation.
#' @description Statistical thesaurus entry calculation for a specified matrix of topics and a word.
#' @param H A matrix of topics (each row is a topic, each column is a word).
#' @param word An integer index or a pattern for a word (a column of H).
#' @param n Number of nearest neighbors to be found (size of the thesaurus entry).
#' @param fixed If TRUE word is taken as a fixed pattern; if FALSE as a regexp.
#' @return Returns a data frame with columns c("Distance", "Index", "Word").
#' @details Euclidean distance is used to find the nearest neighbors.
#' @export
NearestWords <- function( H, word, n = 20, fixed = TRUE ) {

  if( is.integer(word) || is.numeric(word) ) {
    ind <- word[[1]]
  } else {
    ind <- grep( pattern = word, x = colnames(H), fixed = fixed )
  }

  if ( length(ind) == 0 ) {
    stop( "The word argument is not found in the column names of the matrix argument.", call. = TRUE )
  }
  if ( length(ind) > 1 ) {
    warning( "More that one column name corresponds to the search word; the first match is used." )
  }
  if ( n < 1 || ncol(H) < n ) { n = ncol(H) }

  M <- H[, rep( ind[[1]], ncol(H) ) ]
  M <- M - H
  M <- M * M
  dists <- colSums( M )
  sinds <- order(dists)[1:n]
  data.frame( Distance = dists[sinds], Index = sinds, Word = colnames(H)[ sinds ], stringsAsFactors = FALSE )
}


#===========================================================
## SparseMatrixExponent
##===========================================================

#' Sparse matrix exponent
#' @description Computes the exponent of a sparse matrix.
#' @param smat Sparse matrix.
#' @param exp Exponent (a real number.)
#' @param tol Tolerance (a real number.)
#' @param ... Additional arguments for \code{\link{svd}}.
#' @details Uses SVD through \code{\link{svd}}.
#' @return Sparse matrix
#' @export
SparseMatrixExponent <- function( smat, exp = -1, tol = 10^-6, ... ) {

  if( !SparseMatrixQ(smat) ) {
    stop( "The argument smat is expected to be a sparse matrix.", call. = TRUE )
  }

  if( !is.numeric(tol) && length(tol) == 1 ) {
    stop( "The argument exp is expected to be a real number.", call. = TRUE )
  }

  m <- nrow(smat)
  n <- ncol(smat)

  if( !( m > 1 || n > 1) ) {
    stop( "The argument smat is expected not be a one-by-one matrix.", call. = TRUE )
  }

  if( is.null(tol) ){
    tol <- min(1e-6, .Machine$double.eps * max(m,n) * max(smat))
  }

  if( m >= n ) {

    #svdRes <- irlba::irlba( A = smat, nv = min(m,n)-1, nu = min(m,n)-1, tol = tol )
    svdRes <- svd( x = smat )

    keep <- which( svdRes$d > tol )

    res <- t( svdRes$u[,keep] %*% diag( svdRes$d[keep]^exp, nrow=length(keep) ) %*% t(svdRes$v[,keep]) )
  }

  if ( m < n) {
    #svdRes <- irlba::irlba( A = t(smat), nv = min(m,n)-1, nu = min(m,n)-1, tol = tol )
    svdRes <- svd( x = t(smat) )

    keep <- which(svdRes$d > tol)

    res <- svdRes$u[,keep] %*% diag(svdRes$d[keep]^exp, nrow=length(keep) ) %*% t(svdRes$v[,keep])
  }

  colnames(res) <- rownames(smat)
  rownames(res) <- colnames(smat)

  return(res)
}


#===========================================================
## SparseMatrixPseudoInverse
##===========================================================

#' Sparse matrix pseudo inverse
#' @description Computes the pseudo inverse of a sparse matrix.
#' @param smat Sparse matrix.
#' @param tol Tolerance (a real number.)
#' @param ... Additional arguments for \code{\link{svd}}.
#' @details Uses SVD through \code{\link{svd}}.
#' @return Sparse matrix
#' @export
SparseMatrixPseudoInverse <- function( smat, tol = 10^-6, ... ) {

  if( !SparseMatrixQ(smat) ) {
    stop( "The first argument is expected to be a sparse matrix.", call. = TRUE )
  }

  SparseMatrixExponent( smat = smat, exp = -1, tol = tol, ... )
}
