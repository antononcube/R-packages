context("Sparse matrix Euclidean distances")
library(SparseMatrixRecommender)

# Create an SMR in order to get the matrix.

smr <- SMRCreate( dfMushroom, itemColumnName = "id" )

smat <- smr$M

rsmat <- smat
rsmat@x <- runif( n = length(rsmat@x), min = -10, max = 10 )

## Distances with standard computations.
mvec <- colMeans(smat)
mvecMat <- matrix( rep( mvec,  nrow(smat) ), byrow = T, nrow = nrow(smat) )
dMat <- smat - mvecMat
dists0 <- sqrt(rowSums(dMat * dMat))

mvec <- colMeans(rsmat)
mvecMat <- matrix( rep( mvec,  nrow(rsmat) ), byrow = T, nrow = nrow(rsmat) )
dMat <- rsmat - mvecMat
rdists0 <- sqrt(rowSums(dMat * dMat))

## Distance with sparse(r) computations.
dists1 <- SMREuclideanDistances( smat = smat, vec = colMeans(smat) )

rdists1 <- SMREuclideanDistances( smat = rsmat, vec = colMeans(rsmat) )

## Tests

test_that("Expected structures", {
  
  expect_is( smr, "SMR" )
  expect_is( smat, "dgCMatrix" )
  
  expect_is( dists0, "numeric" )
  expect_is( rdists0, "numeric" )
  
  expect_is( dists1, "numeric" )
  expect_is( rdists1, "numeric" )
  
})

test_that("Expected equivalences", {

  expect_true( mean( abs( dists0 - dists1 ) < 10^-12 ) == 1 )
  
  expect_true( mean( abs( rdists0 - rdists1 ) < 10^-12 ) == 1 )
  
})
