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
dists1 <- SMRMatrixEuclideanDistances( smat = smat, vec = colMeans(smat) )

rdists1 <- SMRMatrixEuclideanDistances( smat = rsmat, vec = colMeans(rsmat) )

smr2 <- SMRCreate( dfTitanic, itemColumnName = "id" )

dfDists <- SMREuclideanDistances( smr2, tagType = "passengerClass" )

dfAllDists <- SMREuclideanDistances( smr2, tagType = NULL )

## Tests

test_that("Expected structures", {
  
  expect_is( smr, "SMR" )
  expect_is( smat, "dgCMatrix" )
  
  expect_is( dists0, "numeric" )
  expect_is( rdists0, "numeric" )
  
  expect_is( dists1, "numeric" )
  expect_is( rdists1, "numeric" )
  
  expect_is( dfDists, "data.frame" )
  expect_is( dfAllDists, "data.frame" )
  
})

test_that("Expected equivalences", {

  expect_true( mean( abs( dists0 - dists1 ) < 10^-12 ) == 1 )
  
  expect_true( mean( abs( rdists0 - rdists1 ) < 10^-12 ) == 1 )
  
})

test_that("Expected shapes", {
  
  expect_equal( names(dfDists), c("TagType", "Tag", smr$ItemColumnName, "Index", "Distance" ) )
  
  expect_equal( names(dfAllDists), c("TagType", "Tag", smr$ItemColumnName, "Index", "Distance" ) )
  
  expect_true( mean( smr2$TagTypes %in% dfAllDists$TagType ) == 1 )
})