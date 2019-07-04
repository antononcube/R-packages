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
dists1 <- SMRMatrixDistances( smat = smat, vec = colMeans(smat), method = "euclidean" )

rdists1 <- SMRMatrixDistances( smat = rsmat, vec = colMeans(rsmat), method = "euclidean" )

csmat <- Diagonal( x = 1 / sqrt( rowSums( smat * smat ) ) ) %*% smat
cvec <- colMeans(smat) 
cvec <- cvec / sqrt( sum(cvec * cvec) )
cdists0 <- ( 1 - csmat %*% cvec )[,1]

cdists1 <- SMRMatrixDistances( smat = smat, vec = colMeans(smat), method = "cosine" )

smr2 <- SMRCreate( dfTitanic, itemColumnName = "id" )

dfDists <- SMRDistances( smr2, tagType = "passengerClass", method = "euclidean" )

dfAllDists <- SMRDistances( smr2, tagType = NULL, method = "euclidean" )

## Tests

test_that("Expected structures", {
  
  expect_is( smr, "SMR" )
  expect_is( smat, "dgCMatrix" )
  
  expect_is( dists0, "numeric" )
  expect_is( rdists0, "numeric" )
  
  expect_is( dists1, "numeric" )
  expect_is( rdists1, "numeric" )

  expect_true( length(dists1) == nrow(smat) ) 
  expect_true( length(rdists1) == nrow(rsmat) ) 
  
  expect_is( cdists0, "numeric" )
  expect_is( cdists1, "numeric" )
  expect_true( length(cdists1) == nrow(smat) ) 
  expect_true( length(cdists1) == length(cdists0) ) 
  
  
  expect_is( dfDists, "data.frame" )
  expect_is( dfAllDists, "data.frame" )
  
})

test_that("Expected equivalences", {

  expect_true( mean( abs( dists0 - dists1 ) < 10^-12 ) == 1 )
  
  expect_true( mean( abs( rdists0 - rdists1 ) < 10^-12 ) == 1 )
  
  expect_true( mean( abs( cdists0 - cdists1 ) < 10^-12 ) == 1 )
  
})

test_that("Expected shapes", {
  
  expect_equal( names(dfDists), c("TagType", "Tag", smr$ItemColumnName, "Index", "Distance" ) )
  
  expect_equal( names(dfAllDists), c("TagType", "Tag", smr$ItemColumnName, "Index", "Distance" ) )
  
  expect_true( mean( smr2$TagTypes %in% dfAllDists$TagType ) == 1 )
})