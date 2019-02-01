context("Sparse matrix recommender creation")
library(SparseMatrixRecommender)


## Create SMR directly from data

smr1 <- SMRCreate( dfTitanic, 
                   tagTypes = setdiff( colnames(dfTitanic), "id" ), 
                   itemColumnName = "id" )

## Create SMR with matrices

sMatNames <- colnames(dfTitanic)[-1]
sMats <- purrr::map( sMatNames, function(x) { xtabs( as.formula( paste0("~ id + ", x) ), dfTitanic, sparse = TRUE ) } )
names(sMats) <- sMatNames

smr2 <- SMRCreateFromMatrices( matrices = sMats, itemColumnName = "id" )

test_that("SMR objects", {
  expect_is( smr1, "SMR" )
  expect_is( smr2, "SMR" )
  
  expect_equal( names(smr1), c("M", "M01", "TagTypeRanges","TagTypes",      
                               "ItemColumnName", "TagToIndexRules", "ItemToIndexRules") )
  expect_equal( names(smr1), names(smr2) )
  
  expect_is( smr1$M, "dgCMatrix" ) 
  expect_is( smr2$M, "dgCMatrix" ) 
}) 

test_that("Same (sub-)matrices", {
  expect_true( norm(as.matrix( smr1$M - smr1$M ) ) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerClass" ) - SMRSubMatrix( smr2, "passengerClass" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerAge" ) - SMRSubMatrix( smr2, "passengerAge" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerSex" ) - SMRSubMatrix( smr2, "passengerSex" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerSurvival" ) - SMRSubMatrix( smr2, "passengerSurvival" ) )) < 1E-14 )
}) 
