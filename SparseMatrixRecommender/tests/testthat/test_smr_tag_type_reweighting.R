context("Sparse matrix recommender tag types re-weighting")
library(SparseMatrixRecommender)

## When creating the submatrices SMRCreateFromSpecificaiton
## sorts the obtained names (because it uses split.)
sMatNames <- sort(colnames(dfTitanic)[-1])

## Create SMR directly from data

smr <- SMRCreate( dfTitanic[, c("id", sMatNames)], 
                   tagTypes = sMatNames, 
                   itemColumnName = "id" )


rwMat1 <- SMRApplyTagTypeWeights( smr = smr, weights = c( 2, 10, 1, 2) )

rwMat2 <- SMRApplyTagTypeWeights( smr = smr, weights = c( passengerClass = 10, passengerAge = 2, passengerSurvival = 2, passengerSex = 1 ) )

rwMat3 <- SMRApplyTagTypeWeights( smr = smr, weights = c( passengerClass = 10, passengerAge = 2, passengerSurvival = 2) )

rwMat4 <- SMRApplyTagTypeWeights( smr = smr, weights = c( 2, 10, 2, 10 ) )

rwMat5 <- SMRApplyTagTypeWeights( smr = smr, weights = c( 2, 10 ) )

rwMat6 <- SMRApplyTagTypeWeights( smr = smr, weights = c( 2, 10, 2, 10, 130) )

## Tests

test_that("Expected objects", {
  expect_is( smr, "SMR" )
  
  expect_equal( names(smr), c("M", "M01", "TagTypeRanges","TagTypes",      
                               "ItemColumnName", "TagToIndexRules", "ItemToIndexRules") )
  
  expect_is( smr$M, "dgCMatrix" ) 
  
  expect_is( rwMat1, "dgCMatrix" ) 
  expect_is( rwMat2, "dgCMatrix" ) 
  expect_is( rwMat3, "dgCMatrix" ) 
  expect_is( rwMat4, "dgCMatrix" ) 
  expect_is( rwMat5, "dgCMatrix" ) 
}) 

test_that("Same matrices with unnamed tag type weights and named ones", {
  expect_true( norm(as.matrix( rwMat1 - rwMat2 ) ) < 1E-14 )
  expect_true( norm(as.matrix( rwMat1 - rwMat3 ) ) < 1E-14 )
}) 

test_that("Same matrices with just and too short unnamed tag type weights vactor", {
  expect_true( norm(as.matrix( rwMat4 - rwMat5 ) ) < 1E-14 )
  expect_true( norm(as.matrix( rwMat4 - rwMat6 ) ) < 1E-14 )
}) 
