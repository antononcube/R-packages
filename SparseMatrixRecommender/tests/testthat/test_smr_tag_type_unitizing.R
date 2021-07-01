context("Sparse matrix recommender tag types unitizing")
library(SparseMatrixRecommender)

## When creating the submatrices SMRCreateFromSpecificaiton
## sorts the obtained names (because it uses split.)
sMatNames <- sort(colnames(dfTitanic)[-1])

## Create SMR directly from data

smr <- SMRCreate( dfTitanic[, c("id", sMatNames)], 
                   tagTypes = sMatNames, 
                   itemColumnName = "id" )

smr$M <- SMRApplyTermWeightFunctions( docTermMat = smr$M, globalWeightFunction = "IDF", "None", "None")


smr2 <- SMRCreate( dfTitanic[, c("id", sMatNames)], 
                  tagTypes = sMatNames, 
                  itemColumnName = "id" )

smr2$M <- SMRApplyTermWeightFunctions( docTermMat = smr2$M, globalWeightFunction = "None", "Binary", "None")


## Tests

test_that("Expected objects", {
  expect_is( smr, "SMR" )
  expect_is( smr$M, "dgCMatrix" ) 
  
  expect_is( smr2, "SMR" )
  expect_is( smr2$M, "dgCMatrix" ) 
  
  expect_true( max(abs(smr2$M@x)) == 1 )
}) 

test_that("Same unitized matrices", {
  smr3 <- SMRUnitizeSubMatrices( smr = smr, tagTypes = NULL, tol = 0)
  
  expect_is( smr3, "SMR" )
  
  expect_true( norm(as.matrix( smr3$M - smr2$M ) ) < 1E-14 )
  
}) 

test_that("Not unitizing all matrices", {
  smr4 <- SMRUnitizeSubMatrices( smr = smr, tagTypes = c("passengerSex", "passengerClass"), tol = 0)
  
  expect_is( smr4, "SMR" )
  
  expect_true( norm(as.matrix( SMRSubMatrix(smr = smr4, tagType = "passengerSex") - SMRSubMatrix(smr = smr2, tagType = "passengerSex") ) ) < 1E-14 )
  
  expect_true( norm(as.matrix( SMRSubMatrix(smr = smr4, tagType = "passengerClass") - SMRSubMatrix(smr = smr2, tagType = "passengerClass") ) ) < 1E-14 )

  expect_true( norm(as.matrix( SMRSubMatrix(smr = smr4, tagType = "passengerAge") - SMRSubMatrix(smr = smr2, tagType = "passengerAge") ) ) > 1 )
  
}) 