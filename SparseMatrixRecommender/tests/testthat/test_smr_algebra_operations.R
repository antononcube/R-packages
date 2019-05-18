context("Sparse matrix recommender algebra operations")
library(SparseMatrixRecommender)

## The creations here are tested/taken from the test file:
##   test_different_creations_same_matrices.R

## When creating the submatrices SMRCreateFromSpecificaiton
## sorts the obtained names (because it uses split.)
sMatNames <- sort(colnames(dfTitanic)[-1])

## Create SMR directly from data.

smr0 <- SMRCreate( dfTitanic[, c("id", sMatNames)], 
                   tagTypes = sMatNames, 
                   itemColumnName = "id" )

## Separate ID's.

smr1IDs <- sample( dfTitanic$id, floor( nrow(dfTitanic) * 0.65 ) )
smr2IDs <- setdiff( dfTitanic$id, smr1IDs )
smr2IDs <- c(smr2IDs, sample(smr1IDs, floor(length(smr1IDs) * 0.2 ) ) )
  
smr1 <- SMRCreate( dfTitanic[ dfTitanic$id %in% smr1IDs, c("id", sMatNames)], 
                   tagTypes = sMatNames[1:2], 
                   itemColumnName = "id" )

smr2 <- SMRCreate( dfTitanic[ dfTitanic$id %in% smr2IDs, c("id", sMatNames)], 
                   tagTypes = sMatNames[3:4], 
                   itemColumnName = "id" )

smr2full <- SMRCreate( dfTitanic[ dfTitanic$id %in% smr2IDs, c("id", sMatNames)], 
                   tagTypes = sMatNames, 
                   itemColumnName = "id" )

smr3inner <- SMRJoin( smr1 = smr1, smr2 = smr2, joinType = "inner" )

smr3left <- SMRJoin( smr1 = smr1, smr2 = smr2, joinType = "left" )

smr3outer <- SMRJoin( smr1 = smr1, smr2 = smr2, joinType = "outer" )


## Tests

test_that("Expected objects", {
  expect_is( smr0, "SMR" )
  expect_is( smr1, "SMR" )
  expect_is( smr2, "SMR" )
  expect_is( smr2full, "SMR" )  
})

test_that("SMRJoin using inner type", {
  expect_true( mean( sort(rownames(smr3inner$M)) == sort( intersect( rownames(smr1$M), rownames(smr2$M) ) ) ) == 1 )
})

test_that("SMRJoin using left type", {
  expect_true( mean( sort(rownames(smr3left$M)) == sort(rownames(smr1$M)) ) == 1 )
})

test_that("SMRJoin using outer type", {
  expect_true( mean( sort(rownames(smr3outer$M)) == sort(rownames(smr0$M)) ) == 1 )
})

test_that("SMRJoin using outer type of non-disjoined SMR tag types", {
  expect_error( SMRJoin( smr1 = smr1, smr2 = smr2, joinType = "same" ) )
  expect_error( SMRJoin( smr1 = smr1, smr2 = smr2full, joinType = "outer" ) )
})