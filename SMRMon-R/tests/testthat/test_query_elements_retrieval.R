context("Query elements retrieval")
library(SMRMon)
library(SparseMatrixRecommender)

smrObj <-
  SMRMonUnit( data = dfTitanic ) %>%
  SMRMonCreate( itemColumnName = "id" )

## No sanity check tests.
## We assume the sanity checks are done in the other files.

## Test data shapes of long form.
test_that("Expected identifiers presence", {

  res <-
    smrObj %>%
    SMRMonRetrieveByQueryElements( should = c("1st", "2nd"), must = c("male"), mustNot = c("40")  ) %>%
    SMRMonTakeValue

  res2 <-
    dfTitanic %>%
    dplyr::filter( passengerSex == "male" & passengerAge != 40 )


  expect_equal( length( intersect( res$id, res2$id ) ), length( res$id ) )

  expect_equal( length( intersect( res$id, res2$id ) ), length( res2$id ) )

  expect_true( mean( res$id %in% res2$id ) == 1 )

  expect_true( mean( res2$id %in% res$id ) == 1 )

  expect_true( length( setdiff( res2$id, res$id ) ) == 0 )

  expect_true( length( setdiff( res$id, res2$id ) ) == 0 )

})

