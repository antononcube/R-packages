context("Basic pipelines with history recommendations")
library(SMRMon)
library(SparseMatrixRecommender)

smrObj <-
  SMRMonUnit( data = dfTitanic ) %>%
  SMRMonCreate( itemColumnName = "id")

## Tests
test_that("Sanity check", {
  expect_s3_class( smrObj, "SMR" )
  expect_true( length( intersect( names(smrObj) ,
                                  c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data") ) ) == 8 )
})

## Half of these tests are repeating the tests in "test_recommendations_by_history.R" .
## Recommend by history data frame.
dfRecs <-
  smrObj %>%
  SMRMonRecommend( history = data.frame( Rating = 1, Item = dfTitanic$id[1:3], stringsAsFactors = F), nrecs = 20 ) %>%
  SMRMonTakeValue

dfRecsG <-
  smrObj %>%
  SMRMonGetTopRecommendations( spec = data.frame( Rating = 1, Item = dfTitanic$id[1:3], stringsAsFactors = F), nrecs = 20 ) %>%
  SMRMonTakeValue

## Recommend by history character vector.
chRecs <-
  smrObj %>%
  SMRMonRecommend( history = dfTitanic$id[1:3], nrecs = 20 ) %>%
  SMRMonTakeValue

chRecsG <-
  smrObj %>%
  SMRMonGetTopRecommendations( spec = dfTitanic$id[1:3], nrecs = 20 ) %>%
  SMRMonTakeValue

## Recommend by history numeric vector with named elements.
nvRecs <-
  smrObj %>%
  SMRMonRecommend( history = setNames( 1:3, dfTitanic$id[1:3]), nrecs = 20 ) %>%
  SMRMonTakeValue

nvRecsG <-
  smrObj %>%
  SMRMonGetTopRecommendations( spec = setNames( 1:3, dfTitanic$id[1:3]), nrecs = 20 ) %>%
  SMRMonTakeValue


## Tests
test_that("History recommendations by data frame", {
  expect_true( nrow(dfRecs) == 20  )
  expect_true( ncol(dfRecs) == 3 )

  expect_s3_class( dfRecs, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(dfRecs) ) ) == 3 )

  expect_true( nrow(dfRecsG) == 20  )
  expect_true( ncol(dfRecsG) == 3 )

  expect_s3_class( dfRecsG, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(dfRecsG) ) ) == 3 )

  expect_equal( dfRecs, dfRecsG )
})

test_that("History recommendations by character vector", {
  expect_true( nrow(chRecs) == 20  )
  expect_true( ncol(chRecs) == 3 )

  expect_s3_class( chRecs, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(chRecs) ) ) == 3 )

  expect_true( nrow(chRecsG) == 20  )
  expect_true( ncol(chRecsG) == 3 )

  expect_s3_class( chRecsG, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(chRecsG) ) ) == 3 )

  expect_equal( chRecs, chRecsG )
})

test_that("History recommendations by numeric vector", {
  expect_true( nrow(nvRecs) == 20  )
  expect_true( ncol(nvRecs) == 3 )

  expect_s3_class( nvRecs, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(nvRecs) ) ) == 3 )

  expect_true( nrow(nvRecsG) == 20  )
  expect_true( ncol(nvRecsG) == 3 )

  expect_s3_class( nvRecsG, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(nvRecsG) ) ) == 3 )

  expect_equal( nvRecs, nvRecsG )
})
