context("Basic pipeline with profile recommendations")
library(SMRMon)
library(SparseMatrixRecommender)

smrObj <-
  SMRMonUnit( data = dfTitanic ) %>%
  SMRMonCreate( itemColumnName = "id")

## Tests
test_that("Sanity check", {
  expect_is( smrObj, "SMR" )
  expect_true( length( intersect( c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data"), names(smrObj) ) ) == 8 )
})


## Recommend by history data frame.
dfRecs <-
  smrObj %>%
  SMRMonRecommendByProfile( profile = data.frame( Score = 1, Tag = c( "female", "survived" ),  stringsAsFactors = F), nrecs = 20 ) %>%
  SMRMonTakeValue

## Recommend by profile character vector.
chRecs <-
  smrObj %>%
  SMRMonRecommendByProfile( profile = c( "female", "survived" ), nrecs = 20 ) %>%
  SMRMonTakeValue

## Recommend by history numeric vector with named elements.
nvRecs <-
  smrObj %>%
  SMRMonRecommendByProfile( profile = setNames( c(1, 1), c( "female", "survived" )), nrecs = 20 ) %>%
  SMRMonTakeValue

## Tests
test_that("Profile recommendations by data frame", {
  expect_true( nrow(dfRecs) == 20  )
  expect_true( ncol(dfRecs) == 3 )

  expect_is( dfRecs, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(dfRecs) ) ) == 3 )
})


test_that("Profile recommendations by character vector", {
  expect_true( nrow(chRecs) == 20  )
  expect_true( ncol(chRecs) == 3 )

  expect_is( chRecs, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(chRecs) ) ) == 3 )
})

test_that("Profile recommendations by numeric vector", {
  expect_true( nrow(nvRecs) == 20  )
  expect_true( ncol(nvRecs) == 3 )

  expect_is( nvRecs, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(nvRecs) ) ) == 3 )
})
