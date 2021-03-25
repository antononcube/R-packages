context("Basic pipelines with profiles with unknown tags")
library(SMRMon)
library(SparseMatrixRecommender)

smrObj <-
  SMRMonUnit( data = dfTitanic ) %>%
  SMRMonCreate( itemColumnName = "id")

## Tests
test_that("Sanity check", {
  expect_s3_class( smrObj, "SMR" )
  expect_true( length( intersect( c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data"), names(smrObj) ) ) == 8 )
})


## Recommend by history data frame.
dfRecs <-
  smrObj %>%
  SMRMonRecommendByProfile( profile = data.frame( Score = 1, Tag = c( "female", "survived", "egg" ), stringsAsFactors = F), nrecs = 20, ignoreUnknownTagsQ = TRUE ) %>%
  SMRMonTakeValue

dfRecsG <-
  smrObj %>%
  SMRMonGetTopRecommendations( spec = data.frame( Score = 1, Tag = c( "female", "survived", "caste" ),  stringsAsFactors = F), nrecs = 20, ignoreUnknownTagsQ = TRUE ) %>%
  SMRMonTakeValue

## Recommend by profile character vector.
chRecs <-
  smrObj %>%
  SMRMonRecommendByProfile( profile = c( "female", "survived", "egg" ), nrecs = 20, ignoreUnknownTagsQ = TRUE ) %>%
  SMRMonTakeValue

## This fails with stop-error.
# chRecsG <-
#   smrObj %>%
#   SMRMonGetTopRecommendations( spec = c( "female", "survived" ), nrecs = 20 ) %>%
#   SMRMonTakeValue

## Recommend by profile numeric vector with named elements.
nvRecs <-
  smrObj %>%
  SMRMonRecommendByProfile( profile = setNames( c(1, 1, 1), c( "female", "survived", "caste" )), nrecs = 20, ignoreUnknownTagsQ = TRUE ) %>%
  SMRMonTakeValue

## This fails with stop error.
# nvRecsG <-
#   smrObj %>%
#   SMRMonGetTopRecommendations( spec = setNames( c(1, 1), c( "female", "survived" )), nrecs = 20 ) %>%
#   SMRMonTakeValue

## Recommend by profile sparse matrix / vector.

profVec <- SMRProfileVector( smrObj, itemHistory = data.frame( Score = 1, Tag = c( "id.2", "id.34" ),  stringsAsFactors = F) )
profVec2 <- profVec
rownames(profVec2) <- paste0( "some-", rownames(profVec2) )
profVec <- rbind( profVec, profVec2 )

sVecRecs <-
  smrObj %>%
  SMRMonRecommendByProfile( profile = profVec, nrecs = 20, ignoreUnknownTagsQ = TRUE ) %>%
  SMRMonTakeValue

sVecRecsG <-
  smrObj %>%
  SMRMonGetTopRecommendations( spec = profVec, nrecs = 20, ignoreUnknownTagsQ = TRUE ) %>%
  SMRMonTakeValue


## Tests
test_that("Profile recommendations by data frame", {
  expect_true( nrow(dfRecs) == 20  )
  expect_true( ncol(dfRecs) == 3 )

  expect_s3_class( dfRecs, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(dfRecs) ) ) == 3 )

  expect_true( nrow(dfRecsG) == 20  )
  expect_true( ncol(dfRecsG) == 3 )

  expect_s3_class( dfRecsG, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(dfRecsG) ) ) == 3 )
})

test_that("Profile recommendations by character vector", {
  expect_true( nrow(chRecs) == 20  )
  expect_true( ncol(chRecs) == 3 )

  expect_s3_class( chRecs, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(chRecs) ) ) == 3 )

  expect_equivalent( chRecs$Index, dfRecs$Index )
  #expect_equivalent( chRecs, chRecsG )
})

test_that("Profile recommendations by numeric vector", {
  expect_true( nrow(nvRecs) == 20  )
  expect_true( ncol(nvRecs) == 3 )

  expect_s3_class( nvRecs, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(nvRecs) ) ) == 3 )

  expect_equivalent( nvRecs$Index, dfRecs$Index )
  #expect_equivalent( nvRecs, dfRecsG )
})

test_that("Profile recommendations by sparse matrix / vector", {
  expect_true( nrow(sVecRecs) == 20  )
  expect_true( ncol(sVecRecs) == 3 )

  expect_s3_class( sVecRecs, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(sVecRecs) ) ) == 3 )

  expect_true( nrow(sVecRecsG) == 20  )
  expect_true( ncol(sVecRecsG) == 3 )

  expect_s3_class( sVecRecsG, "data.frame" )
  expect_true( length( intersect( c("Score", "Index", smrObj %>% SMRMonTakeItemColumnName), colnames(sVecRecsG) ) ) == 3 )

  expect_equivalent( sVecRecs, sVecRecsG )
})
