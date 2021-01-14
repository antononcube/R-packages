context("Data retrieval")
library(SMRMon)
library(SparseMatrixRecommender)

smrObj <-
  SMRMonUnit( data = dfTitanic ) %>%
  SMRMonCreate( itemColumnName = "id" )

## Sanity check tests
test_that("Sanity check", {

  expect_s3_class( smrObj, "SMR" )

  expect_equal( length( intersect( c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data"), names(smrObj) ) ),
                8 )

})

test_that("Sanity check after matrix filtering", {

  expect_s3_class( smrObj %>% SMRMonFilterMatrix( profile = c( "male" ) ), "SMR" )

  expect_equal( length( intersect( c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data"),
                                  names( smrObj %>% SMRMonFilterMatrix( profile = c( "male" ) ) ) ) ),
                8 )

})

## Test data shapes of long form.
test_that("Expected long form representation data shapes", {

  expect_s3_class( smrObj %>% SMRMonGetLongFormData( tagTypesQ = FALSE ) %>% SMRMonTakeValue, "data.frame" )

  expect_s3_class( smrObj %>% SMRMonGetLongFormData( tagTypesQ = TRUE ) %>% SMRMonTakeValue, "data.frame" )

  expect_true( ncol( smrObj %>% SMRMonGetLongFormData( tagTypesQ = FALSE ) %>% SMRMonTakeValue ) == 3 )

  expect_true( ncol( smrObj %>% SMRMonGetLongFormData( tagTypesQ = TRUE ) %>% SMRMonTakeValue ) == 4 )

  expect_equal( length( intersect( c( smrObj %>% SMRMonTakeItemColumnName, "Tag", "Value" ),
                                  colnames( smrObj %>% SMRMonGetLongFormData( tagTypesQ = FALSE ) %>% SMRMonTakeValue ) ) ),
                3 )

  expect_equal( length( intersect( c( smrObj %>% SMRMonTakeItemColumnName, "TagType", "Tag", "Value" ),
                                   colnames( smrObj %>% SMRMonGetLongFormData( tagTypesQ = TRUE ) %>% SMRMonTakeValue ) ) ),
                4 )

  expect_equal( length( (smrObj %>% SMRMonTakeM)@x ), nrow( smrObj %>% SMRMonGetLongFormData( tagTypesQ = FALSE ) %>% SMRMonTakeValue ) )

  expect_equal( length( (smrObj %>% SMRMonTakeM)@x ), nrow( smrObj %>% SMRMonGetLongFormData( tagTypesQ = TRUE ) %>% SMRMonTakeValue ) )

})

## Test data shapes after filtering of type "union".
test_that("Expected data shapes after filtering: union", {

  expect_equal( nrow( smrObj %>% SMRMonFilterMatrix( profile = c( "male" ), type = "union" ) %>% SMRMonTakeM ) ,
                nrow( dfTitanic %>% dplyr::filter( passengerSex == "male" ) ) )

  expect_equal( nrow( smrObj %>% SMRMonFilterMatrix( profile = c( "male", "1st" ), type = "union" ) %>% SMRMonTakeM ) ,
                nrow( dfTitanic %>% dplyr::filter( passengerSex == "male" | passengerClass == "1st" ) ) )

})

## Test data shapes after filtering of type "intersection".
test_that("Expected data shapes after filtering: intersection", {

  expect_equal( nrow( smrObj %>% SMRMonFilterMatrix( profile = c( "male" ), type = "intersection" ) %>% SMRMonTakeM ) ,
                nrow( dfTitanic %>% dplyr::filter( passengerSex == "male" ) ) )

  expect_equal( nrow( smrObj %>% SMRMonFilterMatrix( profile = c( "male", "1st" ), type = "intersection" ) %>% SMRMonTakeM ) ,
                nrow( dfTitanic %>% dplyr::filter( passengerSex == "male" & passengerClass == "1st" ) ) )

  expect_equal( nrow( smrObj %>% SMRMonFilterMatrix( profile = c( "male", "1st", "died" ), type = "intersection" ) %>% SMRMonTakeM ) ,
                nrow( dfTitanic %>% dplyr::filter( passengerSex == "male" & passengerClass == "1st" & passengerSurvival == "died" ) ) )

})

## Test summarization.
test_that("Expected data shapes after summarization", {

  expect_s3_class( itemSummary <- smrObj %>% SMRMonSummarizeItem( item = "id.10" ) %>% SMRMonTakeValue, "list" )

  expect_true( mean( c( "Profile", "TagsSummary" ) %in% names(itemSummary) ) == 1 )

  expect_s3_class( itemSummary$Profile, "data.frame" )

  expect_s3_class( itemSummary$TagsSummary, "data.frame" )

})
