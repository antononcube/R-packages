context("Data retrieval")
library(SMRMon)
library(SparseMatrixRecommender)

smrObj <-
  SMRMonUnit( data = dfTitanic ) %>%
  SMRMonCreate( itemColumnName = "id" )

## Sanity check tests
test_that("Sanity check", {

  expect_is( smrObj, "SMR" )

  expect_equal( length( intersect( c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data"), names(smrObj) ) ),
                8 )

})

test_that("Sanity check after matrix filtering", {

  expect_is( smrObj %>% SMRMonFilterMatrix( profile = c( "male" ) ), "SMR" )

  expect_equal( length( intersect( c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data"),
                                  names( smrObj %>% SMRMonFilterMatrix( profile = c( "male" ) ) ) ) ),
                8 )

})

## Test data shapes of long form.
test_that("Expected long form representation data shapes", {

  expect_is( smrObj %>% SMRMonGetLongFormData( tagTypesQ = FALSE ) %>% SMRMonTakeValue, "data.frame" )

  expect_is( smrObj %>% SMRMonGetLongFormData( tagTypesQ = TRUE ) %>% SMRMonTakeValue, "data.frame" )

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

## Test data shapes after filtering.
test_that("Expected data shapes after filtering", {

  expect_equal( nrow( smrObj %>% SMRMonFilterMatrix( profile = c( "male" ) ) %>% SMRMonTakeM ) ,
                nrow( dfTitanic %>% dplyr::filter( passengerSex == "male" ) ) )

  expect_equal( nrow( smrObj %>% SMRMonFilterMatrix( profile = c( "male", "1st" ) ) %>% SMRMonTakeM ) ,
                nrow( dfTitanic %>% dplyr::filter( passengerSex == "male" | passengerClass == "1st" ) ) )

})
