context("Profile by history")
library(SMRMon)
library(SparseMatrixRecommender)
library(dplyr)
library(tidyr)

smrObj <-
  SMRMonUnit( data = dfMushroom ) %>%
  SMRMonCreate( itemColumnName = "id")

dfMushroomLongForm <- dfMushroom %>% tidyr::gather( key = "Property", value = "Value", -id )
dfMushroomLongForm$id <- as.character( dfMushroomLongForm$id )

smrObj2 <-
  SMRMonUnit( data = dfTitanic ) %>%
  SMRMonCreate( itemColumnName = "id")

dfTitanicLongForm <- dfTitanic %>% tidyr::gather( key = "Property", value = "Value", -id )


## Test sanity
test_that("Sanity check", {
  expect_is( smrObj, "SMR" )
  expect_true( length( intersect( c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data"), names(smrObj) ) ) == 8 )

  expect_is( smrObj2, "SMR" )
  expect_true( length( intersect( c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data"), names(smrObj2) ) ) == 8 )
})


## Tests
test_that("Profile data frame", {

  expect_is( prof0 <- smrObj %>% SMRMonProfile( history = c("1001"), tagTypesQ = FALSE ) %>% SMRMonTakeValue, "data.frame" )
  expect_is( prof1 <- smrObj %>% SMRMonProfile( history = c("1001"), tagTypesQ = TRUE ) %>% SMRMonTakeValue, "data.frame" )

  expect_equal( names( prof0 ), c( "Score", "Index", "Tag" ) )
  expect_equal( names( prof1 ), c( "Score", "Index", "TagType", "Tag" ) )

  ## I would like to test for Property == TagType,
  ## but for a given id dfMushroom has same tag values corresponding to different values of TagType / Property.
  expect_equal(
    mean(
      dfMushroomLongForm %>%
        dplyr::filter( id == "1001" ) %>%
        dplyr::inner_join( prof1, by = c( "Property" = "TagType") ) %>%
        dplyr::mutate( Test = Value == Tag ) %>%
        dplyr::pull( Test )
    ),
    1
  )

  expect_is( prof2 <- smrObj2 %>% SMRMonProfile( history = c("id.10"), tagTypesQ = FALSE ) %>% SMRMonTakeValue, "data.frame" )
  expect_is( prof3 <- smrObj2 %>% SMRMonProfile( history = c("id.10"), tagTypesQ = TRUE ) %>% SMRMonTakeValue, "data.frame" )

  expect_equal(
    mean(
      dfTitanicLongForm %>%
        dplyr::filter( id == "id.10" ) %>%
        dplyr::inner_join( prof3, by = c( "Value" = "Tag") ) %>%
        dplyr::mutate( Test = Property == TagType ) %>%
        dplyr::pull( Test )
    ),
    1
  )

})
