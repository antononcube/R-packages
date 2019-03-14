context("Same results with different signatures")
library(OutlierIdentifiers)

randData <- runif( n = 100, min = -10, max = 100 )


test_that("OutlierIdentifier equivalences", {

  expect_equal(
    OutlierIdentifier( data = randData, identifier = SPLUSQuartileIdentifierParameters ),
    OutlierIdentifier( data = randData, lowerAndUpperThresholds = SPLUSQuartileIdentifierParameters(randData) )
  )

  expect_equal(
    OutlierIdentifier( data = randData, identifier = HampelIdentifierParameters ),
    OutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData) )
  )

})

test_that("TopOutlierIdentifier equivalences", {

  expect_equal(
    TopOutlierIdentifier( data = randData, identifier = SPLUSQuartileIdentifierParameters ),
    TopOutlierIdentifier( data = randData, lowerAndUpperThresholds = SPLUSQuartileIdentifierParameters(randData) )
  )

  expect_equal(
    TopOutlierIdentifier( data = randData, identifier = HampelIdentifierParameters ),
    TopOutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData) )
  )

})

test_that("BottomOutlierIdentifier equivalences", {

  expect_equal(
    BottomOutlierIdentifier( data = randData, identifier = SPLUSQuartileIdentifierParameters ),
    BottomOutlierIdentifier( data = randData, lowerAndUpperThresholds = SPLUSQuartileIdentifierParameters(randData) )
  )

  expect_equal(
    BottomOutlierIdentifier( data = randData, identifier = HampelIdentifierParameters ),
    BottomOutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData) )
  )

})

test_that("OutlierPosition equivalences", {

  expect_equal(
    OutlierPosition( data = randData, identifier = SPLUSQuartileIdentifierParameters ),
    OutlierPosition( data = randData, lowerAndUpperThresholds = SPLUSQuartileIdentifierParameters(randData) )
  )

  expect_equal(
    OutlierPosition( data = randData, identifier = HampelIdentifierParameters ),
    OutlierPosition( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData) )
  )

})
