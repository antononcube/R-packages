context("Same results with different signatures")
library(OutlierIdentifiers)

set.seed(2342)
randData <- runif( n = 200, min = -10, max = 100 )
randData2 <- runif( n = 19, min = 140, max = 160 )
randData3 <- runif( n = 12, min = -50, max = -30 )
randData <- sample( c( randData, randData2, randData3 ) )

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
    TopOutlierIdentifier( data = randData, identifier = QuartileIdentifierParameters ),
    TopOutlierIdentifier( data = randData, lowerAndUpperThresholds = QuartileIdentifierParameters(randData) )
  )

  expect_equal(
    TopOutlierIdentifier( data = randData, identifier = HampelIdentifierParameters ),
    TopOutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData) )
  )

})

test_that("BottomOutlierIdentifier equivalences", {

  expect_equal(
    BottomOutlierIdentifier( data = randData, identifier = QuartileIdentifierParameters ),
    BottomOutlierIdentifier( data = randData, lowerAndUpperThresholds = QuartileIdentifierParameters(randData) )
  )

  expect_equal(
    BottomOutlierIdentifier( data = randData, identifier = HampelIdentifierParameters ),
    BottomOutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData) )
  )

})

test_that("OutlierPosition equivalences", {

  expect_equal(
    OutlierPosition( data = randData, identifier = QuartileIdentifierParameters ),
    OutlierPosition( data = randData, lowerAndUpperThresholds = QuartileIdentifierParameters(randData) )
  )

  expect_equal(
    OutlierPosition( data = randData, identifier = HampelIdentifierParameters ),
    OutlierPosition( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData) )
  )

})

test_that("TopOutlierPosition equivalences", {

  expect_equal(
    TopOutlierPosition( data = randData, identifier = QuartileIdentifierParameters ),
    TopOutlierPosition( data = randData, lowerAndUpperThresholds = QuartileIdentifierParameters(randData) )
  )

  expect_equal(
    TopOutlierPosition( data = randData, identifier = HampelIdentifierParameters ),
    TopOutlierPosition( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData) )
  )

})

test_that("BottomOutlierPosition equivalences", {

  expect_equal(
    BottomOutlierPosition( data = randData, identifier = QuartileIdentifierParameters ),
    BottomOutlierPosition( data = randData, lowerAndUpperThresholds = QuartileIdentifierParameters(randData) )
  )

  expect_equal(
    BottomOutlierPosition( data = randData, identifier = HampelIdentifierParameters ),
    BottomOutlierPosition( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData) )
  )

})
