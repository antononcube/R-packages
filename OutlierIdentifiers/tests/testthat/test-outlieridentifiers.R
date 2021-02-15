context("Same results with different signatures")
library(OutlierIdentifiers)

set.seed(2342)
randData <- runif( n = 200, min = -10, max = 100 )
randData2 <- runif( n = 19, min = 140, max = 160 )
randData3 <- runif( n = 12, min = -50, max = -30 )
randData <- sample( c( randData, randData2, randData3 ) )

test_that("OutlierIdentifier equivalences", {

  expect_equal(
    OutlierIdentifier( data = randData, identifier = SPLUSQuartileIdentifierParameters, valueQ = TRUE ),
    OutlierIdentifier( data = randData, lowerAndUpperThresholds = SPLUSQuartileIdentifierParameters(randData), value = TRUE )
  )

  expect_equal(
    OutlierIdentifier( data = randData, identifier = HampelIdentifierParameters, valueQ = TRUE ),
    OutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData), valueQ = TRUE )
  )

  expect_equal(
    OutlierIdentifier( data = randData, identifier = SPLUSQuartileIdentifierParameters, valueQ = FALSE ),
    OutlierIdentifier( data = randData, lowerAndUpperThresholds = SPLUSQuartileIdentifierParameters(randData), value = FALSE )
  )

  expect_equal(
    OutlierIdentifier( data = randData, identifier = HampelIdentifierParameters, valueQ = FALSE ),
    OutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData), valueQ = FALSE )
  )

})

test_that("TopOutlierIdentifier equivalences", {

  expect_equal(
    TopOutlierIdentifier( data = randData, identifier = QuartileIdentifierParameters, valueQ = TRUE ),
    TopOutlierIdentifier( data = randData, lowerAndUpperThresholds = QuartileIdentifierParameters(randData), valueQ = TRUE )
  )

  expect_equal(
    TopOutlierIdentifier( data = randData, identifier = HampelIdentifierParameters, valueQ = TRUE ),
    TopOutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData), valueQ = TRUE )
  )

  expect_equal(
    TopOutlierIdentifier( data = randData, identifier = QuartileIdentifierParameters, valueQ = FALSE ),
    TopOutlierIdentifier( data = randData, lowerAndUpperThresholds = QuartileIdentifierParameters(randData), valueQ = FALSE )
  )

  expect_equal(
    TopOutlierIdentifier( data = randData, identifier = HampelIdentifierParameters, valueQ = FALSE ),
    TopOutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData), valueQ = FALSE )
  )

  expect_equal(
    TopOutlierIdentifier( data = randData, identifier = HampelIdentifierParameters, valueQ = FALSE ),
    OutlierIdentifier( data = randData, identifier = function(x) TopOutliersOnlyThresholds(HampelIdentifierParameters(x)), valueQ = FALSE )
  )

})

test_that("BottomOutlierIdentifier equivalences", {

  expect_equal(
    BottomOutlierIdentifier( data = randData, identifier = QuartileIdentifierParameters, valueQ = TRUE ),
    BottomOutlierIdentifier( data = randData, lowerAndUpperThresholds = QuartileIdentifierParameters(randData), valueQ = TRUE )
  )

  expect_equal(
    BottomOutlierIdentifier( data = randData, identifier = HampelIdentifierParameters, valueQ = TRUE ),
    BottomOutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData), valueQ = TRUE )
  )

  expect_equal(
    BottomOutlierIdentifier( data = randData, identifier = QuartileIdentifierParameters, valueQ = FALSE ),
    BottomOutlierIdentifier( data = randData, lowerAndUpperThresholds = QuartileIdentifierParameters(randData), valueQ = FALSE )
  )

  expect_equal(
    BottomOutlierIdentifier( data = randData, identifier = HampelIdentifierParameters, valueQ = FALSE ),
    BottomOutlierIdentifier( data = randData, identifier = NULL, lowerAndUpperThresholds = HampelIdentifierParameters(randData), valueQ = FALSE )
  )

  expect_equal(
    BottomOutlierIdentifier( data = randData, identifier = HampelIdentifierParameters, valueQ = FALSE ),
    OutlierIdentifier( data = randData, identifier = function(x) BottomOutliersOnlyThresholds(HampelIdentifierParameters(x)), valueQ = FALSE )
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
