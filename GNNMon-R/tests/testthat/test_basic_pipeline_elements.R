context("Basic pipeline elements")
library(GNNMon)

## Generated random data
set.seed(234)
data <- matrix( c( rnorm( n = 30, mean = 0, sd = 5 ), rnorm( n = 30, mean = 12, sd = 3) ), ncol = 2 )
colnames(data) <- c("X", "Y")

dfPoints <- data.frame( X = c(-6,2), Y = c(4.5,16) )

gnnObj <-
  GNNMonUnit( data ) %>%
  GNNMonComputeNearestNeighborDistances( nTopNNs = 3, method = "euclidean" ) %>%
  GNNMonComputeThresholds( nnsRadiusFunction = mean, thresholdsIdentifier = OutlierIdentifiers::HampelIdentifierParameters )

expectedMemberNames <- c("Value", "Data", "NumberOfNNs", "DistanceMethod", "NearestNeighborDistances", "RadiusFunction", "Radius", "LowerThreshold", "UpperThreshold")

## Tests
test_that("Sanity check", {
  expect_is( gnnObj, "GNNMon" )
  expect_true( length( intersect( expectedMemberNames, names(gnnObj) ) ) == length(expectedMemberNames) )
})


test_that("NN distances data frame", {
  expect_is( gnnObj %>% GNNMonTakeNearestNeighborDistances, "data.frame" )
  expect_true( mean( c( "SearchIndex", "Distance" ) %in% names( gnnObj %>% GNNMonTakeNearestNeighborDistances ) ) == 1 )
})

test_that("Radiuses data frame", {
  expect_is( gnnObj %>% GNNMonTakeValue, "data.frame" )
  expect_true( mean( c( "SearchIndex", "Radius" ) %in% names( gnnObj %>% GNNMonTakeValue ) ) == 1 )
})

## Classify
test_that("Classify", {
  expect_is( dfRes <- gnnObj %>% GNNMonClassify( points = as.matrix(dfPoints) ) %>% GNNMonTakeValue, "data.frame" )
  expect_true( mean( c( "Index", "Radius", "Label" ) %in% names( dfRes ) ) == 1 )
})
