library(SMRMon)
library(SparseMatrixRecommender)
library(OutlierIdentifiers)

set.seed(3323)

test_that("Identical anomalies computations", {

  expect_s3_class(
    smrTitanic <-
      SMRMonUnit() %>%
      SMRMonCreate( dfTitanic[sample(1:nrow(dfTitanic),600), ], itemColumnName = "id" ) %>%
      SMRMonApplyTermWeightFunctions( "None", "None", "Cosine" ),
    "SMR" )

  expect_equal(
    sort(
      smrTitanic %>%
        SMRMonFindAnomalies(
          numberOfNearestNeighbors = 30,
          aggregationFunction = mean,
          thresholdsIdentifier = function(x) BottomOutliersOnlyThresholds(HampelIdentifierParameters(x)),
          normalizeQ = FALSE,
          property = "RowNames",
          useBatchRecommendationQ = TRUE) %>%
        SMRMonTakeValue),
    sort(
      smrTitanic %>%
        SMRMonFindAnomalies(
          numberOfNearestNeighbors = 30,
          aggregationFunction = mean,
          thresholdsIdentifier = function(x) BottomOutliersOnlyThresholds(HampelIdentifierParameters(x)),
          normalizeQ = FALSE,
          property = "RowNames",
          useBatchRecommendationQ = FALSE) %>%
        SMRMonTakeValue
    )
  )

  expect_equal(
    sort(
      smrTitanic %>%
        SMRMonFindAnomalies(
          numberOfNearestNeighbors = 30,
          aggregationFunction = mean,
          thresholdsIdentifier = function(x) BottomOutliersOnlyThresholds(HampelIdentifierParameters(x)),
          normalizeQ = TRUE,
          property = "OutlierThresholds",
          useBatchRecommendationQ = TRUE) %>%
        SMRMonTakeValue),
    sort(
      smrTitanic %>%
        SMRMonFindAnomalies(
          numberOfNearestNeighbors = 30,
          aggregationFunction = mean,
          thresholdsIdentifier = function(x) BottomOutliersOnlyThresholds(HampelIdentifierParameters(x)),
          normalizeQ = TRUE,
          property = "OutlierThresholds",
          useBatchRecommendationQ = FALSE) %>%
        SMRMonTakeValue
    )
  )

})

