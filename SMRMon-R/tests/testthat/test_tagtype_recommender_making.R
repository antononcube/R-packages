context("Tag type recommender making")
library(SMRMon)


test_that("Metadata recommender matrix same as contingency matrix", {

  ## Contingency matrices
  expect_s4_class(
    matXTabs <-
      cbind(
        xtabs( formula = ~ passengerClass + passengerAge, data = dfTitanic, sparse = TRUE ),
        xtabs( formula = ~ passengerClass + passengerSex, data = dfTitanic, sparse = TRUE ),
        xtabs( formula = ~ passengerClass + passengerSurvival, data = dfTitanic, sparse = TRUE )
      ),
    "dgCMatrix"
    )

  ## Make SMR
  expect_s3_class( smrObj <- SMRMonUnit( data = dfTitanic ) %>% SMRMonCreate( itemColumnName = "id" ), "SMR" )

  ## By replacement
  expect_s3_class( smrMetaObjRepl <- smrObj %>% SMRMonMakeTagTypeRecommenderByReplacement( tagTypeTo = "passengerClass" ), "SMR" )

  expect_s4_class( smrMetaObjRepl$M, "dgCMatrix" )

  expect_equal( smrMetaObjRepl$M, matXTabs )

  ## By matrix
  expect_s3_class( smrMetaObj <- smrObj %>% SMRMonMakeTagTypeRecommender( tagTypeTo = "passengerClass" ), "SMR" )

  expect_s4_class( smrMetaObj$M, "dgCMatrix" )

  expect_equal( smrMetaObj$M, matXTabs )

})
