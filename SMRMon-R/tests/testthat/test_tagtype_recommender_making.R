context("Tag type recommender making")
library(SMRMon)


test_that("Metadata recommender matrix same as contingency matrix", {

  expect_s3_class( smrObj <- SMRMonUnit( data = dfTitanic ) %>% SMRMonCreate( itemColumnName = "id" ), "SMR" )

  expect_s3_class( { smrMetaObj <- smrObj %>% SMRMonMakeTagTypeRecommender( tagTypeTo = "passengerClass" ); smrMetaObj}, "SMR" )

  expect_s3_class( smrMetaObj$M, "dgCMatrix" )

  expect_s3_class(
    matXTabs <-
      cbind(
        xtabs( formula = ~ passengerClass + passengerAge, data = dfTitanic, sparse = TRUE ),
        xtabs( formula = ~ passengerClass + passengerSex, data = dfTitanic, sparse = TRUE ),
        xtabs( formula = ~ passengerClass + passengerSurvival, data = dfTitanic, sparse = TRUE )
      ),
    "dgCMatrix"
    )

  expect_equal( smrMetaObj$M, matXTabs )

})
