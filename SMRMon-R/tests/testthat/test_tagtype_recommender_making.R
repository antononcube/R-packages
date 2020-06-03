context("Tag type recommender making")
library(SMRMon)


test_that("Metadata recommender matrix same as contingency matrix", {

  expect_is( smrObj <- SMRMonUnit( data = dfTitanic ) %>% SMRMonCreate( itemColumnName = "id" ), "SMR" )

  expect_is( smrMetaObj <- smrObj %>% SMRMonMakeTagTypeRecommender( tagTypeTo = "passengerClass"  ), "SMR" )

  expect_is( smrMetaObj$M, "dgCMatrix" )

  expect_is(
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
