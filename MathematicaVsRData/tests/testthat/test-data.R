test_that("Data tests", {

  expect_is( dfTitanic, "data.frame" )
  expect_equal( sum(is.na(dfTitanic)), 0 )

  expect_is( dfMushroom, "data.frame" )
  expect_true( mean(is.na(dfMushroom)) < 0.02 )

  expect_is( dfDistributionData, "data.frame" )
  expect_equal( sum(is.na(dfDistributionData)), 0 )

  expect_is( dfFinancialData, "data.frame" )
  expect_equal( sum(is.na(dfFinancialData)), 0 )

  expect_is( dfTemperatureData, "data.frame" )
  expect_equal( sum(is.na(dfTemperatureData)), 0 )

  expect_is( dfUSAPresidentsSpeeches, "data.frame" )
  expect_equal( sum(is.na(dfUSAPresidentsSpeeches)), 0 )

  expect_is( textHamlet, "character")
  expect_equal( length(textHamlet), 223 )

  expect_is( dfSP500, "data.frame")
  expect_is( dfSP500$AbsoluteTime, "numeric" )
  expect_is( dfSP500$Date, "POSIXct" )

})
