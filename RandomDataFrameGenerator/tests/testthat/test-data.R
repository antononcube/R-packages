
test_that("Data tests", {

  expect_type( dfEnglishWords, "data.frame" )
  expect_equal( mean(is.na(dfEnglishWords)), 0 )

  expect_equal( sort(names(dfEnglishWords)), sort(c("Word", "KnownWordQ", "CommonWordQ", "StopWordQ")) )

  expect_equal( sum(dfEnglishWords$KnownWordQ), 84923 )
  expect_equal( sum(dfEnglishWords$CommonWordQ), 39176 )
  expect_equal( sum(dfEnglishWords$StopWordQ), 329 )

})
