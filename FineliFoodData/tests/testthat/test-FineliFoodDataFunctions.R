library(FineliFoodData)

test_that("List of Fineli food data tables", {

  expect_true( is.list( lsFineliFoodDataTables ) )

  expect_equal( length( lsFineliFoodDataTables ), 47)

  expect_equal( mean(sapply( lsFineliFoodDataTables, is.data.frame)), 1)
})


test_that("Summarize", {

  expect_true( is.data.frame( SummarizeTables() ) )

  expect_true( is.data.frame( SummarizeTables(lsFineliFoodDataTables) ) )

})

test_that("Dictionary conversions", {

  expect_true( is.character( MakeDictionary( "food", nameFrom = "FOODID", nameTo = "IGCLASS", lang = "English" ) ) )

  expect_true( is.integer( MakeDictionary( "food", nameFrom = "IGCLASS", nameTo = "FOODID", lang = "English" ) ) )

  expect_true( is.character( MakeDictionary( "foodname", lang = "English" ) ) )

  expect_true( is.character( MakeDictionary( "foodname", lang = "EN" ) ) )

  expect_true( is.character( MakeDictionary( "foodname", lang = "Finnish" ) ) )

  expect_true( is.character( MakeDictionary( "foodname", lang = "FI" ) ) )

  expect_true( is.character( MakeDictionary( "foodname", lang = "Swedish" ) ) )

  expect_true( is.character( MakeDictionary( "foodname", lang = "SV" ) ) )

})


test_that("Wrong dictionary conversions", {

  expect_error( MakeDictionary( "food", nameFrom = "FOODID", nameTo = "IGCLASS", lang = "Edfd" ), regexp = "The argument lang.*" )

  expect_error( MakeDictionary( "food", nameFrom = "dfdsfsw", nameTo = "IGCLASS", lang = "English" ), regexp = "The value of nameFrom.*" )

  expect_error( MakeDictionary( "food", nameFrom = "FOODID", nameTo = "dfsfsd", lang = "English" ), regexp = "The value of nameTo.*" )

})
