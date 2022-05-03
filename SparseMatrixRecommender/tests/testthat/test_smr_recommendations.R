context("Recommendations")
library(SparseMatrixRecommender)


## When creating the submatrices SMRCreateFromSpecificaiton
## sorts the obtained names (because it uses split.)
sMatNames <- sort(colnames(dfMushroom)[-1])

## Create SMR directly from data.

smr <- SMRCreate( dfMushroom[, c("id", sMatNames)], 
                  tagTypes = sMatNames, 
                  itemColumnName = "id" )

historyRecs <- SMRRecommendations( smr = smr, userHistoryItems = c(1, 14, 33), userRatings = c(1), nrecs = 12 )

historyRecsNULL <- SMRRecommendations( smr = smr, userHistoryItems = c(1, 14, 33), userRatings = c(1), nrecs = NULL )

prof <- SMRProfileDF( smr, itemHistory = data.frame( Rating = c(1,1,1), Item = c("1", "14", "33"), stringsAsFactors = F ) )

profRecs12 <- SMRRecommendationsByProfileDF( smr = smr, profile = prof[, c("Score", "Tag")], nrecs = 12 )

profRecs10K <- SMRRecommendationsByProfileDF( smr = smr, profile = prof[, c("Score", "Tag")], nrecs = nrow(dfMushroom) + 100 )

profRecsNULL <- SMRRecommendationsByProfileDF( smr = smr, profile = prof[, c("Score", "Tag")], nrecs = NULL )


## Tests

test_that("Expected SMR object", {
  expect_is( smr, "SMR" )
  
  expect_true( SMRSparseMatrixQ(smr$M) )
  expect_true( SMRSparseMatrixQ(smr$M01) )
  
  expect_true( nrow(smr$M) > 8000 )
  expect_true( ncol(smr$M) > 110 )
})


test_that("Expected profile object", {
  expect_is( prof, "data.frame" )
  
  expect_true( mean( colnames(prof) == c( "Score", "Index", "Tag" ) ) == 1 )
  expect_is( prof$Tag, "character" )

})


test_that("Recommendations by history expected objects", {
  
  expect_is( historyRecs, "data.frame" )
  
  expect_true( mean( colnames(historyRecs) == c( "Score", "Index", smr$ItemColumnName ) ) == 1 )
  expect_is( historyRecs[[smr$ItemColumnName]], "character" )
  
})

test_that("Recommendations by history expected objects, nrecs=NULL", {
  
  expect_is( historyRecsNULL, "data.frame" )
  
  expect_true( mean( colnames(historyRecsNULL) == c( "Score", "Index", smr$ItemColumnName ) ) == 1 )
  expect_is( historyRecsNULL[[smr$ItemColumnName]], "character" )
  
})

test_that("Recommendations by history errors", {
  expect_error( SMRRecommendations( smr = smr, userHistoryItems = c("a", "b"), userRatings = c(2,3), nrecs = 10 ) )
  expect_error( SMRRecommendations( smr = smr, userHistoryItems = c( 1, 12), userRatings = c(2,3), nrecs = -10 ) )
})


test_that("Recommendations by profile expected objects", {
  
  expect_is( profRecs12, "data.frame" )
  expect_is( profRecs10K, "data.frame" )
  expect_is( profRecsNULL, "data.frame" )
  
  expect_true( mean( colnames(profRecs12) == c( "Score", "Index", smr$ItemColumnName ) ) == 1 )
  expect_true( mean( colnames(profRecs10K) == c( "Score", "Index", smr$ItemColumnName ) ) == 1 )
  expect_true( mean( colnames(profRecsNULL) == c( "Score", "Index", smr$ItemColumnName ) ) == 1 )
  
  expect_is( profRecs12[[smr$ItemColumnName]], "character" )
  expect_is( profRecs10K[[smr$ItemColumnName]], "character" )
  expect_is( profRecsNULL[[smr$ItemColumnName]], "character" )
  
  expect_equal( profRecs12$Score, rev(sort(profRecs12$Score)) )
  expect_equal( profRecs10K$Score, rev(sort(profRecs10K$Score)) )
  expect_equal( profRecsNULL$Score, rev(sort(profRecsNULL$Score)) )
  
})