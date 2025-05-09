context("Sparse matrix recommender creation")
library(SparseMatrixRecommender)

## When creating the submatrices SMRCreateFromSpecificaiton
## sorts the obtained names (because it uses split.)
sMatNames <- sort(colnames(dfTitanic)[-1])

## Create SMR directly from data

smr1 <- SMRCreate( dfTitanic[, c("id", sMatNames)], 
                   tagTypes = sMatNames, 
                   itemColumnName = "id" )


## Create SMR with matrices

sMats <- purrr::map( sMatNames, function(x) { xtabs( as.formula( paste0("~ id + ", x) ), dfTitanic, sparse = TRUE ) } )
names(sMats) <- sMatNames

smr2 <- SMRCreateFromMatrices( matrices = sMats, itemColumnName = "id", imposeSameRowNamesQ = TRUE )

## With matrices with different number of rows.
sMats[[1]] <- sMats[[1]][1:300,]
sMats[[2]] <- sMats[[2]][300:800,]
smr2a <- SMRCreateFromMatrices( matrices = sMats, itemColumnName = "id" )

## Create SMR with specification

dfSpec <- SMREmptySpecification( nrow = length(sMatNames) )
dfSpec$ColumnName <- sMatNames

smr3 <- SMRCreateFromSpecification( data = dfTitanic, 
                                    metaDataSpec = dfSpec, 
                                    itemColumnName = "id") 

## Create SMR from long form

dfSMRLongForm <- SMRMatricesToLongForm(smr = smr1 )

smr4 <- SMRCreateFromLongForm( data = dfSMRLongForm, itemColumnName = "id" ) 


## Tests
test_that("Matrices long form", {
  expect_is( dfSMRLongForm, "data.frame" )  

  expect_equal( names(dfSMRLongForm), c( "id", "Value", "Weight", "TagType" ) )
})

test_that("SMR objects", {
  expect_is( smr1, "SMR" )
  expect_is( smr2, "SMR" )
  expect_is( smr2a, "SMR" )
  expect_is( smr3, "SMR" )
  expect_is( smr4, "SMR" )
  
  expect_equal( names(smr1), c("M", "M01", "TagTypeRanges","TagTypes",      
                               "ItemColumnName", "TagToIndexRules", "ItemToIndexRules") )
  expect_equal( names(smr1), names(smr2) )
  expect_equal( names(smr1), names(smr2a) )
  expect_equal( names(smr1), names(smr3) )
  
  expect_is( smr1$M, "dgCMatrix" ) 
  expect_is( smr2$M, "dgCMatrix" ) 
  expect_is( smr2a$M, "dgCMatrix" ) 
  expect_is( smr3$M, "dgCMatrix" ) 
  expect_is( smr4$M, "dgCMatrix" ) 
  
}) 

test_that("Same (sub-)matrices: SMRCreate and SMRCreateFromMatrices", {
  expect_true( norm(as.matrix( smr1$M - smr2$M ) ) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerClass" ) - SMRSubMatrix( smr2, "passengerClass" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerAge" ) - SMRSubMatrix( smr2, "passengerAge" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerSex" ) - SMRSubMatrix( smr2, "passengerSex" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerSurvival" ) - SMRSubMatrix( smr2, "passengerSurvival" ) )) < 1E-14 )
}) 

test_that("Same (sub-)matrices: SMRCreate and SMRCreateFromSpecification", {
  expect_true( norm(as.matrix( smr1$M - smr3$M ) ) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerClass" ) - SMRSubMatrix( smr3, "passengerClass" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerAge" ) - SMRSubMatrix( smr3, "passengerAge" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerSex" ) - SMRSubMatrix( smr3, "passengerSex" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerSurvival" ) - SMRSubMatrix( smr3, "passengerSurvival" ) )) < 1E-14 )
}) 

test_that("Same (sub-)matrices: SMRCreate and SMRCreateFromLongForm", {
  expect_true( mean( dim(smr1$M) == dim(smr4$M) ) == 1 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerClass" ) - SMRSubMatrix( smr4, "passengerClass" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerAge" ) - SMRSubMatrix( smr4, "passengerAge" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerSex" ) - SMRSubMatrix( smr4, "passengerSex" ) )) < 1E-14 )
  expect_true( norm(as.matrix( SMRSubMatrix( smr1, "passengerSurvival" ) - SMRSubMatrix( smr4, "passengerSurvival" ) )) < 1E-14 )
}) 
