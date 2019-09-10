context("Random pipelines")
library(QRMon)

set.seed(4827)
numberOfPipelines <- 40

pipelineLevels <-
  list( level1 = c( "LSAMonUnit()",
                    "LSAMonUnit( textHamlet )" ),

        level2 = c( "LSAMonMakeDocumentTermMatrix()",
                    "LSAMonMakeDocumentTermMatrix( stopWords = stopwords::stopwords() )",
                    "LSAMonMakeDocumentTermMatrix( stemWordsQ = T )",
                    "LSAMonMakeDocumentTermMatrix( stemWordsQ = T, stopWords = stopwords::stopwords() )" ),

        level3 = c( "LSAMonApplyTermWeightFunctions( 'None', 'None', 'Cosine' )",
                    "LSAMonApplyTermWeightFunctions( 'IDF', 'Binary', 'Cosine' )",
                    "LSAMonApplyTermWeightFunctions( 'None', 'None', 'BlahBlah' )" ),

        level4 = c( "LSAMonExtractTopics( numberOfTopics = 20, minNumberOfDocumentsPerTerm = 2, maxSteps = 100, method = 'SVD' )",
                    "LSAMonExtractTopics( numberOfTopics = 20, minNumberOfDocumentsPerTerm = 2, maxSteps = 3, method = 'NNMF' )",
                    "LSAMonExtractTopics( maxSteps = 10, method = 'SVD' )",
                    "LSAMonExtractTopics( maxSteps = 10, method = 'BlahBlah' )",
                    "LSAMonExtractTopics( method = 'BlahBlah' )",
                    "LSAMonExtractTopics( numberOfTopics = 'a' )" ),

        level5 = c( "LSAMonInterpretBasisVectors( n = 20 )",
                    "LSAMonTopicRepresentation( tags = tags, minThreshold = 0.005 )",
                    "LSAMonStatisticalThesaurus( searchWords = c('king', 'sword', 'ship', 'poison') )",
                    "LSAMonFindMostImportantTexts( nTop = 8 )" )
  )

randomPipelines <-
  purrr::map( 1:numberOfPipelines,
              function(x) {
                rp <- Reduce( function(a,x) { c( a, sample(x,1) )}, init = c(), x = pipelineLevels )
                parse( text = paste( rp, collapse = " %>% " ))
              })

lsaMonRes <- purrr::map( randomPipelines, purrr::safely(eval))

checkRes <- purrr::map_lgl( lsaMonRes, function(x) is.na(x$result) || is.list(x$result) && class(x$result) == "LSAMon" )

test_that( "Random pipelines produce NA's or LSAMon S3 objects.", {
  expect_true( sum(is.na(checkRes)) == 0 )
  expect_true( sum(is.na(checkRes)) == 0 && mean(checkRes) == 1 )
})
