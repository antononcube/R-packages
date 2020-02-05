# Latent Semantic Analysis Monad in R (LSAMon-R)

This package, `LSAMon-R`, implements a software monad for the specification of Latent Semenatic Analysis (LSA) workflows.
`LSAMon-R`'s computations are based on .

Here is an example pipeline:

```r
lsaObj <-
  LSAMonUnit( textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stemWordsQ = TRUE, stopWords = c( stopwords::stopwords(), "enter") ) %>%
  LSAMonApplyTermWeightFunctions( "IDF", "None", "Cosine" ) %>%
  LSAMonExtractTopics( numberOfTopics = numberOfTopics, minNumberOfDocumentsPerTerm = minNumberOfDocumentsPerTerm, maxSteps = maxSteps, method = "NNMF") %>%
  LSAMonEchoTopicsTable(numberOfTerms = 20)
```
