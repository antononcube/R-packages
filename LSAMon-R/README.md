# Latent Semantic Analysis Monad in R (LSAMon-R)

This package, `LSAMon-R`, implements a software monad for the specification of Latent Semantic Analysis (LSA) workflows.
`LSAMon-R`'s computations are based on the document-term matrix creation functions in the package
[SparseMatrixRecommender](https://github.com/antononcube/R-packages/tree/master/SparseMatrixRecommender)
and the packages for matrix factorization
[irlba](https://cran.r-project.org/web/packages/irlba/index.html) and
[NonNegativeMatrixFactorization](https://github.com/antononcube/R-packages/tree/master/NonNegativeMatrixFactorization).

Here is an example pipeline:

```r
lsaObj <-
  LSAMonUnit( textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stemWordsQ = TRUE, stopWords = stopwords::stopwords() ) %>%
  LSAMonApplyTermWeightFunctions( "IDF", "None", "Cosine" ) %>%
  LSAMonExtractTopics( numberOfTopics = numberOfTopics, minNumberOfDocumentsPerTerm = minNumberOfDocumentsPerTerm, maxSteps = maxSteps, method = "NNMF" ) %>%
  LSAMonEchoTopicsTable( numberOfTerms = 20 )
```

To install use the command:

```r
devtools::install_github("antononcube/R-packages", subdir = "LSAMon-R")   
```
