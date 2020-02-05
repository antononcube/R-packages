# Sparse Matrix Recommender Monad in R (SMRMon-R)

This package, `SMRMon-R`, implements a software monad for the specification of recommendation system workflows.
`SMRMon-R`'s recommendation computations are based on Sparse matrix Linear algebra.

Most of the functions of `SMRMon-R` delegate to corresponding functions of the package 
[SparseMatrixRecommender](https://github.com/antononcube/R-packages/tree/master/SparseMatrixRecommender).

Here is an example pipeline:

```r
smrObj <-
  SMRMonUnit( data = dfTitanic ) %>%
  SMRMonCreate( itemColumnName = "id", addTagTypesToColumnNamesQ = FALSE )
  SMRMonRecommend( history = dfTitanic$id[1:2], nrecs = 6, removeHistoryQ = FALSE ) %>%
  SMRMonJoinAcross( dfTitanic ) %>%
  SMRMonEchoValue
```

To install use the command:

```r
devtools::install_github("antononcube/R-packages", subdir = "SMRMon-R")   
```
