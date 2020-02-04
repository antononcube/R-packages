# Sparse Matrix Recommender Monad in R (SMRMon-R)

This package, `SMRMon-R`, implements a software monad for the specification of recommendation system workflows.
`SMRMon-R`'s recommendation computations are based on sparse matrix Linear algebra.

Here is an example pipeline:

```r
library(SMRMon)
smrObj <-
  SMRMonUnit( data = dfTitanic ) %>%
  SMRMonCreate( itemColumnName = "id", addTagTypesToColumnNamesQ = FALSE )
  SMRMonRecommend( history = dfTitanic$id[1:2], nrecs = 6, removeHistoryQ = FALSE ) %>%
  SMRMonJoinAcross( dfTitanic ) %>%
  SMRMonEchoValue
```
