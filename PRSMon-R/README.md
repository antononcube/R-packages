# Proxy Recommender Monad in R (PRSMon-R)

This package, `PRSMon-R`, implements a software monad for the specification of recommendation system workflows.
`PRSMon-R`'s recommendation computations are based on GET / POST server calls.


Here are example pipelines:

```r
prsObj <- PRSMonUnit() %>% PRSMonSetServerURL( "http://127.0.0.1:9720" )
```

```r
prsObj <- 
  prsObj %>% 
  PRSMonRecommend( history = c( "id.12"), nrecs = 20, parser = jsonlite::fromJSON ) %>% 
  PRSMonEchoValue
```

```{r}
prsObj <- 
  prsObj %>% 
  PRSMonRecommendByProfile( profile = c( "male", "1st" ), nrecs = 6, parser = jsonlite::fromJSON ) %>% 
  PRSMonEchoValue
```

To install use the command:

```r
devtools::install_github("antononcube/R-packages", subdir = "PRSMon-R")   
```
