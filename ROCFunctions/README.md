# ROC functions

## In brief

This R package (sub-repository) is for computations of 
[Receiver Operating Characteristic (ROC)](https://en.wikipedia.org/wiki/Receiver_operating_characteristic)
functions.

------

## Installation

Here we install the `ROCFunctions` package from GitHub; 
we also install 
[`RandomDataFrameGenerator`](https://github.com/antononcube/R-packages/tree/master/RandomDataFrameGenerator)
which is used in the examples below:

``` r
devtools::install_github(repo = "antononcube/R-packages", subdir = "ROCFunctions")
devtools::install_github(repo = "antononcube/R-packages", subdir = "RandomDataFrameGenerator")
```

------

## Examples

```
library(ROCFunctions)
library(RandomDataFrameGenerator)
```


Dictionary of ROC functions abbreviations and their (descriptive) full names:

``` r
ROCAcronymsDictionary()
```

    ##                 Acronym                      Description
    ## TPR                 TPR               true positive rate
    ## TNR                 TNR               true negative rate
    ## SPC                 SPC                      specificity
    ## PPV                 PPV        positive predictive value
    ## NPV                 NPV        negative predictive value
    ## FPR                 FPR              false positive rate
    ## FDR                 FDR             false discovery rate
    ## FNR                 FNR              false negative rate
    ## ACC                 ACC                         accuracy
    ## AUROC             AUROC         area under the ROC curve
    ## FOR                 FOR              false omission rate
    ## F1                   F1                         F1 score
    ## MCC                 MCC Matthews correlation coefficient
    ## Recall           Recall                      same as TPR
    ## Precision     Precision                      same as PPV
    ## Accuracy       Accuracy                      same as ACC
    ## Sensitivity Sensitivity                      same as TPR

Here we compute specified ROCs for a numeric list:

``` r
ComputeROCFunctions( c("TruePositive" = 15, 
                       "FalsePositive" = 149, 
                       "TrueNegative" = 1523, 
                       "FalseNegative" = 15), c("Precision", "Recall", "PPV") )
```

    ##    Precision Recall        PPV
    ## 1 0.09146341    0.5 0.09146341

Here we generate a random data frame with specific column names used to compute ROCs:

``` r
dfRand <- RandomDataFrame(nrow =  3, columnNames = c("TruePositive", "TrueNegative", "FalsePositive", "FalseNegative"), generators = c( function(x) round(runif(x,0,100))) )
dfRand
```

    ##   TruePositive TrueNegative FalsePositive FalseNegative
    ## 1           90           51            93            14
    ## 2           79           16             7            18
    ## 3           67           68            52             3


Verify it is a ROC-data-frame:

```r
ROCDataFrameQ(dfRand)
```

    ## TRUE
    
Here we compute ROCs over the random data frame above:

``` r
ROCFunctions::ComputeROCFunctions(dfRand)
```

    ##         FPR       TPR
    ## 1 0.6458333 0.8653846
    ## 2 0.3043478 0.8144330
    ## 3 0.4333333 0.9571429
