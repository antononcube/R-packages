# Data obfuscation

This package has functions for data obfuscation.

The primary use case is the obfuscation of financial transactions data.

Here is an example call for one of the columns of 
[`dfMintBubblesTransactions`](./inst/extdata/dfMintBubblesTransactions.csv) :

```r
ObfuscateDescriptions( descriptions = dfMintBubblesTransactions$Description, 
                       mappedWords = c( "Angela" = "LANDLORD", "AMAZON" = "NETFLIX" ), 
                       preservedWords = c("CANADA", "SAN"), 
                       randomWordsQ = TRUE, randomWordLengthsQ = FALSE )
```
