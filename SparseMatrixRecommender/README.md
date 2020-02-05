# Sparse Matrix Recommender (SMR) package

## In brief

This package, `SparseMatrixRecommender`, has different functions for computations of recommendations
based (user) profile or history using Sparse matrix Linear Algebra. Initially the package mirrored
the Mathematica implementation [AAp1]. 

The package is based on a certain "standard" Information retrieval paradigm -- it utilizes 
Latent Semantic Indexing (LSI) functions like IDF, TF-IDF, etc. Hence the package also has 
document-term matrix creation functions and LSI application functions. I included them in the 
package since I wanted to minimize the external package dependencies.

See the notebook 
[SMR-creation-and-usage-example.Rmd](./notebooks/SMR-creation-and-usage-example.Rmd) 
or the corresponding
[HTML notebook](https://htmlpreview.github.io/?https://raw.githubusercontent.com/antononcube/R-packages/master/SparseMatrixRecommender/notebooks/SMR-creation-and-usage-example.nb.html)
for examples.

The package includes two data-sets `dfTitanic` and `dfMushroom` in order to make easier the
writing of introductory examples and unit tests.

For more theoretical description see the article [AA1].

## Extensions

The package 
[`SMRMon-R`](https://github.com/antononcube/R-packages/tree/master/SMRMon-R), 
[AAp2], implements a software monad for SMR workflows. 
Most of `SMRMon-R` functions delegate to `SparseMatrixRecommender`.

The package 
[`SparseMatrixRecommenderInterfaces`](https://github.com/antononcube/R-packages/tree/master/SparseMatrixRecommenderInterfaces), 
[AAp3], provides functions for interactive 
[Shiny](https://shiny.rstudio.com)
interfaces for the recommenders made with `SparseMatrixRecommender` and/or `SMRMon-R`.

The package 
[`LSAMon-R`](https://github.com/antononcube/R-packages/tree/master/LSAMon-R),
[AAp4], can be used to make matrices for `SparseMatrixRecommender`.


## References

### Articles

[AA1] Anton Antonov,
[Mapping Sparse Matrix Recommender to Streams Blending Recommender](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/MappingSMRtoSBR/Mapping-Sparse-Matrix-Recommender-to-Streams-Blending-Recommender.pdf)
(2017),
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).


### Packages 

[AAp1] Anton Antonov, 
[Sparse matrix recommender framework in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/SparseMatrixRecommenderFramework.m),
(2014),
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[AAp2] Anton Antonov,
[Sparse Matrix Recommender Monad in R](https://github.com/antononcube/R-packages/tree/master/SMRMon-R)
(2019),
[R-packages at GitHub](https://github.com/antononcube/R-packages).

[AAp3] Anton Antonov,
[Sparse Matrix Recommender framework interface functions](https://github.com/antononcube/R-packages/tree/master/SparseMatrixRecommenderInterfaces)
(2019),
[R-packages at GitHub](https://github.com/antononcube/R-packages).

[AAp4] Anton Antonov,
[Latent Semantic Analysis Monad in R](https://github.com/antononcube/R-packages/tree/master/LSAMon-R)
(2019),
[R-packages at GitHub](https://github.com/antononcube/R-packages).
