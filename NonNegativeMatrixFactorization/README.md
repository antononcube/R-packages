# Implementation of a Non-Negative Matrix Factorization algorithm in R
 
This R-package has Non-Negative Matrix Factorization (NNMF) functions re-programmed from the 
Mathematica package 
[NonNegativeMatrixFactorization.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m),
[AAp1].

The implementation follows the description of the hybrid algorithm
GD-CLS (Gradient Descent with Constrained Least Squares) in the article [FS1]

The functions is based on base R's sparse matrix library `Matrix`.
(The reason I wrote this package is that there was no R library for NNMF using sparse matrices.)

## References

[AAp1] Anton Antonov, "Implementation of the Non-Negative Matrix Factorization algorithm in Mathematica",
[NonNegativeMatrixFactorization.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m),
(2013), 
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[FS1] Shahnaz, F., Berry, M., Pauca, V., Plemmons, R., "Document clustering using nonnegative matrix factorization", (2006),
Information Processing & Management 42 (2), 373-386.
