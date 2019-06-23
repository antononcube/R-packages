# R-packages

Miscellaneous R packages. Most correspond to the packages in 
[MathematicaForPrediction](https://github.com/antononcube/MathematicaForPrediction).

## Monadic programming style

Most of the packages provide monadic style pipelines.

For more details see the explanations in the 
[MathematicaForPrediction](https://github.com/antononcube/MathematicaForPrediction)
directory
[Monadic Programming](https://github.com/antononcube/MathematicaForPrediction/tree/master/MonadicProgramming).



## On package licensing

Some loosely followed rules follow for package licenses in this repository follow.

- In general the simple packages have BSD-3 licenses.

  - For example, the package [OutlierIdentifiers](https://github.com/antononcube/R-packages/tree/master/OutlierIdentifiers) 
    is very simple. 

- The package license is based on the license(s) of the most important package(s) it builds upon.

  - If a package is based on one package with MIT license then the package is also with MIT license. 

- If a package is with a GPL-3 license that is because:
 
  - at least one of the underlying packages is with GPL-3,
   
  - the original version of the package was published with GPL-3.

