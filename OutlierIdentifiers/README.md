# Outlier Identifiers

This script of R functions re-implements this Mathematica package:

It was easier for me to implement in R the one-dimensional outlier detection functions
in [AAp1] than to comprehend the signatures of the different R-libraries that provide/do
outlier detection.

My first introduction to outlier detection was through the book [RP1] -- see the blog post [AA1].

To install use the command:

    devtools::install_github("antononcube/R-packages", subdir = "OutlierIdentifiers") 
    
## References

[AAp1] Anton Antonov, "Implementation of one dimensional outlier identifying algorithms in Mathematica",
[OutlierIdentifiers.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m), 
(2013), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[AA1] Anton Antonov, 
["Outlier detection in a list of numbers"](https://mathematicaforprediction.wordpress.com/2013/10/16/outlier-detection-in-a-list-of-numbers/),
(2013), 
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).

[RP1] Ronald K. Pearson, 
[“Mining Imperfect Data: Dealing with Contamination and Incomplete Records”](https://www.amazon.com/Mining-Imperfect-Data-Contamination-Incomplete/dp/0898715822), 
SIAM, 2005.
