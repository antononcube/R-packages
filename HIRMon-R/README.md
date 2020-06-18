# Hub-Item Recommender Monad

The package named, HIRMon-R, stands for **H**ub-**I**tem **R**ecommender **Mon**ad in R."
HIRMon imlements the Hub-Item Dynamic Ranking Algorithm (HIDRA).

HIDRA can be seen as an extension of [PageRank](http://en.wikipedia.org/wiki/PageRank).

The difference is that HIDRA works on a bi-partite graph and multiple rank vectors are
calculated using different biasing based on predicates over the nodes.

For a particular query a linear combination of the closest ranks vectors is used in
order to obtain the response ranks. (That is the dynamic ranking part.)

There are at least two other similar algorithms:
    1. [Hyperlink-Induced Topic Search (HITS)](http://en.wikipedia.org/wiki/HITS_algorithm)
    2. [Topic-Sensitive PageRank](http://en.wikipedia.org/wiki/Topic-Sensitive_PageRank)

This software monad is self-contained implementation based in previous R implementations, \[1,2\].

## References

[1] Anton Antonov,
[Implementation of a Hub-Item Dynamic Ranking Algorithm in R](https://github.com/antononcube/MathematicaForPrediction/blob/master/R/HubItemDynamicRanks.R),
(2015),
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[2] Anton Antonov, 
[Hub-Item Dynamic Ranks R package](https://github.com/antononcube/R-packages/tree/master/HubItemDynamicRanks),
(2019),
[R-packages at GitHub](https://github.com/antononcube/R-packages).
