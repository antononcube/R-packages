# Random Data Generators in R

## In brief

This R package implements the function `RandomDataFrame` that generates random data frames.
The design, implementation, and unit tests closely resemble the Mathematica packages [AAp1, AAp2].

The package also has additional random data generation functions: 
`RandomDate`, `RandomPetName`, `RandomPretentiousJobTitle`, `RandomString`, `RandomWord`.

See the document 
["Rapid specification of random tabular datasets generation in R"](https://htmlpreview.github.io/?https://github.com/antononcube/R-packages/blob/master/RandomDataFrameGenerator/notebooks/Rapid-specification-of-random-tabular-datasets-generation-in-R.nb.html)
for a more detailed introduction.
([This is the corresponding Rmd notebook](./notebooks/Rapid-specification-of-random-tabular-datasets-generation-in-R.Rmd).)

**Remark:** Similar functionalities are provided by:

- Python package 
[RandomDataGenerators](https://pypi.org/project/RandomDataGenerators/), [AAp3]

- [Raku](https://raku.org) module
[Data::Generators](https://modules.raku.org/dist/Data::Generators), [AAp4]

**Remark:** All package functions have corresponding functions with the same names in
[Wolfram Function Repository](https://resources.wolframcloud.com/FunctionRepository/).

## Examples

Here is random data frame with 14 rows:

```r
set.seed(556)
RandomDataFrame(14)

#    microfarad    cognizant     schist distillation      silkscreen     smelt    avowal
# 1   128.41662 incapacitate 104.646227    139.82694    stemmatology  84.58968 105.23215
# 2    60.64684    Anaphalis  60.115781     94.78120              AC  80.13380  56.82287
# 3    74.36977        Bevin 105.786429     74.99358     three-sided  96.29678  81.08573
# 4   106.38133  Cynoglossum  86.558457     73.15772      homologous 106.26842  89.79793
# 5   108.22410   cockleburr  96.419656    158.43356         benight  21.46612  88.48630
# 6   112.75338     assignor  96.690191    121.88561       Collinsia  37.87241  57.44803
# 7   108.29561     sinciput 156.283083     85.82031 insignificantly  90.15897 123.91021
# 8   125.81305   adulterant   5.815316    124.43350    unacquainted  83.83798 159.71666
# 9   143.34763   herniation 154.085376     98.29034         formula  87.43241 118.33813
# 10   59.78724      contemn 106.490506    117.72419         Odonata 111.13304 109.65395
# 11   54.83056     handbill  62.201833    100.10282           hoary  73.74650  90.24583
# 12   86.89153           GI 164.618819     42.90729         outrage  95.84622 111.37075
# 13  117.43177       ramble 149.006736    131.44364     headquarter  52.73246 122.40366
# 14   59.40202        jiffy  80.626660     59.43358             CO2 141.99879 112.86204
```

Here is a random data frame with 10 rows, 
column names that generated with `RandomWord`, 
column values generated by with alternating numerical and string generators, 
and specified minimum of non-missing values:

```r
set.seed(169)
RandomDataFrame(
     nrow = 10,
     columnNamesGenerator = RandomWord,
     generators = c(runif, RandomString, RandomPetName, rnorm),
     minNumberOfValues = 50)

#       padder anaerobe      trunks      blain  mincingly sufficiently
# 1  0.3047487       eM       Mitzu  1.1060173 0.67791920         <NA>
# 2  0.1833649      QYV        <NA>  1.1292480 0.02718936          qNM
# 3  0.3633935      Lvz   Mr. Darcy -0.7413958 0.57505996        vOIjG
# 4         NA       CA   Buttercup -0.5036046 0.65113661         rSIm
# 5  0.6189841    mgNsl      Athena  0.7535618 0.51157628          znO
# 6  0.8200943    ztcqa       Piper  0.5253288         NA          VFu
# 7  0.9958427     <NA>   Jo Jo Cat -0.5989310 0.79725662           Xy
# 8  0.6062899     NGlA Ox Kawakubo -0.4286492 0.86762227          kAl
# 9  0.9202223  knwbSrB        <NA>  0.7347059 0.48381914           lf
# 10 0.5919762      xQB        Huso -0.7915235 0.69749880         SVxK
```


## References

[AAp1] Anton Antonov,
[Random Tabular Dataset Mathematica Package](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/RandomTabularDataset.m),
(2020),
[MathematicaForPrediction at GitHub/antononcube](https://github.com/antononcube/MathematicaForPrediction).

[AAp2] Anton Antonov,
[RandomTabularDataset Mathematica unit tests](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/RandomTabularDataset-Unit-Tests.wlt),
(2020),
[MathematicaForPrediction at GitHub/antononcube](https://github.com/antononcube/MathematicaForPrediction).

[AAp3] Anton Antonov,
[RandomDataGenerators Python package](https://pypi.org/project/RandomDataGenerators/),
(2021),
[PyPI](https://pypi.org).

[AAp4] Anton Antonov,
[Data::Generators Raku module](https://modules.raku.org/dist/Data::Generator),
(2021),
[Raku Modules](https://modules.raku.org).