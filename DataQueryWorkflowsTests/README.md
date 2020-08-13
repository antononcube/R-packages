# Data Query Workflows Tests

This R-package provides data and tests for the data query workflows code generated with the 
[Raku](https://raku.org) 
package 
[DSL::English::DataQueryWorkflows](https://github.com/antononcube/Raku-DSL-English-DataQueryWorkflows), 
\[AAp1\].

It is assumed that Raku and \[AAp1\] are installed. 
Note that the package \[AAp1\] has its own parsing and translation tests.

## Installation

This R-package can be installed with the command:

```r
devtools::install_github( repo = "antononcube/R-packages", subdir = "DataQueryWorkflowsTests" )
```

(If the R-package `devtools` is not installed, then the command above *should* install it.)

The tests are run with the command:

```r
devtools::test()
```

## Data

The package uses Star Wars data taken from the package 
[`dplyr`](https://github.com/tidyverse/dplyr),
which cites as source SWAPI, the Star Wars API, https://swapi.dev/.

There are four data frames:

- `dfStarwars` with characters data

- `dfStarwarsFilms` with the films the characters appeared in

- `dfStarwarsStarships` with the starships the characters piloted

- `dfStarwarsVehicles` with the vehicles the characters piloted

The column `name` is the "key" for all four data frames.
Except `dfStarwars` all data frames are in long format. 

## References

[AAp1] Anton Antonov, 
[DSL::English::DataQueryWorkflows Raku package](https://github.com/antononcube/Raku-DSL-English-DataQueryWorkflows),
(2020),
[Raku-DSL-English-DataQueryWorkflows at GitHub](https://github.com/antononcube/Raku-DSL-English-DataQueryWorkflows).




