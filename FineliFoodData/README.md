# Fineli Food Data Tables

This data package has data frames with the food data collected and organized by 
National Institute for Health and Welfare, Fineli.

For more details see: https://fineli.fi/fineli/en/avoin-data .

Here is an excerpt from that web-page:

> The content of the Fineli database is freely available as open data. The use of the material is governed by the terms and conditions specified by license: Creative Commons Attribution 4.0 (CC-BY 4.0). When used, the material or the service including or utilising the material must be accompanied by information on the original source and the date of the current version as follows: National Institute for Health and Welfare, Fineli.
>
> Fineli is registered trademark of National Institute for Health and Welfare. National Institute for Health and Welfare holds the copyright to the Fineli web site and the Fineli database.
>
> - Data package includes food names in Finnish, in Swedish and in English.
> - Data package contains house-hold measures and portion sizes of the food items.
> - Data format is character-separated values (CSV) in ASCII-files.
> - Data description in file descript.txt
> - Instructions (pdf)
> - Food components (pdf)

In this package the content of Fineli's text file "descript.txt" is copied in ["Descriptions.md"](./notebooks/Descriptions.md).
The original file is placed here: ["descript.txt"](./txt/descript.txt).


## Installation

```r
devtools::install_github("antononcube/R-packages", subdir = "FineliFoodData")
```

## Example code

Load the packages and show summary of its tables:

```r
library(FineliFoodData)
SummarizeTables()
```

Make a dictionary of food class codes to English descriptions:

```r
MakeDictionary("fuclass", nameFrom = "THSCODE", nameTo = "DESCRIPT", lang = "English")
```
