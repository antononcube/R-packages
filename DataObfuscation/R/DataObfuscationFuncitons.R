##===========================================================
## Data obfuscation functions in R
##
## BSD 3-Clause License
##
## Copyright (c) 2019, Anton Antonov
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## * Redistributions of source code must retain the above copyright notice, this
## list of conditions and the following disclaimer.
##
## * Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.
##
## * Neither the name of the copyright holder nor the names of its
## contributors may be used to endorse or promote products derived from
## this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
## CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
## Written by Anton Antonov,
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================
## TODO:
##   1. [ ] Change plyr with purrr and/or dplyr.
##   2. [ ] Handle mapping of phrases not just words.
##   3. [ ] Consider using a specification data frame.
##===========================================================

##---
## Title: Data Obfuscation Functions
## Author: Anton Antonov
## Start date: 2016-10-5
##---

library(purrr)
library(stringr)
library(stringi)


##===========================================================
## Descriptions obfuscation
##===========================================================

#' Descriptions obfuscation.
#' @description Obfuscates transactions descriptions with specified word mapping,
#' preserved words. All other words are mapped to random words with corresponding lengths
#' or random lengths.
#' @param descriptions A character vector.
#' @param mappedWords A character vector with named elements; names are mapped to the elements.
#' @param preservedWords A character list/vector of words that should be kept the same.
#' @param randomWordsQ Should the words that are not in \code{mappedWords} and \code{preservedWords} be randomly mapped?
#' @param randomWordLengthsQ Should the random words have random lengths?
#' @return A character vector with obfuscated descriptions.
#' @export
ObfuscateDescriptions <- function( descriptions,
                                   mappedWords = NULL, preservedWords = NULL,
                                   randomWordsQ = TRUE, randomWordLengthsQ = TRUE )
{

  ## Get all unique words from descriptions
  allWords <- unique( unlist( strsplit( x = descriptions, split = "\\W" ) ) )

  ## Remove preserved words
  allWords <- setdiff( allWords, preservedWords )

  ## Words to randomize
  wordsForRandomMapping <- setdiff( allWords, names(mappedWords) )
  wordsForRandomMapping <- wordsForRandomMapping[ nchar(wordsForRandomMapping) > 1 ]

  ## Random words lengths
  if ( randomWordLengthsQ ) {
    rLengths <- nchar(wordsForRandomMapping)
    rLengthMean <- mean(rLengths)
    if(length(unique(rLengths)) > 1 ) {
      rLengthSD <- sd(rLengths)
    } else {
      rLengthSF <- 3
    }
    rLengths <- round( rnorm( n = length(wordsForRandomMapping), mean = rLengthMean, sd = rLengthSD ) )
    rLengths[ rLengths < 2 ] <- 2
  } else {
    rLengths <- nchar(wordsForRandomMapping)
  }

  ## Word types
  wtypes <-
    purrr::map_chr( wordsForRandomMapping, function(w) {
      if ( grepl( pattern = "^[[:upper:]]*$", x = w ) ) { return("upper") }
      if ( grepl( pattern = "^[[:lower:]]*$", x = w ) ) { return("lower") }
      if ( grepl( pattern = "^[[:alpha:]]*$", x = w ) ) { return("alpha") }
      if ( grepl( pattern = "^[[:digit:]]*$", x = w ) ) { return("digit") }
      if ( grepl( pattern = "^[[:alnum:]]*$", x = w ) ) { return("alnum") }
      "cntrl"
    })

  ## Make random words and put them in a data frame.
  wordRulesDF <-
    purrr::map_df( 1:length(wordsForRandomMapping), function(i) {
      repeat {
        if ( wtypes[i] == "upper" ) {
          rword <- stri_rand_strings(n=1, length=rLengths[i], pattern="[A-Z]")
        } else if ( wtypes[i] == "alpha" || wtypes[i] == "lower" ) {
          rword <- stri_rand_strings(n=1, length=rLengths[i], pattern="[A-Za-z]")
        } else if ( wtypes[i] == "digit" ) {
          rword <- stri_rand_strings(n=1, length=rLengths[i], pattern="[0-9]")
        } else {
          rword <- stri_rand_strings(n=1, length=rLengths[i], pattern="[A-Za-z0-9]")
        }
        if ( ! ( rword %in% mappedWords) ) { break }
      }
      data.frame( Word = wordsForRandomMapping[i], RandomWord = rword, stringsAsFactors = FALSE )
    })


  if ( !is.null(mappedWords) ) {
    ## This can be optimized to use temporary word boundary replacement only for
    ## mapped words that are also in word list to be replaced with random words.
    tempBBStr <- "aXXa"
    wordRulesDF <-
      rbind(
        data.frame( Word = names(mappedWords), RandomWord =  paste0( tempBBStr, mappedWords, tempBBStr ),
                    stringsAsFactors = FALSE ),
        wordRulesDF,
        data.frame( Word = paste0( tempBBStr, mappedWords, tempBBStr ), RandomWord =  mappedWords,
                    stringsAsFactors = FALSE ),
        stringsAsFactors = FALSE  )
  }

  ## Replace
  Reduce(
    f = function( ds, i ) {
      gsub( pattern = paste0("\\b", wordRulesDF$Word[[i]], "\\b"),
            replacement = wordRulesDF$RandomWord[[i]], x = ds )
    },
    x = 1:nrow(wordRulesDF), init = descriptions  )
}


##===========================================================
## Money amounts obfuscation
##===========================================================

#' Amounts obfuscation.
#' @description Changes amounts according to their values.
#' Same amounts at different records would have the same obfuscated values.
#' @param amounts Amounts vector to be randomized.
#' @param randomizationFraction The fraction to randomize with.
#' @param factor A factor to multiply the randomized fractions with.
#' @details All unique amounts are mapped into unique amounts.
#' The original motivation for this function was to obfuscate money amounts.
#' @return A numerical vector.
#' @export
ObfuscateAmounts <- function( amounts, randomizationFraction = 0.1, factor = NULL ) {
  origAmounts <- amounts
  amounts <- amounts[ !is.na(amounts) ]
  uvals <- sort( unique( amounts ) )
  ruvals <- uvals * runif( n = length(uvals), min = 1-randomizationFraction, max = 1+randomizationFraction ) * factor
  ruvals <- round( ruvals, 2 )
  intervalInds <- findInterval( amounts, vec = ifelse( uvals < 0, uvals * 1.001, uvals * 0.999 ) )
  origAmounts[ !is.na(origAmounts) ] <- ruvals[intervalInds]
  origAmounts
}

#' Shuffle amounts by column partition.
#' @description Shuffles one or several columns within subsets of the transactions derived by unique
#' values of specified columns.
#' @param data A data frame of banking transactions.
#' @param amountsColNames A list of column names with amounts to be shuffled.
#' @param splitColNames NULL or a list of column names for splitting the transactions (for \code{\link{ddply}}.)
#' @details The arguments allow the shuffling of several amount columns with the same permutation.
#' Note that the original order of the transactions data frame is lost.
#' @return A data frame.
#' @export
ShuffleAmountsByColumnValuesPartition <- function( data, amountsColNames, splitColNames = NULL ) {

  if( !is.data.frame(data) ) { stop( "Data frame is expected as a first argument (data).", call. = TRUE) }
  if( !is.character(amountsColNames) ) { stop( "A vector of column names is expected as a second argument (amountsColNames).", call. = TRUE) }
  if( !( is.character(splitColNames) || is.null(splitColNames) ) ) {
    stop( "A vector of column names or NULL is expected as a third argument (splitColNames).", call. = TRUE)
  }
  if( length(intersect( names(data), amountsColNames ) ) < length(amountsColNames) ) {
    stop("Not all elements of the second argument (amountsColNames) are in the names of the first (data).", call. = TRUE )
  }

  if ( is.null(splitColNames) ) {

    ## Simple shuffling
    sinds <- sample( 1:nrow(data), nrow(data) )
    data[ , amountsColNames ] <- data[ sinds, amountsColNames ]
    data

  } else {

    ## Append a column with ordering indices
    data <- cbind(data, 1:nrow(data), stringsAsFactors=FALSE)

    ## Shuffling by column values partition
    data <-
      plyr::ddply( data, splitColNames, function(x) {
        sinds <-  sample( 1:nrow(x), nrow(x) )
        x[ , amountsColNames ] <- x[ sinds, amountsColNames ]
        x
      })

    ## Sort according to the last column and remove it
    data[order(data[[ncol(data)]]),1:(ncol(data)-1)]
  }
}

#' Shuffle amounts by regex subsets.
#' @description Shuffles one or several columns within subsets of transactions derived by
#' by adherence to regex patterns.
#' @param data A data frame of banking transactions.
#' @param amountsColNames A list of column names with amounts to be shuffled.
#' @param forPatternsColName NULL or a column name to the values of which the patterns are applied.
#' @param patterns regex patterns to be used do derive subsets from the values of forPatternsColName
#' @details The arguments allow the shuffling of several amount columns with the same permutation.
#' @export
ShuffleAmountsInRegexSubsets <- function( data, amountsColNames, forPatternsColName, patterns ) {
  Reduce(
    f = function(df, pat) {
      patInds <- grep( pattern = pat, x = data[[forPatternsColName]] )
      sinds <-sample( 1:length(patInds), length(patInds) )
      data[ patInds, amountsColNames ] <- data[ patInds[sinds], amountsColNames ]
      data
    },
    x = patterns,
    init = data )
}

##===========================================================
## Addresses obfuscation
##===========================================================
