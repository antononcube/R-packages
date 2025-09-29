##===========================================================
## Receiver Operating Characterisic (ROC) functions in R
##
## BSD 3-Clause License
##
## Copyright (c) 2021, Anton Antonov
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:A
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
## antononcube @@@ posteo ... net,
## Windermere, Florida, USA.
##===========================================================

#' @import purrr
#' @import dplyr
#' @import tidyr
NULL


##===========================================================
## Predicates
##===========================================================

lsROCTypes <- c("TruePositive", "TrueNegative", "FalsePositive", "FalseNegative")

#' ROCVectorQ
#' @description Is an object a vector with ROC names or not.
#' @param x Object
#' @return A logical value
#' @export
ROCVectorQ <- function( x ) {
  ( is.numeric(x) || is.integer(x) || is.list(x) && mean(sapply(x,is.numeric)) == 1) && length( intersect( names(x), lsROCTypes ) ) == length(lsROCTypes)
}

#' ROCDataFrameQ
#' @description Is an object a data frame with ROC columns or not.
#' @param x Object
#' @return A logical value
#' @export
ROCDataFrameQ <- function( x ) {
  is.data.frame(x) && length( intersect( names(x), lsROCTypes ) ) == length(lsROCTypes)
}


##===========================================================
## Core ROC functions
##===========================================================

TPR <- function(rocAssoc) {
  (rocAssoc[["TruePositive"]]) / (rocAssoc[["TruePositive"]] + rocAssoc[["FalseNegative"]])
}

SPC <- function(rocAssoc) {
  (rocAssoc[["TrueNegative"]]) / (rocAssoc[["FalsePositive"]] + rocAssoc[["TrueNegative"]])
}

PPV <- function(rocAssoc) {
  (rocAssoc[["TruePositive"]]) / (rocAssoc[["TruePositive"]] + rocAssoc[["FalsePositive"]])
}

NPV <- function(rocAssoc) {
  (rocAssoc[["TrueNegative"]]) / (rocAssoc[["TrueNegative"]] + rocAssoc[["FalseNegative"]])
}

FPR <- function(rocAssoc) {
  (rocAssoc[["FalsePositive"]]) / (rocAssoc[["FalsePositive"]] + rocAssoc[["TrueNegative"]])
}

FDR <- function(rocAssoc) {
  (rocAssoc[["FalsePositive"]]) / (rocAssoc[["FalsePositive"]] + rocAssoc[["TruePositive"]])
}

FNR <- function(rocAssoc) {
  (rocAssoc[["FalseNegative"]]) / (rocAssoc[["FalseNegative"]] + rocAssoc[["TruePositive"]])
}

ACC <- function(rocAssoc) {
  (rocAssoc[["TruePositive"]] + rocAssoc[["TrueNegative"]]) / sum(rocAssoc)
}

FOR <- function(rocAssoc) { 1 - NPV(rocAssoc) }

F1 <- function(rocAssoc) { 2 * PPV(rocAssoc) * TPR(rocAssoc) / ( PPV(rocAssoc) + TPR(rocAssoc) ) }

AUROC <- function(rocAssoc) { NA }

MCC <- function(rocAssoc) { NA }


##===========================================================
## Dictionaries
##===========================================================

aROCAcronyms <-
      setNames(
        c( "true positive rate", "true negative rate", "specificity", "positive predictive value",
           "negative predictive value", "false positive rate",
           "false discovery rate", "false negative rate", "accuracy", "area under the ROC curve", "false omission rate",
           "F1 score", "Matthews correlation coefficient",
           "same as TPR", "same as PPV", "same as ACC", "same as TPR"),
        c("TPR", "TNR", "SPC", "PPV", "NPV", "FPR", "FDR", "FNR", "ACC", "AUROC", "FOR", "F1", "MCC", "Recall", "Precision", "Accuracy", "Sensitivity")
      )


aROCFunctions <-
  c(
    setNames(
      c(TPR, SPC, SPC, PPV, NPV, FPR, FDR, FNR, ACC, AUROC, FOR, F1, MCC),
      c("TPR", "TNR", "SPC", "PPV", "NPV", "FPR", "FDR", "FNR", "ACC", "AUROC", "FOR", "F1", "MCC")
    ),
    setNames(
      c( TPR, TPR, PPV, ACC, SPC, FPR, TPR, FNR, SPC, FDR, FOR, F1, AUROC, MCC),
      c("Recall", "Sensitivity", "Precision", "Accuracy", "Specificity",
        "FalsePositiveRate", "TruePositiveRate", "FalseNegativeRate", "TrueNegativeRate", "FalseDiscoveryRate",
        "FalseOmissionRate", "F1Score", "AreaUnderROCCurve", "MatthewsCorrelationCoefficient")
    )
  )


##===========================================================
## ROCAcronymsDictionary
##===========================================================

#' ROC acronyms dictionary
#' @description Gives a data frame that is a ROC acronyms dictionary.
#' @return A data frame
#' @export
ROCAcronymsDictionary <- function() {
  data.frame( Acronym = names(aROCAcronyms), Description = aROCAcronyms, stringsAsFactors = FALSE )
}


##===========================================================
## Computation
##===========================================================

#' Compute ROC functions
#' @description Computes specified Receiver Operating Characteristic (ROC)
#' functions over a list or a data frame.
#' @param x A numeric vector or data frame with names:
#' \code{ c("TruePositive", "TrueNegative", "FalsePositive", "FalseNegative" )}.
#' @param rocs A list of ROC functions names
#' @return A data frame
#' @export
ComputeROCFunctions <- function( x, rocs = c( "FPR", "TPR" ) ) {

  if( ! (ROCVectorQ(x) || ROCDataFrameQ(x) ) ) {
    stop( paste0("The first argument is expected to be a numerical list or data frame with names: ", lsROCTypes, "." ), call. = T)
  }

  if( ROCVectorQ(x) ) {
    x <- as.data.frame(as.list(x))
  }

  dfRes <-
    purrr::map_df( split(x, 1:nrow(x)), function(row) {

      setNames(
        purrr::map_dbl( rocs, function(rocFunc) {
          if( !(rocFunc %in% names(aROCFunctions) ) ) {
            stop( paste( "Unknown ROC function", rocFunc,". The known ROC functions are:", paste( names(aROCFunctions), collapse = ", "), "." ), call. = TRUE )
          }
          aROCFunctions[[rocFunc]](row)
        }),
        rocs )

    })

  x <- x[, setdiff(colnames(x), lsROCTypes), drop=F]
  cbind(x, dfRes)
}




##===========================================================
## Computation of core ROC counts
##===========================================================

dfROCTypes <-
  dplyr::tribble(
    ~Actual,     ~Predicted, ~ROCType,
    "0",         "0",        "TrueNegative",
    "1",         "1",        "TruePositive",
    "1",         "0",        "FalseNegative",
    "0",         "1",        "FalsePositive"
  )

#' Compute ROC type counts
#' @description Computes core Receiver Operating Characteristic (ROC)
#' counts over a data frame with predictions data.
#' @param data A data frame
#' @param actualColumnName A string with the actual labels column name.
#' @param probColumnName A string with the column name of the probabilities
#' to assign the "true" label.
#' @param thresholds A numeric vector with numbers between 0 and 1.
#' @param trueLabel A string that is "true" label.
#' @param falseLabel A string that is "false" label.
#' @details If \code{form == "wide"}
#' them it produces a data frame with the columns
#' \code{ c("Threshold", "TruePositive", "TrueNegative", "FalsePositive", "FalseNegative" )}.
#' Otherwise the long form version is returned.
#' @return A data frame
#' @export
ComputeROCTypeCounts <- function( data, actualColumnName, probColumnName,
                                  thresholds = seq(0, 1, 0.05),
                                  falseLabel = '0',
                                  trueLabel = '1',
                                  form = 'wide' ) {


  if( is.logical(data[[actualColumnName]]) ) {
    data[[actualColumnName]] <- as.character(as.numeric(data[[actualColumnName]]))
  }

  if( mean( data[[actualColumnName]] %in% c(falseLabel, trueLabel) ) < 1 ) {
    stop( paste0( "The actual values column '", actualColumnName, "' has values that are not one of the provided true/false labels."), call. = T)
  }

  if( is.null(thresholds) ) {
    thresholds <- seq(0, 1, 0.05)
  }

  if( mean( 0 <= thresholds & thresholds <= 1 ) < 1 ) {
    stop( "The thresholds argument is expected to have values between 0 and 1.", call. = T)
  }

  dfROCTypesLocal <- dplyr::mutate(dfROCTypes, Actual = ifelse( Actual == "0", falseLabel, trueLabel))
  dfROCTypesLocal <- dplyr::mutate(dfROCTypesLocal, Predicted = ifelse( Predicted == "0", falseLabel, trueLabel))

  dfROCTypeCounts <-
    purrr::map_df( thresholds, function(th) {
      dfTemp <-
        cbind(
          data,
          Actual = data[[actualColumnName]],
          Predicted = ifelse( data[[probColumnName]] < th, falseLabel, trueLabel)
        )
      dfGrid <- setNames(expand.grid(c(trueLabel, falseLabel), c(trueLabel, falseLabel)), c("Actual", "Predicted"))
      dfTemp <- xtabs( ~ Actual + Predicted, dfTemp )
      dfTemp <- as.data.frame(dfTemp)
      dfTemp <- tidyr::complete( data = dfTemp, dfGrid, fill = list( Freq = 0) )
      dfTemp <- cbind( Threshold = th, dfTemp)
      dplyr::inner_join(dfTemp, dfROCTypesLocal, by = c("Actual", "Predicted"))
    })

  if ( tolower(form) == "wide" ) {
    dfROCTypeCountsWide <- tidyr::pivot_wider( data = dfROCTypeCounts, id_cols = "Threshold", names_from = "ROCType", values_from = "Freq" )
    dfROCTypeCountsWide[ is.na(dfROCTypeCountsWide) ] <- 0
    dfROCTypeCountsWide
  } else {
    dplyr::rename( .data = dfROCTypeCounts, Count = Freq )
  }
}
