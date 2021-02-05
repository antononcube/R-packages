##===========================================================
## Receiver Operating Characterisic (ROC) functions in R
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
## antononcube @@@ posteo ... net,
## Windermere, Florida, USA.
##===========================================================

#' @import purrr
NULL


##===========================================================
## Predicates
##===========================================================

lsROCTypes <- c("TruePositive", "TrueNegative", "FalsePositive", "FalseNegative" )

ROCAssociationQ <- function( x ) {
  is.vector(x) && is.numeric(x) && length( intersect( names(x), lsROCTypes ) ) == length(lsROCTypes)
}

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
  (rocAssoc[["TruePositive"]] + rocAssoc[["TrueNegative"]]) / Total[Values[rocAssoc]]
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
#' @description Give a data frame that is a ROC acronyms dictionary.
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

  if( ! (ROCAssociationQ(x) || ROCDataFrameQ(x) ) ) {
    stop( paste0("The first argument is expected to be a numerical list or data frame with names: ", lsROCTypes, "." ), call. = T)
  }

  if( ROCAssociationQ(x) ) {
    x <- as.data.frame(as.list(x))
  }

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

}


