

#' Profile by text
#' @description Computes the profile of items in a SMR object that are retrieved
#' with a terms and topics profile derived using an LSA object.
#' @param smrObj An SMR object.
#' @param lsaObj An LSA object.
#' @param text Query text.
#' @param nrecs Number of the top recommended by \code{text} items
#' to be used for computing the profile.
#' @param representByTermsQ Should representation by terms be used or not?
#' @param representByTopicsQ Should representation by topics be used or not?
#' @details Uses the \code{LSAMon} functions:
#' \code{\link{LSAMon::LSAMonRepresentByTerms}} and
#' \code{\link{LSAMon::LSAMonRepresentByTopics}}.
#' Uses the \code{SMRMon} functions:
#' \code{\link{SMRMon::SMRApplyTermWeightFunctions}},
#' \code{\link{SMRMon::SMRMonRecommendByProfile}}, and
#' \code{\link{SMRMon::SMRMonProfile}}
#' @export
ProfileByText <- function( smrObj, lsaObj, tagTypeWeights, text, nrecs = 10, representByTermsQ = TRUE, representByTopicsQ = TRUE ) {

  lsProfileFreeWordsProf <-
    if( nchar(text) == 0 || !representByTermsQ ) {
      NULL
    } else {

      matRes <-
        lsaObj %>%
        LSAMonRepresentByTerms( query = text ) %>%
        LSAMonTakeValue

      lsCSums <- Matrix::colSums(matRes)
      lsProf <- lsCSums[abs(lsCSums) > 0]

      if( length(lsProf) > 0 ) {
        names(lsProf) <- paste0( "Word:", names(lsProf) )
        lsProf <- lsProf[ names(lsProf) %in% colnames(smrObj$M) ]

        if( max(abs(lsProf)) > 0 ) { lsProf <- lsProf / max(abs(lsProf)) }
        rev(sort(lsProf))
      } else {
        NULL
      }
    }

  lsProfileFreeTopicsProf <-
    if( nchar(text) == 0 || !representByTopicsQ ) {
      NULL
    } else {

      matRes <-
        lsaObj %>%
        LSAMonRepresentByTopics( query = text ) %>%
        LSAMonTakeValue

      matRes <- SMRApplyTermWeightFunctions( matRes, "None", "None", "Cosine" )

      lsCSums <- Matrix::colSums(matRes)
      lsProf <- lsCSums[abs(lsCSums) > 0]

      if( length(lsProf) > 0 ) {
        names(lsProf) <- paste0( "Topic:", names(lsProf) )
        lsProf <- lsProf[ names(lsProf) %in% colnames(smrObj$M) ]

        if( max(abs(lsProf)) > 0 ) { lsProf <- lsProf / max(abs(lsProf)) }
        rev(sort(lsProf))
      } else {
        NULL
      }
    }


  lsProfileFreeProf <- c( lsProfileFreeWordsProf, lsProfileFreeTopicsProf )

  if ( length(lsProfileFreeProf) == 0 ) {
    NULL
  } else {

    dfItemsByFreeText <-
      smrObj %>%
      SMRMonApplyTagTypeWeights(
        weights = tagTypeWeights,
        default = 1 ) %>%
      SMRMonRecommendByProfile( profile = lsProfileFreeProf, nrecs = nrecs, normalizeQ = TRUE ) %>%
      SMRMonTakeValue

    dfProfileByFreeText <-
      smrObj %>%
      SMRMonProfile( history = setNames(dfItemsByFreeText, c("Rating", "Index", "Item")), tagTypesQ = TRUE, normalizeQ = TRUE ) %>%
      SMRMonTakeValue

    ## Result
    list( ItemsByText = dfItemsByFreeText, ProfileByText = dfProfileByFreeText)
  }

}
