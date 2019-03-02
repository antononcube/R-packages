##===========================================================
## State Monad code generator in R
## Copyright (C) 2019  Anton Antonov
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## Written by Anton Antonov,
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================

#' @import devtools
#' @import magrittr
#' @import purrr
NULL

#' Generate State monad code.
#' @param monadName The monad name (to be used as function prefix); a string.
#' If NULL the stencil code name is used.
#' @param memberNames A character vector wit the names of the monad members.
#' @param monadObjectArgumentName The monad object name used in the generated functions code.
#' @param outputFile The file name to contain the generated monad code.
#' @details There are several stencil code files that are read and the corresponding names
#' replaced. If \code{memberNames} is not NULL then those names are used the setters and takers
#' are generated and state monad unit S3 object has \code{memberNames} as element names.
#' If \code{memberNames} is a character vector with named elements,
#' then element names are the member names and the values are member classes (like "numeric", "list", etc.)
#' If \code{outputFile} (of course) no code is written.
#' @export
GenerateStateMonadCode <- function( monadName,  memberNames = NULL, monadObjectArgumentName = NULL, outputFile = NULL ) {

  if( !( is.character(monadName) && length(monadName) == 1 ) ) {
    stop( "The argument monadName is expected to be a string.", call. = TRUE )
  }

  if( !( is.null(memberNames) || is.character(memberNames) ) ) {
    stop( "The argument memberNames is expected to be character vector or NULL.", call. = TRUE )
  }

  if( !( is.null(monadObjectArgumentName) || is.character(monadObjectArgumentName) && length(monadObjectArgumentName) == 1) ) {
    stop( "The argument monadObjectArgumentName is expected to be a string or NULL.", call. = TRUE )
  }

  coreCode <- textMStateCoreFunctions

  resCode <- gsub( pattern = "MState", replacement = monadName, x = coreCode )

  if( is.character(monadObjectArgumentName) ) {
    resCode <- gsub( pattern = "msObj", replacement = monadObjectArgumentName, x = resCode )
  }

  if( is.character(memberNames) ) {

    ## Modify MStateUnit
    listOfNULLs <- paste( memberNames, "=", rep_len("NULL", length(memberNames)), collapse = ", " )
    resCode <- gsub( pattern = "res <- list( Value = NULL )",
                     replacement = paste( "res <- list( Value = NULL,", listOfNULLs, ")", collapse = " "),
                     x = resCode,
                     fixed = TRUE)

    ## Generate setters
    if( !is.null(names(memberNames)) ) {

      setterResCode <-
        do.call( c,
                 purrr::map( names(memberNames),
                             function(x) GenerateSetterCode( monadName = monadName,
                                                             memberName = x,
                                                             memberClassName = memberNames[[x]],
                                                             monadObjectArgumentName = monadObjectArgumentName ) )
        )

    } else {

      setterResCode <-
        do.call( c,
                 purrr::map( memberNames,
                             function(x) GenerateSetterCode( monadName = monadName,
                                                             memberName = x,
                                                             monadObjectArgumentName = monadObjectArgumentName ) )
        )

    }

    ## Generate takers
    takerResCode <-
      do.call( c,
               purrr::map( memberNames,
                           function(x) GenerateTakerCode( monadName = monadName, memberName = x, monadObjectArgumentName = monadObjectArgumentName ) )
      )

    resCode <- c( resCode, setterResCode, takerResCode)
  }

  if( is.character(outputFile) ) {
    writeLines( text = resCode, con = outputFile )
  }

  resCode
}


#' Generate State monad setter code.
#' @param monadName The monad name (to be used as function prefix); a string.
#' @param memberName The name of the monad member.
#' @param monadObjectArgumentName The monad object name used in the generated functions code.
#' @param memberClassName Member class name (like "character", "numeric", "list", etc.)
#' @export
GenerateSetterCode <- function( monadName, memberName, monadObjectArgumentName = NULL, memberClassName = NULL ) {

  if( !( is.character(monadName) && length(monadName) == 1 ) ) {
    stop( "The argument monadName is expected to be a string.", call. = TRUE )
  }

  if( !( is.character(memberName) && length(memberName) == 1 ) ) {
    stop( "The argument monadName is expected to be a string.", call. = TRUE )
  }

  if( !( is.null(monadObjectArgumentName) || is.character(monadObjectArgumentName) && length(monadObjectArgumentName) == 1) ) {
    stop( "The argument monadObjectArgumentName is expected to be a string or NULL.", call. = TRUE )
  }

  coreCode <- textMStateSetMember

  resCode <- gsub( pattern = "MState", replacement = monadName, x = coreCode )
  resCode <- gsub( pattern = "MEMBER", replacement = memberName, x = resCode )

  if( is.character(monadObjectArgumentName) ) {
    resCode <- gsub( pattern = "msObj", replacement = monadObjectArgumentName, x = resCode )
  }

  if( is.character(memberClassName) ) {
    resCode <- gsub( pattern = "CLASSNAME", replacement = memberClassName, x = resCode )
  }

  resCode
}



#' Generate State monad taker code.
#' @param monadName The monad name (to be used as function prefix); a string.
#' @param memberName The name of the monad member.
#' @param monadObjectArgumentName The monad object name used in the generated functions code.
#' @export
GenerateTakerCode <- function( monadName, memberName, monadObjectArgumentName = NULL ) {

  if( !( is.character(monadName) && length(monadName) == 1 ) ) {
    stop( "The argument monadName is expected to be a string.", call. = TRUE )
  }

  if( !( is.character(memberName) && length(memberName) == 1 ) ) {
    stop( "The argument monadName is expected to be a string.", call. = TRUE )
  }

  if( !( is.null(monadObjectArgumentName) || is.character(monadObjectArgumentName) && length(monadObjectArgumentName) == 1) ) {
    stop( "The argument monadObjectArgumentName is expected to be a string or NULL.", call. = TRUE )
  }

  coreCode <- textMStateTakeMember

  resCode <- gsub( pattern = "MState", replacement = monadName, x = coreCode )
  resCode <- gsub( pattern = "MEMBER", replacement = memberName, x = resCode )

  if( is.character(monadObjectArgumentName) ) {
    resCode <- gsub( pattern = "msObj", replacement = monadObjectArgumentName, x = resCode )
  }

  resCode
}
