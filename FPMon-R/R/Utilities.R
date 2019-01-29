##================================================================================
##  Functional parsers utilities in R
##  
##  BSD 3-Clause License
##  
##  Copyright (c) 2015, Anton Antonov
##  All rights reserved.
##  
##  Redistribution and use in source and binary forms, with or without
##  modification, are permitted provided that the following conditions are met:
##    
##  * Redistributions of source code must retain the above copyright notice, this
##  list of conditions and the following disclaimer.
##  
##  * Redistributions in binary form must reproduce the above copyright notice,
##  this list of conditions and the following disclaimer in the documentation
##  and/or other materials provided with the distribution.
##  
##  * Neither the name of the copyright holder nor the names of its
##  contributors may be used to endorse or promote products derived from
##  this software without specific prior written permission.
##  
##  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
##  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
##  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
##  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
##  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
##  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
##  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
##  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
##  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
##  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
##  
##  Written by Anton Antonov, 
##  antononcube @ gmail . com,
##  Windermere, Florida, USA.
##  
##================================================================================


##============================================================
## List print-out
##============================================================

#' Convert a list into a pretty string.
#' @param arg A list.
#' @param leftBracket The symbol for the left bracket.
#' @param rightBracket The symbol for the right bracket.
#' @param headLeftBracket The "head" to be used for the left bracket.
#' @param headRightBracket The "head" to be used for the right bracket.
#' @param head A head symbol.
#' @param level A integer used in recursion.
#' @return A string.
#' @details This function is intended to give Mathematica-lists looking results.
#' @export 
ListToString <- function( arg, leftBracket="{", rightBracket="}", headLeftBracket=NULL, headRightBracket=NULL, head=NULL, level=0 ) {
  if ( is.null(arg) ) {
    ""
  } else if( length(arg) == 0 ) {
    "{}" 
  } else {
    res <-
      Reduce( f = function(a,e) {
        pe <- 
          if( is.list(e) ) { 
            ListToString(e, leftBracket = leftBracket, rightBracket = rightBracket ) 
          } else { 
            paste( e, sep = ",", collapse = "" ) 
          }
        if ( is.null(a) ) { pe } else { paste( a, pe, sep="," ) }
      },
      x = arg,
      init = NULL )
    if ( is.vector(arg) && level == 0 && !is.null(headLeftBracket) && !is.null(headRightBracket) ) {
      paste( headLeftBracket, res, headRightBracket, sep = "" )
    } else if ( is.vector(arg) && level == 0 && !is.null(head)) {
      paste( head, "[", res, "]", sep = "" )
    } else if ( is.list(arg)  ) {
      paste( leftBracket, res, rightBracket, sep = "" )
    } else {
      res
    }
  }
}

