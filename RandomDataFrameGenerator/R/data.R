#' English words data
#'
#' @description English words data that correspond to that used in Mathematica's \code{RandomWord}.
#' There are 84923 words of type "Known", 39176 words of type "Common", 329 of type "Stopword".
#'
#' @format A data frame with 84996 rows and 4 columns.
#' \describe{
#'   \item{Word}{English word}
#'   \item{KnownWordQ}{Is the \code{Word} value a known word or not?}
#'   \item{CommonWordQ}{Is the \code{Word} value a common word or not?}
#'   \item{StopWordQ}{Is the \code{Word} value a stop word or not?}
#' }
#' @source \url{github.com}
"dfEnglishWords"
