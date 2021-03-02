#' English words data
#'
#' @description English words data that correspond to that used in Mathematica's \code{RandomWord}.
#' There are 84923 words of type "Known", 39176 words of type "Common", 329 of type "Stopword".
#'
#' @format A data frame with 84996 rows and 4 columns.
#' \describe{
#'   \item{Word}{English word.}
#'   \item{KnownWordQ}{Is the \code{Word} value a known word or not?}
#'   \item{CommonWordQ}{Is the \code{Word} value a common word or not?}
#'   \item{StopWordQ}{Is the \code{Word} value a stop word or not?}
#' }
#' @source \url{github.com}
"dfEnglishWords"


#' Pet name counts
#'
#' @description A data frame of animal pet names and corresponding counts of occurrence in the data source.
#' The following species are present: "Cat", "Dog", "Goat", "Pig".
#'
#' @details The pet names are taken from publicly available data of pets license registrations
#' in the years 2015-2020 in Seattle, WA, USA.
#' For extensive data analysis see: Anton Antonov, "Pets licensing data analysis", (2020), WordPress.
#'
#' @format A data frame with 20790 rows and 3 columns.
#' \describe{
#'   \item{Species}{The species of the pet animal.}
#'   \item{Name}{Name of pet animal.}
#'   \item{Count}{Number of times the pet name occurrs in the data source.}
#' }
#' @source \url{https://catalog.data.gov/dataset/seattle-pet-licenses}
"dfPetNameCounts"


#' Pretentious job title words
#'
#' @description Pretentious job title words in different languages.
#'
#' @format A list of list with named elements.
#' \describe{
#'   \item{names}{Language.}
#' }
#' @source \url{https://www.bullshitjob.com/title/}
"aPretentiousJobTitleWords"
