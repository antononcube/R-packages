
#' Install required packages
#' @description Installs required packages
#' @param gitHubOnlyQ Should only the GitHub packages be installed or not?
#' @details The GitHub packages from "antononcube/R-packages" are
#' provide the recommender and LSA objects that showcased in the flexdashboard(s).
#' @export
InstallRequiredPackages <- function( gitHubOnlyQ = FALSE, ... ) {

  if( !gitHubOnlyQ ) {
    install.packages(c("tidyverse", "purrr", "rmarkdown", "shiny", "flexdashboard", "DT", "stopwords", "SnowballC", "irlba", "NutrienTrackeR", "devtools", "feather"))
  }

  #devtools::install_github( repo = "rstudio/d3heatmap", ...)
  devtools::install_github( repo = "antononcube/R-packages", subdir = "RandomDataGenerators", ...)
  devtools::install_github( repo = "antononcube/R-packages", subdir = "ParetoPrincipleAdherence", ...)
  devtools::install_github( repo = "antononcube/R-packages", subdir = "OutlierIdentifiers", ...)
  devtools::install_github( repo = "antononcube/R-packages", subdir = "SparseMatrixRecommender", ...)
  devtools::install_github( repo = "antononcube/R-packages", subdir = "SMRMon-R", ...)
  devtools::install_github( repo = "antononcube/R-packages", subdir = "GNNMon-R", ...)
  devtools::install_github( repo = "antononcube/R-packages", subdir = "SparseMatrixRecommenderInterfacesNoDT", ...)
  devtools::install_github( repo = "antononcube/R-packages", subdir = "NonNegativeMatrixFactorization", ...)
  devtools::install_github( repo = "antononcube/R-packages", subdir = "LSAMon-R", ...)
}
