
####                 Utility functions                  ####



##%######################################################%##
#                                                          #
####             LoadPackages(packageList)              ####
#                                                          #
####           Load and install if necessary            ####
####             a list of packages on CRAN             ####
#                                                          #
####          tidyverse is always loaded last           ####
#                                                          #
##%######################################################%##

styler:::style_active_file() # tidy the layout

loadPackages <- function(packageList) {
  
catchUnwantedReturn <-   lapply(
  packageList,
  function(pkg) {
    ifelse(
      !require(pkg, character.only = TRUE),
      install.packages(pkg, quiet = TRUE),
      require(pkg, character.only = TRUE)
    )
  })
  
  # tidyverse needs to be loaded with the lib.loc option, so can't come in with lapply above
  library(tidyverse, lib.loc = "/Library/Frameworks/R.framework/Versions/4.0/Resources/library")
}
