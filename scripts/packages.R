## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c('tidycensus','tigris','tidyverse','magrittr','data.table','sf','foreach','doSNOW','scales','odbc','colorspace','openxlsx','here','readxl','openxlsx') ## you can add more packages here
lapply(packages, pkgTest)

library(usmap)
library(ggplot2)
library(stringr)
