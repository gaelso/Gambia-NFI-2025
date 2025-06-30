
use_package <- function(.pkg_name) {
  pkg_name <- as.character(substitute(.pkg_name))
  if (!require(pkg_name, character.only = T,  quietly = TRUE)){
    install.packages(pkg_name, dep =TRUE)
    library(pkg_name, character.only = T, quietly = TRUE)
  } 
}

use_package(googledrive)
use_package(sf)
use_package(terra)
use_package(geosphere)
use_package(tmap)
use_package(tidyterra)
use_package(tidyverse)

## Package setup
ggplot2::theme_set(theme_bw())
options(readr.show_col_types = FALSE)
tmap_mode("view")