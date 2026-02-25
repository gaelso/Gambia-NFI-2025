
## Download necessary data for the analysis
## Core data is taken for a GG drive folder: 'Gambia-NFI-20205'
## Require invitation
## data list: 
## - NFMA data pre-cooked, see R project "NFI-modules-2021-data" and R-support/
## - CEO data, from CEO project: https://app.collect.earth/review-institution?institutionId=5770
## - CEO corrected data, with internal FAO corrections of most obvious misclassifications.
## - LULC 2023 and admin boundaries: Provided by Gambia forest department
## - ROOTS map, provided by FAO Roots project and extracted from GEE: https://code.earthengine.google.com/8568e16d66b811173e31620a2e47764c
##   (Go to task and export image)

## ancillary data is downloaded from their source:
## - Chave et al. 2014 environmental factor (E) as raster.
## - Chave et al. 2009 wood density database
## > Downloaded from BIOMASS Github repository: https://github.com/umr-amap/BIOMASS/tree/master/data-raw

## Contact: Gaël Sola, FAO.


source("R/get-pkg.R")


##
## Download core data ####
##

if (!"data-core" %in% list.files()){
  googledrive::drive_auth()
  drive_download("Gambia-NFI-2025/data-core.zip", overwrite = T)
  unzip(zipfile = "data-core.zip")
  unlink("data-core.zip")
  unlink("__MACOSX", recursive = T)
}

## 
##  Download ancillary data ####
##

## + Chave 2014 raster E ####

if (!"data-anci" %in% list.files()) dir.create("data-anci")

if (!"E.bil" %in% list.files("data-anci")) {
  download.file(
    url = "https://github.com/umr-amap/BIOMASS/raw/refs/heads/master/data-raw/climate_variable/E.zip",
    destfile = file.path("data-anci", "E.zip")
  )
  unzip(zipfile = "data-anci/E.zip", exdir = "data-anci")
  unlink("data-anci/E.zip")
  
}

## + Get WD at species level ####

if (!"wdData.csv" %in% list.files("data-anci")) {
  download.file(
    url = "https://raw.githubusercontent.com/umr-amap/BIOMASS/refs/heads/master/data-raw/wdData.csv",
    destfile = file.path("data-anci", "wdData.csv")
  )
}



