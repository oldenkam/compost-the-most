## What Gaul Has
## Editing Author: oldenkam
## Date: 3/14/2017
## Purpose: 
## whereInGaul() is a function that returns the Gaul Code, Country name of an area given a lat/long pair. Year is an optional
## argument 1990 - 2014 that searches our time varrying GAUL files. lowAdmin is a logical arguement that searches our lower admin 
## shapefiles folder for admin3 - admin 5 shapefiles. Current itteration of function relies heavily on J: drive folder strucure, 
## purpose is to avoid using ArcGIS when searching for smallest polygon available given a lat long coord. Useful for validating.

#########################################################################################################
### Step 1: Script Prep #################################################################################
## Set directory and load libraries
rm(list=ls())
if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
pacman::p_load(plyr, dplyr, rgdal)
## OS locals
os <- .Platform$OS.type
if (os == "windows") {
  j <- "J:/"
} else {
  j <- "/home/j/"
}

work_dir <- paste0(j, "temp/oldena/projects/AdminDictionary/admRdata")
setwd(work_dir)

### Define function #############################################################################
whereInGaul <- function(lat, lon, year=2014, lowAdmin=FALSE){
  require(rgdal)
  latlon <- cbind(lon, lat)
  sp <- SpatialPoints(coords = latlon, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  results <- vector("list", 5)
  if(lowAdmin == FALSE){
    pathAdmin <- sprintf("g2015_%s_2.Rdata", year)
  }else if(lowAdmin == TRUE){
    guess <- as.numeric(readline(prompt="Do you know what country you're looking in? 1 for 'yes', 0 for 'no': "))
    if(guess == 1){
      tarCountry <- readline(prompt="Correctly spell the country you are looking in: ")
    }else if(guess == 0){
      countryShp <- load(sprintf("g2015_%s_2.Rdata", year))
      some_var <- over(sp, countryShp)
      tarCountry <- some_var$ADM0_NAME
    }
    pathAdmin <- sprintf("/g2015_%s_LowerLevels/", tarCountry)
    l <- list.files(pathAdmin, pattern="*.Rdata$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
    if(length(l) > 1){
      print(l)
      tarShp <- as.numeric(readline(prompt="There are multiple files here, as listed. Select by index the file you'd like to search: "))
      pathAdmin <- l[tarShp]
    }else if(length(l) == 1){
      pathAdmin <- l[1]
    }
  }
  adm_count <- 0
  myShp <- load(pathAdmin)
  some_var <- over(sp, myShp)
  while (adm_count <= length(results)){
    if(sprintf("ADM%s_CODE", adm_count) %in% colnames(some_var)){
      results[[adm_count + 1]] <- sprintf("Admin Level: %s GAUL Code: %s Target Name: %s", adm_count, some_var[sprintf("ADM%s_CODE", adm_count)], some_var[,sprintf("ADM%s_NAME", adm_count)])
      adm_count <- adm_count + 1
    } else {
      results[[adm_count + 1]] <- NULL
      adm_count <- adm_count + 1
    }
  }
  for (result in Filter(Negate(is.null), results)){
    print(result)
  }
}

lat <- 8.8080
lon <- 41.6012