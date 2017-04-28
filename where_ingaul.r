## Where In Gaul
## Editing Author: oldenkam
## Date: 4/6/2017
## Purpose: 
## whereInGaul() is a function that spatially joins a lat/long pair to a designated shapefile. year is an argument 1990 - 2014 
## that searches our time varrying GAUL upper admin files. lower is a logical arguement that allows the user to index to our lower
## level admin shapefiles. Current itteration of function relies heavily on J: drive folder strucure, purpose is to avoid using 
## ArcGIS when searching our shapefile libraries for the existance of various geometry. Shapefiles are held in memory so recurring
## spatial joins to the same shapefile do not require a reload of the geometry. 

#################################################################################################################################
### Step 1: Script Prep #########################################################################################################
## Set directory and load libraries
rm(list=ls())
if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
pacman::p_load(data.table, rgdal, plyr, dplyr, fuzzyjoin, tidyr, stringr)
## OS locals
os <- .Platform$OS.type
if (os == "windows") {
  j <- "J:/"
} else {
  j <- "/home/j/"
}

work_dir <- paste0(j, "/WORK/11_geospatial/06_original shapefiles/GAUL_admin")
setwd(work_dir)

### Define function #############################################################################################################
where_ingaul <- function(lat, lon, year, lower=FALSE){
  if(hasArg("lat") & hasArg("lon") & !hasArg("year") & !hasArg("lower") & !hasArg("country") & exists("default_shp")){
    message(sprintf("You have a default folder loaded, point is being spatially joined to features from this file: %s%s", work_dir, file_path))
  }else if(lower==FALSE){
    if(!hasArg("year") & !exists("cur_year")){
      message("Please rerun function, initialize shapefile with a year arguement between 1990 - 2014. Or search lower admin levels by passing lower=TRUE.")
    }else if(!hasArg("year") & exists("cur_year")){
      message(sprintf("Spatial join will be done on the GAUL admin2 shapefile from the year %s, your set default.", cur_year))
    }else if(hasArg("year")){
      if(year %in% 1990:2014){
        if(exists("cur_year")){
          if(year == cur_year){
            message("This shapefile year is already loaded in, will proceed with already loaded shapefile.")
          }else{
            cur_year <<- year
            file_path <<- sprintf("admin2/g2015_%s_2/g2015_%s_2.dbf", cur_year, cur_year)
            default_shp <<- readOGR(file_path)
            message(sprintf("Gaul admin2 from the year %s loaded in! You have initialized the function.", cur_year))
          }
        }else{
          cur_year <<- year
          file_path <<- sprintf("admin2/g2015_%s_2/g2015_%s_2.dbf", cur_year, cur_year)
          default_shp <<- readOGR(file_path)
          message(sprintf("Gaul admin2 from the year %s loaded in!", cur_year))
        }
      }else{
        message("Unacceptable year, please input year from 1990-2014")
      }
    }
    if(exists("default_shp") & hasArg("lat") & hasArg( "lon")){
      latlon <- cbind(lon, lat)
      sp <- SpatialPoints(coords = latlon, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      some_var <- over(sp, default_shp)
      some_var$source <- sprintf("%sWORK/11_geospatial/06_original shapefiles/GAUL_admin/%s", j, file_path )
    }else if(!exists(c("lat", "lon"))){
      message("Seems you're missing values for the lat lon variables.")
    }
    if(exists("some_var")){
      return(some_var)
    }
  }else if(lower==TRUE){
    message("It seems you are looking at lower admin levels. This process is slightly different, pay attention to messages.")
    if(hasArg(year)){
      message("No year arguement required for lower shapefiles.")
    }
    l <- list.files("lower_levels/")
    m <- 1
    for(i in l){
      cat(m, " :: ",  i,"\n")
      m <- m + 1
    }
    user_index <- as.numeric(readline(prompt="There are multiple folders here, as listed. Select by index the folder you'd like to search: "))
    lower_path <- sprintf("lower_levels/%s", l[user_index])
    p <- list.files(lower_path, pattern="*.dbf$", full.names = FALSE, recursive = TRUE, include.dirs = TRUE)
    if(length(p) > 1){
      m <- 1
      for(i in p){
        cat(m, " :: ", i,"\n")
        m <- m + 1
      }
      sub_index <- as.numeric(readline(prompt="There are multiple files here, as listed. Select by index the file you'd like to search: "))
      lower_path <- p[sub_index]
    }else{
      lower_path <- p[1]
    }
    if(lower_path %in% p){
      file_path <<- sprintf("lower_levels/%s/%s", l[user_index], lower_path[1])
      default_shp <<- readOGR(file_path)
    }else{
      message("It seems you've incorrectly indexed. Please start over.")
      break
    }
  }
  if(exists("default_shp") & hasArg("lat") & hasArg( "lon")){
    latlon <- cbind(lon, lat)
    sp <- SpatialPoints(coords = latlon, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    some_var <- over(sp, default_shp)
    some_var$source <- sprintf("%sWORK/11_geospatial/06_original shapefiles/GAUL_admin/%s", j, file_path )
  }else if(!hasArg("lat") & !hasArg("lon")){
    message("Seems you're missing values for the lat lon variables.")
  }
  if(exists("some_var")){
    return(some_var)
  }
}

  