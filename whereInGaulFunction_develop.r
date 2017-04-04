## What Gaul Has
## Editing Author: oldenkam
## Date: 3/14/2017
## Purpose: 
## whereInGaul() is a function that returns the Gaul Code, Country name of an area given a lat/long pair. a_year is an optional
## argument 1990 - 2014 that searches our time varrying GAUL files. lower is a logical arguement that searches our lower admin 
## shapefiles folder for admin3 - admin 5 shapefiles. Current itteration of function relies heavily on J: drive folder strucure, 
## purpose is to avoid using ArcGIS when searching for smallest polygon available given a lat long coord. Useful for validating.

############################################################################################################
### Step 1: Script Prep ####################################################################################
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

### Define function #########################################################################################
whereInGaul <- function(lat, lon, a_year, lower=FALSE, lower_country){
  if(lower==FALSE){
    if(!hasArg("a_year") & !exists("cur_a_year")){
      message("Please rerun function, initialize shapefile with a year arguement between 1990 - 2014.")
    }else if(!hasArg("a_year") & exists("cur_a_year")){
      message(sprintf("Spatial join will be done on the GAUL admin2 shapefile from the year %s, your set default.", cur_a_year))
    }else if(hasArg("a_year")){
      if(a_year %in% 1990:2014){
        if(exists("cur_a_year")){
          if(a_year == cur_a_year){
            message("This shapefile year is already loaded in, will proceed with already loaded shapefile.")
          }else{
            cur_a_year <<- a_year
            file_path <<- sprintf("admin2/g2015_%s_2/g2015_%s_2.dbf", cur_a_year, cur_a_year)
            default_shp <<- readOGR(file_path)
            message(sprintf("Gaul admin2 from the year %s loaded in! You have initialized the function.", cur_a_year))
          }
        }else{
          cur_a_year <<- a_year
          file_path <<- sprintf("admin2/g2015_%s_2/g2015_%s_2.dbf", cur_a_year, cur_a_year)
          default_shp <<- readOGR(file_path)
          message(sprintf("Gaul admin2 from the year %s loaded in! You have initialized the function.", cur_a_year))
        }
      }else{
        message("Unacceptable year, please input year from 1990-2015")
      }
    }
    if(exists(c("default_shp", "lat", "lon"))){
      latlon <- cbind(lon, lat)
      sp <- SpatialPoints(coords = latlon, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      some_var <- over(sp, default_shp)
      some_var$source <- sprintf("SOURCE: %s/WORK/11_geospatial/06_original shapefiles/GAUL_admin/%s", j, file_path )
    }else{
      message("Seems you're missing values for the lat lon variables.")
    }
    if(exists("some_var")){
      return(some_var)
    }else{
      message("Capoots! Please initialize function with a year. If you are not looking for upper admin level results, please proved TRUE to the lower argument.")
    }
  }else if(lower==TRUE){
    if(!exists("country_names")){
      country_names <<- fread('J:/temp/oldena/projects/AdminDictionary/ADM0.csv')
    }
    message("It seems you are looking at lower admin levels. This process is slightly different, pay attention to messages.")
    if(hasArg(a_year)){
      message("No year arguement required for lower shapefiles.")
    }
    if(hasArg(lower_country)){
      if(lower_country %in% country_names$ADM0_CODE){
        country <- country_names[country_names$ADM0_CODE == lower_country,]$ADM0_NAME
        country <- gsub(" ", "", country, fixed = TRUE)
      }else if(lower_country %in% country_names$ADM0_NAME){
        country <- lower_country
        country <- gsub(" ", "", country, fixed = TRUE)
      }else{
        message("There has been no match to the name or code that you have provided, ensure you have spelled the name correctly.")
      }
      if(exists("country")){
        lower_path <- sprintf("lower_levels/g2015_%s_LowerLevels/", country)
        l <- list.files(lower_path, pattern="*.dbf$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
        if(length(l) > 0){
          if(length(l) > 1){
            print(l)
            file_list <- as.numeric(readline(prompt="There are multiple files here, as listed. Select by index the file you'd like to search: "))
            lower_path <- l[file_list]
          }
          default_shp <<- readOGR(lower_path[1])
          latlon <- cbind(lon, lat)
          sp <- SpatialPoints(coords = latlon, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
          some_var <- over(sp, default_shp)
        }else{
          message("There seems to be no lower admin shapefiles for this country, there are many reasons that this could be.")
        }
        
      }
    }else{
      message("This process requires either an ADM0_CODE or case and spelling correct ADM0_NAME of the country you're looking to spatially join.")
    }
    if(exists("some_var")){
      return(some_var)
    }else{
      print('ya goofed!')
    }
  }
}






  
  lat <- 8.8080
  lon <- 41.6012
  