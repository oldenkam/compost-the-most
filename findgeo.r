## Find Existing Geospatial Geometry

## Editing Author: oldenkam
## Date: 3/14/2017
## Purpose:
##
## findgeo()
## findgeo() is a function that searches DHS data, GAUL data, and GADM data location names and returns
## matches based on your search_for string. The first argument, search_for, is a string that the user 
## would like to see if we have lcoations for. The second argument, iso, is an optional argument used to 
## curate possible guesses by only allowing matches within like iso3 codes. The third argument, near, is 
## a logical  argument that will curate away like guesses should an exact match exist to your search_for. 
## The fourth argument, recent is another logical argument that lets you curate away past iterations of 
## a location's geometry. The fifth and final argument, dist, is a numerical argument that sets the 
## string matching tolerance- higher number more results. source is a subsetting argument that takes 
## three inputs, either 'GAUL', 'GADM', or 'codebook' and returns just results from the specified source
## The purpose of this function is to quickly illustrate what features exist in our shapefile library 
## that we may have geometry for, this is not a definitive resource. 
##
## moreInfo()
## findgeo() returns a data table which carries brief information on existing features. Included in that
## table is a variable called more_info_id. 'more_info_id' expands on the observation, returning that
## observation's associated information from the file that it originates from. This function accepts one
## argument, an integer used to index through an appropriate library.

########################################################################################################

### Step 1: Script Prep ################################################################################

## Set directory and load libraries
rm(list=ls())

pacman::p_load(data.table, ggplot2, magrittr, stringr, plyr, Hmisc, dplyr, fuzzyjoin)

## OS locals
os <- .Platform$OS.type
if (os == "windows") {
  j <- "J:/"
} else {
  j <- "/home/j/"
}

work_dir <- paste0(j, "temp/oldena/existingFeatureLibrary")
setwd(work_dir)

### One-time dictionary loader, this will take a few minutes. ##########################################
if(!exists("feature_inventory")){
  feature_inventory <<- fread("existingfeatures.csv", stringsAsFactors = FALSE)
  message('Feature dictionary has been loaded.')
}
if(!exists("codebook_info_features")){
  codebook_info_features <<- fread("codebook_info_features.csv")
  message('Codebook information dictionary has been loaded')
}
if(!exists("GAUL_info_features")){
  GAUL_info_features <<- fread("gaul_info_features.csv")
  message('GAUL information dictionary has been loaded')
}
if(!exists("GADM_info_features")){
  GADM_info_features <<- fread("gadm_info_features.csv")
  message('GADM information dictionary has been loaded')
}



### Define findGeo function ############################################################################
findgeo <- function(search_for, iso, near = TRUE, recent = TRUE, source, dist = 2){
  if(exists("feature_inventory")){
    if(hasArg("search_for")){
      if(hasArg("iso")){
        if(iso %in% feature_inventory$iso3){
          feature_inventory <- feature_inventory[feature_inventory$iso3 == iso,]
        }else{
          message("The value you've passed to iso3 code does not correlate with any country in the "
                  ,"shapefile library. Try a new value.")
          return()
        }
      }
      search_for <- as.data.frame(search_for)
      search_for$matcher <- str_replace_all(tolower(gsub(" ", "", search_for$search_for, fixed = TRUE)),
                                       "[[:punct:]]", '')
      search_for <- as.data.frame(search_for) %>%
        stringdist_left_join(feature_inventory, by = c("matcher" = "matcher"), 
                             max_dist = dist, distance_col = "match_distance")
    }else{
      message('You are missing the critical argument: search_for')
    }  
      if(any(!is.na(search_for$location_name))){
        if(near == FALSE){
          if(any(search_for$matcher.x == search_for$matcher.y)){
            search_for <- search_for[search_for$matcher.x == search_for$matcher.y,]
          }
        }
        if(recent == TRUE){
          search_for$adm_loc <- gsub(" ", "", paste(search_for$source_type, search_for$iso3, 
                                                    search_for$admin_level, search_for$location_name))
          search_for <- do.call(rbind, by(search_for, search_for$adm_loc, 
                                          function(x) x[which.max(x$year), ]))
        }
      }else{
        message("No partial or exact match was detected for this input.")
        return()
      }
    if(hasArg("source")){
      if(source %in% feature_inventory$source_type){
        search_for <- search_for[search_for$source_type == source,]
      }else{
        message('Input to source does not correspond with a possible source value: ',
                'codebook, GADM, and GAUL.')
      }
      
    }
    
    search_for <- search_for[order(search_for$match_distance),]
    search_for <- search_for[,c("source_type", "iso3", "location_name", "admin_level", "year", 
                        "data_path", "more_info_id", "match_distance")]
    return(tbl_df(search_for))
  }else{
    message("Please load libraries at the top.")
  }
}

### Define moreInfo function ###########################################################################
moreinfo <- function(infoID){
    find_info <- feature_inventory[feature_inventory$more_info_id == infoID,]
    if(find_info$source_type == 'codebook'){
      result <- codebook_info_features[codebook_info_features$more_info_id == find_info$more_info_id,]
    }else if(find_info$source_type == 'GAUL'){
      result <- GAUL_info_features[GAUL_info_features$more_info_id == find_info$more_info_id,]
    }else if(find_info$source_type == 'GADM'){
      result <- GADM_info_features[GADM_info_features$more_info_id == find_info$more_info_id,]
    }
    result[result==''] <- NA
    result <- Filter(function(x) !all(is.na(x)), result)
    return(result)
}

