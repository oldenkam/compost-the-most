## What Gaul Has

## Editing Author: oldenkam
## Date: 3/14/2017
## Purpose: 
## whatGaulHas is a function that returns a closest string match to a user's subString arguement. This function returns that match's
## relavant information. The first argument, country is a correctly spelled and correctly cased Admin 0 name string that subsets 
## possible matches to increase spatial accuracy and curate results. The second argument, subString is a string the user is seeking
## to validate against our dictionary of existing strings. includeNear is an optional argument that still includes close matches 
## even if a exact match was detected. recent is an optional arguement that curates only the most recent years

#########################################################################################################

### Step 1: Script Prep #################################################################################

## Set directory and load libraries
rm(list=ls())

if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}

pacman::p_load(data.table, ggplot2, magrittr, reshape2, plyr, dplyr, rgdal, fuzzyjoin)

## OS locals
os <- .Platform$OS.type
if (os == "windows") {
  j <- "J:/"
} else {
  j <- "/home/j/"
}

work_dir <- paste0(j, "temp/oldena/projects/AdminDictionary/")
setwd(work_dir)

### Step 2: Import Data #################################################################################

### Define function #############################################################################

whatGaulHas <- function(country, input, near = FALSE, recent = TRUE, strDist = 2){
  if(exists("GaulDict")){
    if(country %in% GaulDict$ADM0_NAME){    
      gaulSubset <- GaulDict[GaulDict$ADM0_NAME == country,]
      gaulSubset$matcher <- str_replace_all(tolower(gsub(" ", "", gaulSubset$GAUL_NAME, fixed = TRUE)), "[[:punct:]]", '')
      input <- as.data.frame(input)
      input$matcher <- str_replace_all(tolower(gsub(" ", "", input$input, fixed = TRUE)), "[[:punct:]]", '')
      input <- as.data.frame(input) %>%
            stringdist_left_join(gaulSubset, by = c("matcher" = "matcher"), max_dist = strDist)
      input <- subset(input, select=c(-matcher.x, -matcher.y, -V1))
      if(near == FALSE){
        if(any(input$input == input$GAUL_NAME)){
          input <- input[input$input == input$GAUL_NAME,]
        }
      }
      if(recent == TRUE){
        input <- do.call(rbind, by(input, input$GAUL_NAME, function(x) x[which.max(x$year), ]))
      }
      return(tbl_df(input))
    }else{
      print("Country as spelled does not exist in GAUL dictionary, please try again.")
    }
  }else{
      GaulDict <<- fread("GaulDict.csv", stringsAsFactors = FALSE)
      print('Please rerun script, dictionary has been loaded')
    }
}



