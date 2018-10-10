## What Gaul Has

## Editing Author: oldenkam
## Date: 3/14/2017
## Purpose:
## whatGaulHas is a function that returns a closest string match to a user's string. This function returns that match's
## relavant information. The first argument, country is a correctly spelled and correctly cased Admin 0 name string that subsets
## possible matches to increase spatial accuracy and curate results. The second argument, input is a string the user is seeking
## to validate against our dictionary of existing strings. near is an optional argument that still includes close matches
## even if a exact match was detected. recent is an optional arguement that curates guesses to only the most recent year that it exists.

#########################################################################################################

### Step 1: Script Prep #################################################################################

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

work_dir <- paste0(j, "temp/oldena/projects/AdminDictionary")
setwd(work_dir)

### Step 2: Import Data #################################################################################

### Define function #####################################################################################

whatGaulHas <- function(input, country, near = FALSE, recent = TRUE, strDist = 2){
  if(exists("GaulDict")){
    gauler <- GaulDict
    if(hasArg("input")){
      if(hasArg("country")){
        if(is.numeric(country) & country %in% gauler$ADM0_CODE){
          gauler <- gauler[gauler$ADM0_CODE == country,]
        }else if(country %in% gauler$ADM0_NAME){
          gauler <- gauler[gauler$ADM0_NAME == country,]
        }else{
          message("The value you've passed to country does not correlate with any country in the shapefile library. Try a new value: a new string or number OR do not pass a value to country.")
          break
        }
      }
      input <- as.data.frame(input)
      input$matcher <- str_replace_all(tolower(gsub(" ", "", input$input, fixed = TRUE)), "[[:punct:]]", '')
      input <- as.data.frame(input) %>%
        stringdist_left_join(gauler, by = c("matcher" = "matcher"), max_dist = strDist)
      input <- subset(input, select=c(-matcher.x, -matcher.y, -V1))
    }else{
      message('You are missing the critical argument: input')
    }  
      if(any(!is.na(input$matchfeature.name))){
        if(near == FALSE){
          if(any(input$input == input$matchfeature.name)){
            input <- input[input$input == input$matchfeature.name,]
          }
        }
        if(recent == TRUE){
          input <- do.call(rbind, by(input, input$matchfeature.name, function(x) x[which.max(x$poly_year), ]))
        }
      }else{
        print("No partial or exact match was detected for this input.")
      }
      return(tbl_df(input))
  }else{
      GaulDict <<- fread("GaulDict.csv", stringsAsFactors = FALSE)
      GaulDict$matcher <<- str_replace_all(tolower(gsub(" ", "", GaulDict$matchfeature.name, fixed = TRUE)), "[[:punct:]]", '')
      message('Please rerun script, dictionary has been loaded')
    }
}

