## GAUL String Dictionary Builder

## Editing Author: oldenkam
## Date: 3/24/2017
## Purpose: 
## Search through existing gaul shapefiles, return compiled list of every feature from every shapefile
## retun important information about feature, such as source/adm_lvl unit. Two important files are produced.
## The first is a complete list of more than a million elements that are time varrying and admin level varying
## The second is a curated list that includes features that exist as they do in their most recent geometry
## and at their highest admin level.

#########################################################################################################

### Step 1: Script Prep #################################################################################

## Set directory and load libraries
rm(list=ls())

if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
pacman::p_load(plyr, dplyr, stringr, rgdal, data.table, stringi, stringr, tidyr)

### Step 2: Run Script ##################################################################################


adm1GaulFiles <- list.files(path="J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin1", pattern="*.dbf$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
adm2GaulFiles <- list.files(path="J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin2", pattern="*.dbf$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
lowerGaulFiles <- list.files(path="J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/lower_levels", pattern="*.dbf$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
#dhsFiles <- list.files(path="J:/WORK/11_geospatial/05_survey shapefile library/Shapefile directory", pattern="*.dbf$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
#dhsFiles <- dhsFiles[grepl("DHS", dhsFiles)]

gaulFiles <- append(adm1GaulFiles, c(adm2GaulFiles, lowerGaulFiles))



i <- 0
l <- vector("list", 1)
for(file in gaulFiles){
  p <- as.data.frame(readOGR(file))
  
  if("GAUL_CODE" %in% colnames(p)){
    p <- subset(p, select=-c(GAUL_CODE))
  }
  
  adm_unit <- (length(grep("CODE$", colnames(p)))) - 1
  p$poly_id_field <- paste0("ADM", adm_unit, "_CODE")
  
  if(adm_unit < 3){
    res <- str_match(file, "g2015_(.*?)_")
    p$poly_year <- res[,2]
  }else{
    print(file)
    p$poly_year <- stri_extract_last_regex(file, "\\d{4}")
  }
  
  p$shapefile.path <- file
  p$matchfeature.name <- p[,which(colnames(p)==sprintf("ADM%s_NAME", adm_unit))]
  p$matchfeature.code <- p[,which(colnames(p)==sprintf("ADM%s_CODE", adm_unit))]
  
  l[[i + 1]] <- p
  i <- i + 1
}

a <- data.frame()
for(frame in l){
  a <- rbind.fill(a, frame)
}

a <- a[order(a$matchfeature.name),]
write.csv(a, "J:/temp/oldena/projects/AdminDictionary/GaulDict.csv")

a<- read.csv("J:/temp/oldena/projects/AdminDictionary/GaulDict.csv")
a$uniquename <- paste(a$ADM0_NAME, a$matchfeature.name, sep=" ")
a <- unique(setDT(a)[order(ADM0_NAME, matchfeature.name, -poly_year, poly_id_field)], by = "uniquename")
a <- subset(a, select=c(-uniquename))
write.csv(a, "J:/temp/oldena/projects/AdminDictionary/GaulDictMaxAdmMaxYear.csv")
