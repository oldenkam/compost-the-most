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

pacman::p_load(dplyr, stringr, rgdal, data.table)

### Step 2: Run Script ##################################################################################


adm1GaulFiles <- list.files(path="J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin1", pattern="*.dbf$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
adm2GaulFiles <- list.files(path="J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin2", pattern="*.dbf$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
lowerGaulFiles <- list.files(path="J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/lower_levels", pattern="*.dbf$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

gaulFiles <- append(adm1GaulFiles, c(adm2GaulFiles, lowerGaulFiles))

i <- 0
l <- vector("list", 1)
for(file in gaulFiles){
  p <- as.data.frame(readOGR(file))
  
  if("GAUL_CODE" %in% colnames(p)){
    p <- subset(p, select=-c(GAUL_CODE))
  }
  
  adm_unit <- (length(grep("CODE$", colnames(p)))) - 1
  p$fuzzy.adm_lvl <- adm_unit
  
  if(adm_unit < 3){
    res <- str_match(file, "g2015_(.*?)_")
    p$fuzzy.year <- res[,2]
  }else{
    p$fuzzy.year <- 2015
  }
  
  p$fuzzy.source <- file
  p$fuzzy.name <- p[,which(colnames(p)==sprintf("ADM%s_NAME", adm_unit))]
  p$fuzzy.name_code <- p[,which(colnames(p)==sprintf("ADM%s_CODE", adm_unit))]
  p <- rename(p, fuzzy.country = ADM0_NAME, fuzzy.country_code = ADM0_CODE)
  
  p <- p[,c("fuzzy.name", "fuzzy.name_code", "fuzzy.country","fuzzy.country_code","fuzzy.source", "fuzzy.year","fuzzy.adm_lvl")]
  
  l[[i + 1]] <- p
  i <- i + 1
}

a <- data.frame()
for(frame in l){
  a <- rbind(a, frame)
}

a <- a[order(a$fuzzy.name),]
write.csv(a, "J:/temp/oldena/projects/AdminDictionary/GaulDict.csv")

a<- read.csv("J:/temp/oldena/projects/AdminDictionary/GaulDict.csv")
a$uniquename <- paste(a$fuzzy.country, a$fuzzy.name, sep=" ")
a <- unique(setDT(a)[order(fuzzy.country, fuzzy.name, -fuzzy.year, fuzzy.adm_lvl)], by = "uniquename")
a <- subset(a, select=c(-uniquename))
write.csv(a, "J:/temp/oldena/projects/AdminDictionary/GaulDictMaxAdmMaxYear.csv")
