# Build a dictionary of every exsiting string in our Gaul Shapefiles

library(dplyr)
library(stringr)
library(rgdal)
library(data.table)

#list file paths in the gaul Admin2 folder that end in .shp, i.e. all the shapefiles in this folder
adm1GaulFiles <- list.files(path="J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin1", pattern="*.dbf$", full.names = TRUE,
                            recursive = TRUE, include.dirs = TRUE)
adm2GaulFiles <- list.files(path="J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin2", pattern="*.dbf$", full.names = TRUE,
                            recursive = TRUE, include.dirs = TRUE)
#list file paths in the lower_levels folder that end in .shp, i.e. all the shapefiles in this folder
lowerGaulFiles <- list.files(path="J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/lower_levels", pattern="*.dbf$", full.names = TRUE,
                             recursive = TRUE, include.dirs = TRUE)

gaulFiles <- append(adm1GaulFiles, c(adm2GaulFiles, lowerGaulFiles))

i <- 0
l <- vector("list", 1)
for(file in gaulFiles){
  p <- as.data.frame(readOGR(file))
  p <- rename(p, fuzzy.country_name = ADM0_NAME, fuzzy.country_code = ADM0_CODE)

  if("GAUL_CODE" %in% colnames(p)){
  p <- subset(p, select=-c(GAUL_CODE))
  }
  
  adm_unit <- (length(grep("CODE$", colnames(p)))) - 1
  p$fuzzy.adm_lvl <- adm_unit
  
  if(adm_unit < 3){
    res <- str_match(file, "G2015_(.*?)")
    p$fuzzy.year <- res[,2]
  }else{
    p$fuzzy.year <- 2015
  }
  
  p$fuzzy.source <- file
  p$fuzzy.name <- p[,which(colnames(p)==sprintf("ADM%s_NAME", adm_unit))]
  p$fuzzy.code <- p[,which(colnames(p)==sprintf("ADM%s_CODE", adm_unit))]

  p <- p[,c("fuzzy.name", "fuzzy.code", "fuzzy.country_name","fuzzy.country_code","fuzzy.source", "fuzzy.year","fuzzy.adm_lvl")]
  
  l[[i + 1]] <- p
  i <- i + 1
}

a <- data.frame()
for(frame in l){
  a <- rbind(a, frame)
}

a <- a[order(a$fuzzy.name),]
write.csv(a, "J:/temp/oldena/projects/AdminDictionary/GaulDict.csv")
nrow(a)


a <- read.csv("J:/temp/oldena/projects/AdminDictionary/GaulDict.csv")
a$uniquename <- paste(a$fuzzy.country_name, a$name, sep=" ")
a <- unique(setDT(a)[order(fuzzy.country_name, fuzzy.name, -fuzzy.year, fuzzy.adm_lvl], by = "uniquename")
a <- subset(a, select=c(-uniquename))

write.csv(a, "J:/temp/oldena/projects/AdminDictionary/GaulDictMaxAdmMaxYear.csv")
