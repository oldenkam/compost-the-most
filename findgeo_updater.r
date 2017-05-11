## Geometry Features Update Library

## Editing Author: oldenkam
## Date: 5/11/2017

### Load Required Packages and Important CSV's##########################################################
if (!require(pacman)) {
  install.packages("pacman")
  require(pacman)
}
pacman::p_load(plyr, dplyr, stringr, rgdal, data.table, stringi, stringr, tidyr, foreign)

iso_key <- fread(file = 'J:/temp/oldena/findgeo/iso3/iso_key.csv',
                 stringsAsFactors = F)
iso <- as.data.table(iso_key[, c('country', 'iso3')])
### Create Codebook Inventory ##########################################################################
# Create list of our codebook library.
codebooks <-
  list.files(
    path = "J:/WORK/11_geospatial/05_survey shapefile library/codebooks",
    pattern = "*.csv$",
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE
  )
# For each codebook in our library create new variables that we can merge on.
codebook_inventory <- data.frame()
for (book in codebooks) {
  page <- as.data.frame(read.csv(book))
  page$data_path <- book
  page$year <- page$end_year
  codebook_inventory <- rbind.fill(codebook_inventory, page)
}
codebook_inventory[codebook_inventory$location_name == '', ] <- NA
codebook_inventory <-
  codebook_inventory[!is.na(codebook_inventory$location_name), ]
codebook_inventory$unicoder <-
  paste(
    codebook_inventory$data_path,
    '_',
    codebook_inventory$iso3,
    '_',
    codebook_inventory$location_name,
    '_',
    codebook_inventory$year,
    '_',
    codebook_inventory$point,
    '_',
    codebook_inventory$lat,
    '_',
    codebook_inventory$admin_level,
    '_',
    codebook_inventory$shapefile
  )
codebook_inventory <-
  codebook_inventory[!duplicated(codebook_inventory$unicoder), ]

### Create GAUL Inventory ##############################################################################
# Create list of our GAUL shapefiles
admin_1_gaulfiles <-
  list.files(
    path = "J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin1",
    pattern = "*.shp$",
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE
  )
admin_2_gaulfiles <-
  list.files(
    path = "J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin2",
    pattern = "*.shp$",
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE
  )
low_gaulfiles <-
  list.files(
    path = "J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/lower_levels",
    pattern = "*.shp$",
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE
  )
all_gaulfiles <-
  append(admin_1_gaulfiles, c(admin_2_gaulfiles, low_gaulfiles))
# For each shapefile in our GAUL library convert them to a dataframe then add new variables
i <- 1
gaul_inventory <- data.frame()
list_gaul_dataframes <- vector("list", 1)
for (file in all_gaulfiles) {
  shape <- as.data.frame(readOGR(file))
  if ("GAUL_CODE" %in% colnames(shape)) {
    shape <- subset(shape, select = -c(GAUL_CODE))
  }
  adm_unit <- (length(grep("CODE$", colnames(shape)))) - 1
  shape$poly_id_field <- paste0("ADM", adm_unit, "_CODE")
  if (adm_unit < 3) {
    res <- str_match(file, "g2015_(.*?)_")
    shape$year <- res[, 2]
  } else{
    print(file)
    shape$year <- stri_extract_last_regex(file, "\\d{4}")
  }
  shape$admin_level <- adm_unit
  shape$data_path <- file
  shape$location_name <-
    as.character( 
      shape[, which(colnames(shape) == sprintf("ADM%s_NAME", adm_unit))]
    )
  
  gaul_inventory <- rbind.fill(gaul_inventory, shape)
  i <- i + 1
}
gaul_inventory <-
  as.data.table(left_join(gaul_inventory, iso, by = c("ADM0_NAME" = "country")))

## Create GADM Inventory ###############################################################################
# Create list of our GADM shapefiles
all_gadmfiles <-
  list.files(
    path = "J:/WORK/11_geospatial/06_original shapefiles/GADM/gadm28_levels_shp",
    pattern = "*.shp$",
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE
  )
all_gadmfiles <- all_gadmfiles[2:6]
# For each shapefile in our GADM library transform it to a dataframe then create new useful variables
gadm_inventory <- data.frame()
for (file in all_gadmfiles) {
  shape <- as.data.frame(readOGR(file))
  adm_unit <-
    max(unique(as.numeric(unlist(
      strsplit(gsub("[^0-9]", "", unlist(
        paste(colnames(shape),
              collapse = ',')
      )), "")
    ))))
  shape$admin_level <- adm_unit
  shape$location_name <- shape[[paste0("NAME_", adm_unit)]]
  shape$data_path <- file
  shape$year <- 2017
  shape$iso3 <- shape$ISO
  gadm_inventory <- rbind.fill(gadm_inventory, shape)
}

### Bind GADM, GAUL, Codebooks into Massive Single Library #############################################
codebook_inventory$source <- 'codebook'
gadm_inventory$source <- 'gadm'
gaul_inventory$source <- 'gaul'

existing_inventory <-
  rbind.fill(gadm_inventory, gaul_inventory, codebook_inventory)
existing_inventory$more_info_id <- rev(1:nrow(existing_inventory))

codebook_inventory <- existing_inventory[existing_inventory$source == 'codebook', ]
gadm_inventory <- existing_inventory[existing_inventory$source == 'gadm', ]
gaul_inventory <- existing_inventory[existing_inventory$source == 'gaul', ]


existing_inventory$matcher <- str_replace_all(tolower(gsub(
  " ", "",
  existing_inventory$location_name, fixed = TRUE
)), "[[:punct:]]", '')
existing_inventory <-
  existing_inventory[, c(
    "source",
    "iso3",
    "location_name",
    "admin_level",
    "year",
    "data_path",
    "more_info_id",
    "matcher"
  )]

fwrite(existing_inventory, "J:/temp/oldena/findgeo/existing_inventory.csv")

fwrite(codebook_inventory, "J:/temp/oldena/findgeo/codebook_inventory.csv")
fwrite(gadm_inventory, "J:/temp/oldena/findgeo/gadm_inventory.csv")
fwrite(gaul_inventory, "J:/temp/oldena/findgeo/gaul_inventory.csv")
