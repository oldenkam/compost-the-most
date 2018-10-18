##Data munging for Pilsly retention analysis 

##important libraries
library(dplyr)
library(data.table)
library(stringr)

##Create the opposite of %in%
'%!in%' <- function(x, y)
  ! ('%in%'(x, y))

##Load the dose events csv
dose.events <-
  fread('/Users/adamoldenkamp/Desktop/Monthly Reporting/dose events.csv')
dose.events <- tbl_df(dose.events)

nrow(dose.events)

##Load the organization caps, remove these caps from the data cleaning
org.caps <-
  fread('/Users/adamoldenkamp/Desktop/Monthly Reporting/pillsycaps.csv')
nrow(dose.events)

##Turn org.caps into a list of devices that we don't want
org.caps <- org.caps$deviceId

##Take
dose.events <- mutate(dose.events, pilot = deviceId %!in% org.caps)
nrow(dose.events)

dose.events <- filter(dose.events, eventValue == 'TAKE')

dose.events <-
  mutate(dose.events, owner = tolower(owner), drug = tolower(drug))
nrow(dose.events)
dose.events <-
  filter(
    dose.events,
    !str_detect(drug, 'test'),
    !str_detect(drug, 'harvoni'),
    !str_detect(owner, 'pillsy'),
    !str_detect(owner, 'oldenkamp'),
    !str_detect(owner, 'lebrun'),
    !str_detect(owner, 'sipe'),
    !str_detect(owner, 'ramirez'),
    !str_detect(owner, 'lebrun'),
    !str_detect(owner, 'sipe'),
    !str_detect(owner, 'helloarthur'),
    !str_detect(owner, 'chuks')
    )
nrow(dose.events)

fwrite(dose.events, '/Users/adamoldenkamp/Desktop/Monthly Reporting/dose events clean.csv')
