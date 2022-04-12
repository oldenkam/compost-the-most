# Load libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(data.table)

# Read data
demographics <- read_excel(
  "/Users/adamoldenkamp/Desktop/Adam_daata/Demographics.xlsx")
outcomes <- read_excel(
  "/Users/adamoldenkamp/Desktop/Adam_daata/Outcomes_sheet_combined.xlsx")

# Join tables
demog_outs <- merge(x=demographics, y=outcomes, by="DrChPtID", all.x=TRUE)

# Subset to desired columns
keeps <- c("DrChPtID", 
           "DrChDOB", 
           "DrChGender", 
           "DrChRace",
           "WEIGHT",
#           "DrChICD...14",
#           "DrChICD...21",
#           "DrChICD...22",
           "Timestamp",
           "Location",
           "DrChApptProfile.y",
           "IM or IV?",
           "Dose Cleaned",
           # Concentration
           "QIDSScoreOriginal.y",
           "LRDTSScoreOrig.y",
           "GAD7q08",
           "Total Amount of Zofran (needs Cleaned)"
)

df = demog_outs[keeps]

# Dedulicate identical rows
dd = unique(df)

# This section creates the Apt_no column Create column indexes for sort function
pt_id_idx = grep("DrChPtID", colnames(df))
ts_idx = grep("Timestamp", colnames(df))

# Sort timestamps for appointment function to increment
ds = dd[order(xtfrm(dd[,pt_id_idx]), dd[,ts_idx]), ]

# Create dumby column to increment off of and then run cumsum to increment 
# within the group.
ds$one = 1 # Incrementer
ds$Apt_no <- ave(ds$one, ds$DrChPtID, FUN=cumsum) # Cumsum within patient name

# Create total appointment column, which is just an aggregate max of the 
# patient level apt_no joined back to the ds dataframe.
ds_max = ds %>% group_by(DrChPtID) %>% summarise(Apt_no_total = max(Apt_no))
ds <- merge(x=ds, y=ds_max, by="DrChPtID", all.x=TRUE)

# Drop dumby column
ds = subset(ds, select=-c(one))

# Create data and time columns
ds$Date <- as.Date(ds$Timestamp)
ds$Time <- format(ds$Timestamp,"%H:%M:%S")

# Create patient age
ds$Age = as.numeric(difftime(Sys.Date(),ds$DrChDOB, units = "weeks"))/52.25


# Recover and pictor ICD Codes and DrChProvider columns, which were causing
# a data explosion.

icds <- demographics[c("DrChPtID", 
                  "DrChICD...14")]
icdsd = unique(icds)
setDT(icdsd)
icd_codes = icdsd[, .(ICD_Codes = list(unlist(DrChICD...14))), by = DrChPtID]
ds <- merge(x=ds, y=icd_codes, by="DrChPtID", all.x=TRUE)

docs <- demographics[c("DrChPtID",
                     "DrChProvider")]
docsd = unique(docs)
setDT(docsd)
doctors = docsd[, .(Doctors = list(unlist(DrChProvider))), by = DrChPtID]
ds <- merge(x=ds, y=doctors, by="DrChPtID", all.x=TRUE)

# Reorder columns for readability
order <- c("DrChPtID", 
           "Age",
           "DrChDOB", 
           "DrChGender", 
           "DrChRace",
           "WEIGHT",
           "ICD_Codes",
           "Date",
           "Time",
           "Timestamp",
           "Location",
           "Doctors",
           "DrChApptProfile.y",
           "Apt_no",
           "Apt_no_total",
           "IM or IV?",
           "Dose Cleaned",
           # Concentration
           "QIDSScoreOriginal.y",
           "LRDTSScoreOrig.y",
           "GAD7q08",
           "Total Amount of Zofran (needs Cleaned)"
)

dm <- ds[, order]
dm <- apply(dm,2,as.character)

write.csv(dm, '/Users/adamoldenkamp/Desktop/Adam_daata/output/cleaned.csv')