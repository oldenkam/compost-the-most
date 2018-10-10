##update new rows
pacman::p_load(data.table,
               ggplot2,
               magrittr,
               stringr,
               dplyr,
               plyr,
               Hmisc,
               dplyr,
               fuzzyjoin,
               sqldf)

'%!in%' <- function(x,y)!('%in%'(x,y))

##read current list of drug names from forest, download this from 'Drugs' tab
forest_drug_df <- fread('/Users/adamoldenkamp/Desktop/dictionary-project/drugs.csv')
forest_drug_vector <- forest_drug_df$name
forest_drug_vector_unique <- unique(forest_drug_vector)
forest_drug_vector_unique <- as.data.frame(forest_drug_vector_unique)
names(forest_drug_vector_unique) <- c('Drug Name')

#read current dictionary drugnames, download this file from the drive so we're appending to most recent dictionary progress
current_drug_dictionary <- fread('/Users/adamoldenkamp/Desktop/dictionary-project/Drug Dictionary - Drug Dictionary.csv')

n <- 1
for (i in forest_drug_vector_unique$'Drug Name'){
  if (i %!in% current_drug_dictionary$'Drug Name') {
    new_drug <- as.data.frame(forest_drug_vector_unique[n,])
    names(new_drug) <- c('Drug Name')
    current_drug_dictionary <- rbind.fill(current_drug_dictionary, new_drug)
    print(n)
  }
  n <- n + 1
}

fwrite(current_drug_dictionary, '/Users/adamoldenkamp/Desktop/dictionary-project/updated_drug_dict.csv')
