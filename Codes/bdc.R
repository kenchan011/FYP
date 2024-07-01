#bdc
library(bdc)
library(rnaturalearth)
library(readr)
library(tidyverse)

metadata <-
  read.csv('mirid_metadata.csv')

#### Path to the folder containing the example datasets. Can ignore ####
path <- "C:/Users/kendr/OneDrive - National University of Singapore/NUS Documents/FYP/R codes"

# Change in the Configuration table the path to the folder in your computer containing the example datasets
metadata$fileName <-
  gsub(pattern = "https://raw.githubusercontent.com/brunobrr/bdc/master/inst/extdata/input_files/",
       replacement = path,
       x = metadata$fileName)


#### Pre-Filter: Cleaning data ####
database <-
  bdc_standardize_datasets(metadata = metadata,
                           format = "csv",
                           overwrite = TRUE,
                           save_database = TRUE)

#30435 records

#Flag missing scientific names
check_pf <-
  bdc_scientificName_empty(
    data = database,
    sci_name = "scientificName")

#Flag records missing partial or complete info on geographic coordinates
check_pf <- bdc_coordinates_empty(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

#Flag records with out-of-range coordinates
check_pf <- bdc_coordinates_outOfRange(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

# Check record sources of your dataset using:
# check_pf %>%
#   dplyr::group_by(basisOfRecord) %>%
#   dplyr::summarise(n = dplyr::n())

check_pf <- bdc_basisOfRecords_notStandard(
  data = check_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")

#Deriving country names for records missing country names
check_pf <- bdc_country_from_coordinates(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country")

#Standardising country names and getting country code information
check_pf <- bdc_country_standardized(
  data = check_pf,
  country = "country"
)

#Correcting latitude and longitude transposed
check_pf <-
  bdc_coordinates_transposed(
    data = check_pf,
    id = "database_id",
    sci_names = "scientificName",
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    country = "country",
    countryCode = "countryCode",
    border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
    save_outputs = FALSE
  )

#Checking records outside region of interest
check_pf <-
  bdc_coordinates_country_inconsistent(
    data = check_pf,
    country_name = c("Indonesia",'Malaysia','Singapore','Philippines','Brunei',
                     'Laos','Vietnam','Myanmar','Thailand','Cambodia',
                     'India','Nepal','Sri Lanka','Bhutan','Bangladesh',
                     'Australia'),
    country = "country_suggested",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.1 # in decimal degrees (~11 km at the equator)
  )

#Identify records not georeferenced but containing locality information
xyFromLocality <- bdc_coordinates_from_locality(
  data = check_pf,
  locality = "locality",
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  save_outputs = FALSE
)

#Generate report
check_pf <- bdc_summary_col(data = check_pf)
report <-
  bdc_create_report(data = check_pf,
                    database_id = "database_id",
                    workflow_step = "prefilter",
                    save_report = TRUE)

report

#Generate figures to interpret cleaning results
figures <-
  bdc_create_figures(data = check_pf,
                     database_id = "database_id",
                     workflow_step = "prefilter",
                     save_figures = FALSE)

# Check figures using
figures$.coordinates_empty


#### Taxonomy cleaning ####

# Clean and parse species names
db <- 
  read.csv(here::here("Output/Intermediate/00_merged_database.csv"))

parse_names <-
  bdc_clean_names(sci_names = db$scientificName, save_outputs = FALSE) #doesn't standardise/capitalise names?

#Merge parsed names with database
parse_names <-
  parse_names %>%
  dplyr::select(.uncer_terms, names_clean)

db <- dplyr::bind_cols(database, parse_names)

# Harmonising species names
query_names <- bdc_query_names_taxadb(
  sci_name            = db$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Miridae", # a taxonomic rank
  rank                = "family", # name of the taxonomic rank
  parallel            = FALSE, # should parallel processing be used?
  ncores              = 2, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)

#Merging harmonised taxonomy with database
db <-
  db %>%
  dplyr::rename(verbatim_scientificName = scientificName) %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., query_names)

report <-
  bdc_create_report(data = db,
                    database_id = "database_id",
                    workflow_step = "taxonomy",
                    save_report = TRUE)

report #accepted names: 21580; 6009 records unaccepted or uninterpretable names or uncertain

unresolved_names <- 
  bdc_filter_out_names(data = db,
                       col_name = 'notes',
                       taxonomic_status = 'accepted',
                       opposite = TRUE)
unresolved_names %>%
  readr::write_csv(., here::here("Output/Check/02_unresolved_names.csv"))

#Save database
# use qs::qsave() to save the database in a compressed format and then qs:qread() to load the database
db %>%
  readr::write_csv(.,
                   here::here("Output", "Intermediate", "02_taxonomy_database.csv"))

#### Space ####
db1 <-
  readr::read_csv(here::here("Output/Intermediate/02_taxonomy_database.csv"))

# First clean db1 to remove records with NA in coordinate columns
db1 <- db1 %>% 
  filter(!is.na(decimalLatitude) | !is.na(decimalLongitude)) #17452 non-NA coordinates

#Identify records with off-coordinate precision
check_space <-
  bdc_coordinates_precision(
    data = db1,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    ndec = c(0, 1) # number of decimals to be tested
  )

#Use CoordinateCleaner to flag common spatial issues
check_space <-
  CoordinateCleaner::clean_coordinates(
    x =  check_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = ,
    tests = c(
      "capitals",     # records within 2km around country and province centroids
      "centroids",    # records within 1km of capitals centroids
      "duplicates",   # duplicated records
      "equal",        # records with equal coordinates
      "gbif",         # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 100m of zoo and herbaria
      "outliers",     # outliers
      "zeros",        # records with coordinates 0,0
      "urban"         # records within urban areas
    ),
    capitals_rad = 2000,
    centroids_rad = 1000,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 100, # remove zoo and herbaria within 100m
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 1000,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    # seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid" # result of tests are appended in separate columns
  )
#flagged 14147/17452 records

check_space <- bdc_summary_col(data = check_space)

# Mapping spatial errors
check_space %>%
  dplyr::filter(.summary == FALSE) %>% # map only records flagged as FALSE
  bdc_quickmap(
    data = .,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    col_to_map = ".summary",
    size = 0.9
  ) #everywhere has problems

report <-
  bdc_create_report(data = check_space,
                    database_id = "database_id",
                    workflow_step = "space",
                    save_report = TRUE)

report #13845 duplicated coordinates per species -- biggest problem

figures <-
  bdc_create_figures(data = check_space,
                     database_id = "database_id",
                     workflow_step = "space",
                     save_figures = FALSE) # not working

# Check figures using
figures$.rou

#Saving space-cleaned database
check_space %>%
  readr::write_csv(.,
                   here::here("Output", "Intermediate", "03_space_database.csv"))


#### Time ####

#Read database
db2 <-
  readr::read_csv(here::here("Output/Intermediate/03_space_database.csv")) #17452 records

#Flag records without event date information
check_time <-
  bdc_eventDate_empty(data = database, eventDate = "verbatimEventDate") #flagged 16742 records

#Extract 4-digit years from event date
check_time <-
  bdc_year_from_eventDate(data = check_time, eventDate = "verbatimEventDate")

#Flag records with out-of-range event date, default = 1900
check_time <-
  bdc_year_outOfRange(data = check_time,
                      eventDate = "year",
                      year_threshold = 1900)
#Generate report
check_time <- bdc_summary_col(data = check_time)

report <-
  bdc_create_report(data = check_time,
                    database_id = "database_id",
                    workflow_step = "time",
                    save_report = TRUE)

report #16771 records flagged

#Figure
figures <-
  bdc_create_figures(data = check_time,
                     database_id = "database_id",
                     workflow_step = "time",
                     save_figures = FALSE) #works?

#Check figures using
figures$year

#Saving database with all data quality tests results
check_time %>%
  readr::write_csv(.,
                   here::here("Output", "Intermediate", "04_time_database.csv"))

#### Filtering database ####

#Remove potentially erroneous or suspicious data flagged by tests
output <-
  check_time %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all") #13664 records remaining

#Saving 'cleaned' output as csv database
output %>%
  readr::write_csv(.,
                   here::here("Output", "Intermediate", "05_cleaned_database.csv"))

#Output still contains a lot of 'Heteroptera' or 'Miridae' names
db3 <- readr::read_csv(here::here("Output/Intermediate/05_cleaned_database.csv"))

db3 %>%
  count(scientificName == 'Miridae'|scientificName == 'Heteroptera'|scientificName == 'miridae')
# 8336 'Miridae' or 'Heteroptera' or 'miridae' records; 1204 NA records

#Remove 'Miridae', 'Heteroptera' and NA records
db3 <- db3 %>%
  filter(!scientificName == 'Miridae' & 
           !scientificName == 'Heteroptera' &
           !scientificName == 'miridae' &
           !is.na(scientificName))

#4124 records remaining

#Save output
db3 %>%
  write_csv('006_cleaned_database2.csv')

