## code to prepare `individual` dataset goes here
##Setup----
library(dplyr)
source(here::here("R", "geolocate.R"))


##combine individual tables ----
##create paths to inputs (individual table)
raw_data_path <- here::here("data-raw", "wood-survey-data-master")

individual_paths <- fs::dir_ls(fs::path(raw_data_path, "individual"))
## ----- function dir_ls means lists the contents of directory, we then give 
##it path to directpry and we wat that list
##it gives us the whole list of every file in the "individial" file

# read in all individual tables into one
individual <- purrr::map(.x=individual_paths,
                         ~ readr::read_csv(
                           .x,
                           col_types = readr::cols(.default = "c"),
                           show_col_types = FALSE)) %>%
  purrr::list_rbind()
readr::type_convert()

individual %>%
    readr::write_csv(
    file = fs::path(raw_data_path, "vst_individuals.csv")
  )

#will use other code  usethis::use_data(individual, overwrite = TRUE)


# Combine NEON data tables ----
# read in addioinal tables
maptag <- readr::read_csv(
  fs::path(raw_data_path, "vst_mappingandtagging.csv")
) %>%
  select(-eventID)
perplot <- readr::read_csv(
  fs::path(raw_data_path, "vst_perplotperyear.csv"),
  show_col_types = FALSE
) %>%
  select(-eventID)

#Left join tables to individual
individual %<>%
left_join(maptag, by = "individualID",
          suffix = c("", "_map")) %>%
  left_join(perplot,by = "plotID",
            suffix = c("", "_ppl")) %>%
  assertr::assert(
    assertr::not_na, stemDistance, stemAzimuth, pointID,
    decimalLatitude, decimalLongitude
  )

#Geolocate individuals ----
individual <- individual %>% mutate(
  stemLat = get_stem_location(
    decimalLongitude, decimalLatitude,
    stemAzimuth, stemDistance
  )$lat,
  stemLon = get_stem_location(
    decimalLongitude, decimalLatitude,
    stemAzimuth, stemDistance
  )$lon
) %>%
  janitor::clean_names()

#create data directory 

fs::dir_create("data")

individual %>% 
  readr::write_csv(
    here:: here("data", "individual.csv")
  )

#makes new r script usethis::use_r("geolocate")




##copied data##


## code to prepare `individual` dataset goes here
## Setup
library(dplyr)
source(here::here("R", "geolocate.R"))

## Combine individual tables ----
# Create paths to inputs
raw_data_path <- here::here("data-raw", "wood-survey-data-master")
individual_paths <- fs::dir_ls(fs::path(raw_data_path, "individual"))

# read in all individual tables into one
individual <- purrr::map(
  individual_paths,
  ~ readr::read_csv(
    file = .x,
    col_types = readr::cols(.default = "c"),
    show_col_types = FALSE
  )
) %>%
  purrr::list_rbind() %>%
  readr::type_convert()

individual %>%
  readr::write_csv(file = fs::path(raw_data_path, "vst_individuals.csv"))

# Combine NEON data tables ----
# read in additional table
maptag <- readr::read_csv(
  fs::path(
    raw_data_path,
    "vst_mappingandtagging.csv"
  ),
  show_col_types = FALSE
) %>%
  select(-eventID)

perplot <- readr::read_csv(
  fs::path(
    raw_data_path,
    "vst_perplotperyear.csv"
  ),
  show_col_types = FALSE
) %>%
  select(-eventID)

# Left join tables to individual
individual %<>%
  left_join(maptag,
            by = "individualID",
            suffix = c("", "_map")
  ) %>%
  left_join(perplot,
            by = "plotID",
            suffix = c("", "_ppl")
  ) %>%
  assertr::assert(
    assertr::not_na, stemDistance, stemAzimuth, pointID,
    decimalLongitude, decimalLatitude, plotID
  )

# ---- Geolocate individuals_functions ----
individual <- individual %>%
  dplyr::mutate(
    stemLat = get_stem_location(
      decimalLongitude = decimalLongitude,
      decimalLatitude = decimalLatitude,
      stemAzimuth = stemAzimuth,
      stemDistance = stemDistance
    )$lat,
    stemLon = get_stem_location(
      decimalLongitude = decimalLongitude,
      decimalLatitude = decimalLatitude,
      stemAzimuth = stemAzimuth,
      stemDistance = stemDistance
    )$lon
  ) %>%
  janitor::clean_names()
##make sure the function get stem location is actually ran and loaded into envio!


# create data directory
fs::dir_create(here::here("data"))

# write out analytic file
readr::write_csv(individual, here::here("data", "individual.csv"))
