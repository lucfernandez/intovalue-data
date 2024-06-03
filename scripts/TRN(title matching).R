## Script to compile all TRNs and titles of IV + EU trials to then sort by country/sponsor/etc for title matching
# Generates pairings of EU and IV trials and their respective titles based on title matching algorithm

##########################################################

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(fs)
library(lubridate)
library(stringr)
library(stringdist)
library(ctregistries)
library(cli)

dir_raw <- here("data", "raw")
dir_processed <- here("data", "processed")

# Get IntoValue TRNs and titles. For trials registered in ClinicalTrials.gov, this is the 'brief_title'. See IntoValue codebook: https://github.com/maia-sh/intovalue-data/blob/main/data/processed/codebook.csv
# Trials in the DRKS already have their full titles linked here, but ClinicalTrials.gov trials will need to get their full titles from 'studies.csv'
trials <- read_csv(path(dir_processed, "trials.csv"))

# Download EU trial data dump, includes TRNs and full titles
EU_dump <- read_csv(path(dir_raw, "euctr_euctr_dump-2024-02-03-054239.csv"))

# ClinicalTrials.gov includes a brief_title and an official_title.
# For title matching with trials in ClinicalTrials.gov, we use the official_title per previous work (see https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0193088).
# We have previously downloaded the official_title of ClinicalTrials.gov trials from AACT (https://zenodo.org/records/7590083) and integrate them. DRKS titles remain unchanged.
studies <- read_csv(path(dir_raw,"studies.csv" ))

##########################################################

# Extract IDs and process titles for IV trials first
IV_ids <- trials |>
          select(id,title) |>
          unique()

NCT_full_titles <- studies |>
  select(nct_id, official_title) |>
  rename(id = nct_id) |>
  rename(official = official_title)

# Update NCT titles in IV_ids
IV_ids <- rename(IV_ids, brief = title) # Note that the DRKS titles in IV_ids are NOT brief. This column name refers to the brief titles of ClinicalTrials.gov trials. The DRKS titles will not be replaced when the brief titles of NCT trials are replaced with their full titles

# If there is no official title available in ClinicalTrials.gov, the brief title will be used as the 'title'
IV_updated_titles <- left_join(IV_ids, NCT_full_titles, by = "id") |>
  mutate(title = ifelse(is.na(official), brief, official)) |>
  select(-brief, -official)

# Process IV titles to make compatible with title matching algorithm
IV_updated_titles <- IV_updated_titles |>
  mutate(title_processed = tolower(title) |>
           stringr::str_squish() |> # remove whitespace at start and end, as well as any "\t" whitespace characters
           stringr::str_remove_all("[:punct:]") |>
           stringr::str_remove_all(" ")) |>
  select(id, title, title_processed)

###########################################################

# Extract EU IDs and country data (for filtering), plus process EU titles to be used in title matching algorithm
EU_ids <- EU_dump |>
          select(eudract_number,
                 member_state_concerned,
                 full_title_of_the_trial,
                 sponsors)

EU_ids <- rename(EU_ids, id = eudract_number, state = member_state_concerned, title = full_title_of_the_trial)

# Filter out EU trials which dont have 'Germany' listed in member_state_concerned
# Yields table with 13068 unique trials which mention Germany in this field
# Can filter however we want here.

EU_only_German <- EU_ids |>
                  filter(grepl('Germany', state))

# Process EU titles
EU_only_German <- EU_only_German |> select(id,title) |>
  tidyr::drop_na(title) |>
  distinct(id, title,.keep_all = TRUE) |>
  mutate(title_processed = tolower(title) |>
           stringr::str_squish() |> # remove whitespace at start and end, as well as any "\t" whitespace characters
           stringr::str_remove_all("[:punct:]") |>
           stringr::str_remove_all(" "))

##########################################################
## Title matching (from 02_cross-reg-title-matching.R)

# assigning maxDist value for title matching (Elements in x will not be matched
# with elements of table if their distance is larger than maxDist)
DISTANCE <- 7

## The below takes about 3.5 hours on my baseline 2020 MacBook Air
## Let it run, it takes a while!!

start_time <- Sys.time()

# use amatch function to find title matches
# Default method used for matching. Documentation for amatch function:
# https://www.rdocumentation.org/packages/stringdist/versions/0.9.12/topics/amatch
 title_matches <- cbind(
  euctr_title_tm = EU_only_German$title,
  euctr_id = EU_only_German$id,
  IV_updated_titles[amatch(
    EU_only_German$title_processed,
    IV_updated_titles$title_processed,
    maxDist = DISTANCE, matchNA = FALSE),]
) |>
  # keep only those where a match was found
  filter(
    !is.na(id)
  ) |>
  distinct(
    # euctr_id, # we want to preserve all EUCTR crossreg for given trial
    id,
    .keep_all = TRUE
  ) |>
  # add flag for EUCTR crossreg found via title matching
  mutate(
    has_crossreg_eudract_tm = TRUE
  )

end_time <- Sys.time()
total_time <- end_time - start_time


# store title matching results in distance specific dataframe
assign(paste0("title_matches_", DISTANCE), title_matches)

##########################################################

# Save to RDS to use in trn_trn script
saveRDS(title_matches_7, "data/cross-registrations/title_matched_7.rds" )





