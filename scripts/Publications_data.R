## Script to compile all publications currently associated with the IV dataset and catalog
## which TRNs are found in which section of each publication

##############################################################################################

library(tidyverse)
library(readr)
library(here)
library(fs)
library(lubridate)
library(stringr)

dir_processed <- here("data", "processed")

trials <- read_csv(path(dir_processed, "trials.csv"))
cross_registrations <- read_rds(path(dir_processed, "trn", "cross-registrations.rds"))

# 'trials' gives us more information about URL and pub_type to supplement what we know from 'cross_registration'
pubs_supplementary_info <- trials %>%
                           select(doi, url, publication_type) %>%
                           unique()

# pubs_with_crossreg will give us information about WHERE in the publication the cross-registered TRNs can be found
pubs_from_crossreg <- cross_registrations %>%
  filter(is_crossreg_secondary_id == TRUE | is_crossreg_abstract == TRUE | is_crossreg_ft == TRUE) %>% # filter out all cross-regs that aren't linked by a pub
  select(!is_crossreg_reg) %>%
  unique()

##############################################################################################

pubs_with_info <-
  left_join(pubs_from_crossreg, pubs_supplementary_info, by = "doi", relationship = "many-to-many") |>
  relocate(url, publication_type, primary_IV_id = id, .after = pmid) |>
  # `crossreg_trn` should never be NA
  assertr::assert(assertr::not_na, crossreg_trn) |>

  rename(
    trns_si = is_crossreg_secondary_id,
    trns_abs = is_crossreg_abstract,
    trns_ft = is_crossreg_ft
  ) |>

  # Replace booleans with trn
  mutate(across(starts_with("trns"), ~ if_else(. == TRUE, crossreg_trn, NA))) |>

  # Summarize to row per iv trn with all crossreg by location
  group_by(pmid, doi, url, primary_IV_id) |>
  summarize(trns_si = paste(trns_si, collapse = ";"),
            trns_abs = paste(trns_abs, collapse = ";"),
            trns_ft = paste(trns_ft, collapse = ";"),
            url = paste(url, collapse = ";"),
            .groups = "drop") |>
  mutate(across(starts_with("trn"), ~ na_if(., "NA"))) |>
  add_column(trns_other = NA)

saveRDS(pubs_with_info, "data/publications_final.rds")
