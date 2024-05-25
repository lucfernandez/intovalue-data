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

cross_registrations <- read_rds(path(dir_processed, "trn", "cross-registrations.rds"))

# pubs_with_crossreg will give us information about WHERE in the publication the cross-registered TRNs can be found
pubs_from_crossreg <- cross_registrations %>%
  filter(is_crossreg_secondary_id == TRUE | is_crossreg_abstract == TRUE | is_crossreg_ft == TRUE) %>% # filter out all cross-regs that aren't linked by a pub
  select(!is_crossreg_reg) %>%
  unique()

##############################################################################################

pubs_with_info <- pubs_from_crossreg|>
  rename(trns_si = is_crossreg_secondary_id,
         trns_abs = is_crossreg_abstract,
         trns_ft = is_crossreg_ft,
         primary_IV_id = id) |>

  # Replace booleans in trns_si, trns_abs, and trns_ft with the TRN value in crossreg_trn if the boolean is TRUE
  mutate(across(starts_with("trns"), ~ if_else(. == TRUE, crossreg_trn, NA))) |>

  # Summarize to row per iv trn with all crossreg by location
  group_by(pmid, doi, primary_IV_id) |>
  summarize(trns_si = paste(trns_si, collapse = ";"),
            trns_abs = paste(trns_abs, collapse = ";"),
            trns_ft = paste(trns_ft, collapse = ";"),
            .groups = "drop") |>
  mutate(across(starts_with("trn"), ~ na_if(., "NA"))) |>

  # Eliminate all random and intrusive NAs in trns_si, trns_abs, and trns_ft
  mutate(across(starts_with("trns"), ~ {

    cleaned <- str_split(., ";") |> # split semicolon-separated strings into lists
      lapply(function(x) x[x != "NA"]) |> # remove elements of lists that are equal to "NA" character
      sapply(function(x) paste(x, collapse = ";")) # collapse elements back into semicolon-separated strings
      ifelse(cleaned == "" | cleaned == "NA", NA, cleaned) # if result is an empty string or is still "NA", replace with NA value
  })) |>

   # Add final trns_other column for miscellaneous TRNs
  add_column(trns_other = NA)

saveRDS(pubs_with_info, "data/publications_final.rds")
