# Script to condense duplicate entries in trn_trn table to make manual reconciliation easier.

library(here)
library(fs)
library(lubridate)
library(tidyverse)
library(ctregistries)

# Load long trn_trn table
trn_trn <- read_rds("data/cross-registrations/trn_trn.rds")

# Drop unnecessary columns, add new publication booleans
trn_trn_no_regs <- trn_trn |>
  select(-registry1, -registry2)

# Create a unique identifier for pairs, which allows us to identify duplicate pairs
trn_trn_pair_id <- trn_trn_no_regs %>%
  rowwise |>
  mutate(pair_id = paste(sort(c(trn1, trn2)), collapse = "_")) |>
  ungroup()

# Group and merge publication and trn1inreg2/trn2inreg1 information
trn_merged_pubs <- trn_trn_pair_id %>%
  group_by(pair_id) %>%

  # Copy the trn2 boolean information of the other trial with the same pair_id into the other trials trn1 boolean fields
  mutate(
    trn1_in_pub_si = if_else(trn1 == first(trn1), lead(trn2_in_pub_si), lag(trn2_in_pub_si)),
    trn1_in_pub_abs = if_else(trn1 == first(trn1), lead(trn2_in_pub_abs), lag(trn2_in_pub_abs)),
    trn1_in_pub_ft = if_else(trn1 == first(trn1), lead(trn2_in_pub_ft), lag(trn2_in_pub_ft)),

    trn1inreg2 = case_when(
      trn1 == first(trn1) & (is.na(trn1inreg2) & is.na(lead(trn2inreg1))) ~ NA,
      trn1 == first(trn1) & (trn1inreg2 == lead(trn2inreg1)) ~ trn1inreg2,
      trn1 == first(trn1) & (is.na(trn1inreg2) | is.na(lead(trn2inreg1))) ~ coalesce(trn1inreg2, lead(trn2inreg1), FALSE),

      trn1 != first(trn1) & (is.na(trn1inreg2) & is.na(lag(trn2inreg1))) ~ NA,
      trn1 != first(trn1) & (trn1inreg2 == lag(trn2inreg1)) ~ trn1inreg2,
      trn1 != first(trn1) & (is.na(trn1inreg2) | is.na(lag(trn2inreg1))) ~ coalesce(trn1inreg2, lag(trn2inreg1), FALSE),

      .default = NA
    ),
    trn2inreg1 = case_when(
      trn1 == first(trn1) & (is.na(trn2inreg1) & is.na(lead(trn1inreg2))) ~ NA,
      trn1 == first(trn1) & (trn2inreg1 == lead(trn1inreg2)) ~ trn2inreg1,
      trn1 == first(trn1) & (is.na(trn2inreg1) | is.na(lead(trn1inreg2))) ~ coalesce(trn2inreg1, lead(trn1inreg2), FALSE),

      trn1 != first(trn1) & (is.na(trn2inreg1) & is.na(lag(trn1inreg2))) ~ NA,
      trn1 != first(trn1) & (trn2inreg1 == lag(trn1inreg2)) ~ trn2inreg1,
      trn1 != first(trn1) & (is.na(trn2inreg1) | is.na(lag(trn1inreg2))) ~ coalesce(trn2inreg1, lag(trn1inreg2), FALSE),

      .default = NA
    )
  ) %>%
  ungroup()

# Merge protocol ID and title matching information
trn_protocols_merged <- trn_merged_pubs |>
  group_by(pair_id) |>
  mutate(
    is_match_protocol_sponsor_protocol_id = ifelse(any(!is.na(is_match_protocol_sponsor_protocol_id)),
                                                   TRUE, NA),
    is_match_results_sponsor_protocol_id = ifelse(any(!is.na(is_match_results_sponsor_protocol_id)),
                                                  TRUE, NA),
    is_title_matched = ifelse(any(!is.na(is_title_matched)),
                              TRUE, NA)) |>
  ungroup()

# Merge meta booleans
trn_merged_meta <- trn_protocols_merged |>
  group_by(pair_id) |>
  mutate(
    at_least_one_pub = ifelse(any(at_least_one_pub), TRUE, FALSE),
    at_least_one_sponsor_match = ifelse(any(at_least_one_sponsor_match), TRUE, FALSE)) |>
  ungroup()

# Drop duplicated row. They should have the same information now, so just drop the first.
trn_no_dupes <- trn_merged_meta |>
  group_by(pair_id) |>
  slice_head(n= 1) |>
  ungroup()

# Reassign priorities based on Delwen's proposal:
trn_priorities <- trn_no_dupes |>
  mutate(priority = NA) |>
  mutate(priority = case_when(

    # Priority 1
    at_least_one_EU &
      at_least_one_IV &
      trn1inreg2 &
      trn2inreg1 &
      is.na(priority) ~ 1,

    # Priority 2
    at_least_one_EU &
      at_least_one_IV &
      (trn1inreg2 | trn2inreg1) &
      is.na(priority) ~ 2,

    # Priority 3
    at_least_one_EU &
      at_least_one_IV &
      is_title_matched &
      is.na(priority) ~ 3,


    # Priority 4
    at_least_one_EU &
      at_least_one_IV &
      at_least_one_pub &
      is.na(priority) ~ 4,

    # Priority 5
    at_least_one_EU &
      at_least_one_IV &
      at_least_one_sponsor_match &
      is.na(priority) ~ 5,

    .default = 7
  ))

# Rearrange columns to make manual checks easiest
trn_manual_checks <- trn_priorities |>
  relocate(trn1_in_pub_si, trn1_in_pub_abs, trn1_in_pub_ft, .before = trn2_in_pub_si) |>
  select(-pair_id)

