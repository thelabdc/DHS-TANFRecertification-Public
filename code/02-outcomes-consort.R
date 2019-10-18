# TANF Consort Diagram counts
# Ryan T. Moore
# First: 20 February 2018
# Last: 8 August 2019

library(dplyr)
library(readr)

# Load anonymized final data ----------------------------------------------
# (Replication procedure using csv file written by Python after anonymizing
#  using The Lab @ DC's anonymizer.)

# df_baseline_outcomes <- read_csv("data/df_replication_anonymized.csv")

# Remove unnecessary hashed columns ---------------------------------------

# df_baseline_outcomes <- df_baseline_outcomes %>%
#  select(- one_of(c("head_hh", "address", "city_state", 
#                    "tel", "pdc_number")))

# Rewrite anonymized data -------------------------------------------------

# write_csv(df_baseline_outcomes,
#           path = "data/df_replication_anonymized.csv")


# Reload anonymized final data --------------------------------------------

df_baseline_outcomes <- read_csv("data/df_replication_anonymized.csv")


# Load final data ---------------------------------------------------------
# (Our procedure using saved object below)

# load("data/tanf-final-data/df_baseline_outcomes.RData")


# Allocation to conditions ------------------------------------------------

df_baseline_outcomes %>% dplyr::count(condition)


# Full breakdown by Condition, RTS, Lost ----------------------------------

df_baseline_outcomes %>% dplyr::count(condition, is.na(recert_status))

df_baseline_outcomes <- df_baseline_outcomes %>% 
  filter(! is.na(recert_status))


# Return to Sender/No Stamp -----------------------------------------------

df_baseline_outcomes %>% group_by(condition) %>% count(return_to_sender) %>% 
  mutate(prop = n / sum(n))

# Combined: 

df_baseline_outcomes %>% dplyr::count(condition, !is.na(return_to_sender))



# Analysis data ---------------------------------------------------

df_baseline_outcomes %>% dplyr::count(condition)


# Save data without outcome NAs -------------------------------------------

# Number HH/year, calculated in code/01-outcomes-prep.R

n_obs_year <- 10380

# Save outcomes and n_obs_year:

save(df_baseline_outcomes, n_obs_year, 
     file = "data/df_analysis.RData")

