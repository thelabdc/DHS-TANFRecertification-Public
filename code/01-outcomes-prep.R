# TANF Outcomes Prep
# Ryan T. Moore
# First: 16 January 2018
# Last: 23 October 2018

# This file reads raw data with personally identifiable information and creates
# a .csv that is read into Python and anonymized using the Lab Anonymizer. 
# The Python notebook file that reads the .csv and performs the anonymization is 
# code/anonymize_replication_data.ipynb
# The output file is read at the beginning of file 02-outcomes-consort.R, and
# all replication follows from that point.

# Data should be stored in directory 
# "DHS-TANFRecertification/data/tanf-final-data/"
# This directory is not synced via GitHub.  It is gitignored.

# Load libraries ----------------------------------------------------------

library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(forcats)

# Read data ---------------------------------------------------------------

# Read outcomes:

df_outcomes <- read_excel("data/tanf-final-data/DHS_TANF_Recert_Cases_and_Outcomes_Data_extracted_on_12-28-17.xlsx")

n_obs_year <- nrow(df_outcomes)
n_obs_year
# 10380


# Load baseline data:

load("data/tanf-final-data/final_baseline_data_2017-12-07.RData")

# Reformat PDC Number to numeric, to enable joining:

final_baseline_data$pdc_number <- as.numeric(final_baseline_data$pdc_number)

# Remove empty returned_letter variable:

final_baseline_data <- final_baseline_data %>% select(-returned_letter)

# Read Return to Sender data:

final_baseline_data_rts <- read_csv("data/tanf-final-data/FINAL_baseline_return2senderEB_121417 (1).csv")


# Merge RTS data in:

final_baseline_data <- left_join(final_baseline_data, final_baseline_data_rts,
                 by = c("ic_case_id", "service_center", 
                        "head_hh", "address", "recert_month"))


# Rename variables in outcomes df -----------------------------------------

df_outcomes <- df_outcomes %>% 
  rename(ic_case_id = "IC# (Household)", pdc_number = "TANF PDC Case #", 
         pdc_status = "PDC Current Status", renewal_date = "Renewal Date",
         notice_date = "Notice Sent On", 
         days_betwn_notice_and_recert_due = "Days between Notice and Recert Due",
         cert_period_start = "Cert Period Start", cert_period_end = "Cert Period End",
         recert_status = "Recert Status", denial_reason = "Denial Reason",
         updated_by = "Updated By"
  )


# Retain only outcomes for correct recertification months:

df_outcomes <- df_outcomes %>% 
  filter((renewal_date >= "2017-07-30") & (renewal_date <= "2017-11-30"))

# Retain only outcomes for IC's in baseline data:

df_outcomes <- df_outcomes %>%
  mutate(recert_month = month(renewal_date, label = TRUE, abbr = FALSE))


# Process duplicates in baseline data ----------------------------------------------------

# Duplicates: get ICC for those appearing twice:

duplicate_iccs <- final_baseline_data %>% 
  filter(duplicated(ic_case_id)) %>%
  select(ic_case_id)

df_only_duplicated <- final_baseline_data[final_baseline_data$ic_case_id %in% 
                                            duplicate_iccs$ic_case_id, ]

duplicate_iccs_summ <- df_only_duplicated %>% group_by(ic_case_id) %>%
  summarise(n_addr = length(unique(address)), 
            n_month = length(unique(recert_month)), 
            n_pdc = length(unique(pdc_number)))

# ID those 4 duplicated ICC's with multiple addresses or PDC's, but
# *not* multiple months:

duplicate_iccs_pdc_addr <- duplicate_iccs_summ %>% 
  filter(n_month == 1) %>%
  select(ic_case_id)

# ID those 3 duplicated ICC's with multiple months:

duplicate_iccs_month <- duplicate_iccs_summ %>% 
  filter(n_month > 1) %>%
  select(ic_case_id)

# For multiple addresses/PDC's, keep only second row 
# (due to better address for one participant)
# (Only variation across rows is address or PDC.)

df_keep_pdc_addr <- df_only_duplicated %>%
  filter(ic_case_id %in% duplicate_iccs_pdc_addr$ic_case_id) %>%
  group_by(ic_case_id) %>%
  slice(2)

# For multiple months, leave both cases in

df_keep_month <- df_only_duplicated %>%
  filter(ic_case_id %in% duplicate_iccs_month$ic_case_id) %>%
  group_by(ic_case_id) %>%
  slice(1:length(ic_case_id)) # I.e., keep all rows.

df_keep <- dplyr::union(df_keep_pdc_addr, df_keep_month)

# Cut all data from those IC's:

final_baseline_data <- final_baseline_data %>% 
  filter(!(ic_case_id %in% df_keep$ic_case_id))

# Rejoin only selected row:

final_baseline_data <- full_join(final_baseline_data, df_keep)


# Clean up ----------------------------------------------------------------

rm(df_keep, df_keep_month, df_keep_pdc_addr, df_only_duplicated, 
   duplicate_iccs, duplicate_iccs_month, duplicate_iccs_pdc_addr,
   duplicate_iccs_summ)



# Process duplicates in outcome data ----------------------------------------------------

# Duplicates: get ICC for those appearing twice:

duplicate_iccs <- df_outcomes %>% 
  filter(duplicated(ic_case_id)) %>%
  select(ic_case_id)

df_only_duplicated <- df_outcomes[df_outcomes$ic_case_id %in%
                                    duplicate_iccs$ic_case_id, ]

duplicate_iccs_summ <- df_only_duplicated %>% group_by(ic_case_id) %>%
  summarise(n_month = length(unique(recert_month)), 
            n_pdc = length(unique(pdc_number))
            )

# ID those 2 duplicated ICC's with multiple PDC's, but
# *not* multiple months:

duplicate_iccs_pdc <- duplicate_iccs_summ %>% 
  filter(n_month == 1) %>%
  select(ic_case_id)

# ID those 3 duplicated ICC's with multiple months:

duplicate_iccs_month <- duplicate_iccs_summ %>% 
  filter(n_month > 1) %>%
  select(ic_case_id)

# For multiple PDC's, keep only (arbitrarily) second row:

df_keep_pdc <- df_only_duplicated %>%
  filter(ic_case_id %in% duplicate_iccs_pdc$ic_case_id) %>%
  group_by(ic_case_id) %>%
  slice(2)

# For multiple months, leave both cases in

df_keep_month <- df_only_duplicated %>%
  filter(ic_case_id %in% duplicate_iccs_month$ic_case_id) %>%
  group_by(ic_case_id) %>%
  slice(1:length(ic_case_id)) # I.e., keep all rows.

df_keep <- dplyr::union(df_keep_pdc, df_keep_month)

# Cut all data from those IC's:

df_outcomes <- df_outcomes %>% 
  filter(!(ic_case_id %in% df_keep$ic_case_id))

# Rejoin only selected rows:

df_outcomes <- full_join(df_outcomes, df_keep)


# Clean up ----------------------------------------------------------------

rm(df_keep, df_keep_month, df_keep_pdc, df_only_duplicated, 
   duplicate_iccs, duplicate_iccs_month, duplicate_iccs_pdc,
   duplicate_iccs_summ)


# Merge data and validate --------------------------------------------------------------

df_baseline_outcomes <- left_join(final_baseline_data, df_outcomes,
                                  by = c("ic_case_id", "recert_month"))


# Recode service center names ---------------------------------------------

df_baseline_outcomes$service_center <- recode(df_baseline_outcomes$service_center,
                                               'ANACOSTIA SERVICE CENTER' = "Anacostia",
                                               'ANACOSTIA SERVICE CENTER(611)' = "Anacostia",
                                               'CONGRESS HEIGHTS SERVICE CENTER' = "Congress_Heights",      
                                               'CONGRESS HEIGHTS SERVICE CENTER(641)' = "Congress_Heights", 
                                               "FORT DAVIS SERVICE CENTER" = "Fort_Davis",       
                                               "FORT DAVIS SERVICE CENTER(671)" = "Fort_Davis",    
                                               "H STREET SERVICE CENTER" = "H_Street",            
                                               "H STREET SERVICE CENTER(511)" = "H_Street",     
                                               "Office of Specialized services (518)" = "Offc_Spec_Svcs",
                                               "TAYLOR STREET SERVICE CENTER" = "Taylor_Street",
                                               "TAYLOR STREET SERVICE CENTER(681)" = "Taylor_Street",   
                                               "Virginia Williams Service Center(925)" = "Virginia_Williams"
                                               )

# Load DCAS outcome data for outcome repair -------------------------------

df_dcas_outcomes <- read_excel("data/tanf-final-data/DHS_TANF_Recertification_2017-01__2018-02_-_Downloaded_from_DCAS_2018-04-19.xlsx")

df_dcas_outcomes <- df_dcas_outcomes %>% 
  rename(recert_month = "Recert Month", ic_case_id = "IC #", 
         pdc_number = "PDC #", 
         cert_period_start = "Cert Start", cert_period_end = "Cert End",
         n_recert_records = "# of Recert Records", recert_id = "Recert ID",
         recert_record_created = "Recert Record Created",
         derived_notice_status = "Derived Notice Status",
         notice_date = "Notice Date", appt_date_1 = "Appointment Date 1",
         appt_date_2 = "Appointment Date2", recert_status = "Recert Status",
         date_of_recert = "Date of Recertification", denial_reason = "Denial reason",
         recert_last_updated = "Recert Last Updated on", updated_by = "Status updated by",
         svc_center = "Service Center Name")

df_dcas_outcomes$ic_case_id <- as.numeric(df_dcas_outcomes$ic_case_id)
df_dcas_outcomes$pdc_number <- as.numeric(df_dcas_outcomes$pdc_number)


# Summarise duplicates ----------------------------------------------------

df_case_counts <- df_dcas_outcomes %>% group_by(ic_case_id, pdc_number) %>% count()

df_case_counts %>% group_by(n) %>% count()


# DCAS Outcomes for Duplicateds and Non-Duplicateds -----------------------

df_uniques <- df_case_counts %>% filter(n == 1)
df_dcas_uniques <- df_dcas_outcomes %>% semi_join(df_uniques)

df_duplicateds <- df_case_counts %>% filter(n > 1)
df_dcas_duplicateds <- df_dcas_outcomes %>% semi_join(df_duplicateds)


# Fixing outcomes using DCAS raw data -------------------------------------

# Decide which row to keep of DCAS duplicates

df_dcas_duplicateds$recert_status <- fct_relevel(df_dcas_duplicateds$recert_status,
                                                 "Recertified timely",
                                                 "Recertified after recert due date",
                                                 "Denied due to ineligibility",
                                                 "Closed prior to recertification due date",
                                                 "Denied due to failure of action")

df_dcas_duplicateds <- df_dcas_duplicateds %>% 
  mutate(recert_status_int = as.integer(recert_status))

# Keep only those rows, along with all unique ICC/PDC combinations

df_dcas_priority_dups <- df_dcas_duplicateds %>% 
  group_by(ic_case_id, pdc_number) %>%
  filter(recert_status_int == min(recert_status_int))

# If duplicated outcomes, select last updated one:

df_dcas_priority_dups <- df_dcas_priority_dups %>%
  group_by(ic_case_id, pdc_number) %>%
  filter(recert_last_updated == max(recert_last_updated))


# Create full year data for join with baseline_outcomes -------------------

df_dcas_one_per_ic_pdc <- df_dcas_priority_dups %>% 
  select(-recert_status_int) %>%
  dplyr::union(df_dcas_uniques)

# Check:
# tmp <- read.csv("dcas_unique_ic_pdc.csv")
# tmp <- df_dcas_one_per_ic_pdc 
# tmp %>% count(ic_case_id, pdc_number) %>% arrange(desc(n))

# Select cols of DCAS one-per-IC to add to df_baseline_outcomes:

df_dcas_priority_one_per_ic_pdc <- df_dcas_one_per_ic_pdc %>%
  select(ic_case_id, pdc_number, recert_month, notice_date, recert_status,
         date_of_recert)


# Fix varnames:

df_baseline_outcomes <- df_baseline_outcomes %>% 
  rename(pdc_number = pdc_number.x)

# Remove variable to enable anonymization:
df_baseline_outcomes <- df_baseline_outcomes %>%
  select(- pdc_number.y)



df_dcas_priority_one_per_ic_pdc <- df_dcas_priority_one_per_ic_pdc %>% 
  rename(recert_month_year = recert_month,
         recert_status_dcas = recert_status)

df_repaired_outcomes <- left_join(df_baseline_outcomes, df_dcas_priority_one_per_ic_pdc,
                                  by = c("ic_case_id", "pdc_number"))

# Check join: 

# count(df_repaired_outcomes, recert_status, recert_status_dcas)


# Recode outcomes from DCAS to baseline

# If recert_status_dcas = "Recertified..." or recert_status = "Processed", 
# then "successful":

to_processed <- (df_repaired_outcomes$recert_status_dcas == "Recertified timely" |
                   df_repaired_outcomes$recert_status_dcas == "Recertified after recert due date")

to_processed[is.na(to_processed)] <- FALSE

df_repaired_outcomes$recert_status[to_processed] <- "Processed"

# If recert_status_dcas = Denied, then "Received":

to_received <- (df_repaired_outcomes$recert_status_dcas == "Denied due to failure of action" &
                  df_repaired_outcomes$denial_reason == "Verifications not provided")
to_received[is.na(to_received)] <- FALSE
df_repaired_outcomes$recert_status[to_received] <- "Received"

# Recode DCAS recerts to original outcomes:

to_notice_sent <- (df_repaired_outcomes$recert_status_dcas == "Denied due to failure of action" &
                     df_repaired_outcomes$denial_reason == "No response from client") | 
  (df_repaired_outcomes$recert_status_dcas == "Closed prior to recertification due date")

to_notice_sent[is.na(to_notice_sent)] <- FALSE
df_repaired_outcomes$recert_status[to_notice_sent] <- "Notice Sent"


# Check relevels: 

# count(df_repaired_outcomes, recert_status, recert_status_dcas)

# Sample Composition and Timelines:

df_baseline_outcomes %>% count(condition, recert_month)

df_baseline_outcomes %>% count(recert_month)



# Save final outcome data for analysis and anonymizing --------------------

df_baseline_outcomes <- as_tibble(df_repaired_outcomes)

save(df_baseline_outcomes, n_obs_year,
     file = "data/tanf-final-data/df_baseline_outcomes.RData")

# Write to csv for anonymization:
write.csv(df_baseline_outcomes,  
          file = "data/tanf-final-data/df_replication_anon.csv")

# This csv is read into Python and anonymized using the Lab Anonymizer. 
# The file doing so is code/anonymize_replication_data.ipynb
# The output file is read at the beginning of file 02-outcomes-consort.R



# Clean up ----------------------------------------------------------------

rm(df_case_counts, df_dcas_duplicateds, df_dcas_one_per_ic_pdc,
   df_dcas_outcomes, df_dcas_priority_dups, df_dcas_uniques, df_duplicateds,
   df_outcomes, df_uniques, final_baseline_data, final_baseline_data_rts,
   to_notice_sent, to_processed, to_received)
