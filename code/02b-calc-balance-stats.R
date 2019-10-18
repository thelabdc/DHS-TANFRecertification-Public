
# Calculate balance summaries ---------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Use anonymized data for balance checks:

df_baseline_outcomes <- read_csv("data/df_replication_anonymized.csv")

# Trim full anonymized data to include only baseline quantities, 
# excluding hashed variables:
baseline_variable_names <- c("ic_case_id", "service_center", "appt_date",
                             "condition", "recert_month", "letter_sent_date",
                             "recert_id")

final_baseline_data <- df_baseline_outcomes %>% 
  select(one_of(baseline_variable_names))

# Calculate minimum, maximum, and average appt date for each Service Center -
# month - condition combinaion:

df_dates <- final_baseline_data %>% 
  group_by(service_center, recert_month, condition) %>%
  summarise(min = min(appt_date), max = max(appt_date), mean = mean(appt_date),
            median = median(appt_date)) %>%
  gather("statistic", "date", 4:7)

df_date_mad <- df_dates %>% group_by(service_center, recert_month, statistic) %>%
  summarise(mad = median(abs(date - median(date))))

mean((df_date_mad %>% filter(statistic == "mean"))$mad)

# Similar, but only for SVC Centers with > 5 obs:
df_tmp <- df_date_mad %>% filter(! service_center %in% 
                         c("Office of Specialized services (518)", "Virginia Williams Service Center(925)"))

# Similar, but only for SVC Centers with > 5 obs (processed Center names):
df_tmp <- df_date_mad %>% filter(! service_center %in% 
                                   c("Offc_Spec_Svcs", "Virginia_Williams"))


quantile(df_tmp$mad, 0.99)

# MAD stats across conditions ---------------------------------------------

sort(df_tmp$mad) # all less than 0.5 days

