# Date Analysis
# Ryan T. Moore
# First: 4 May 2018
# Last: 2024-11-05

# Preliminaries -----------------------------------------------------------

library(bizdays)
library(directlabels)
library(dplyr)
library(forcats)
library(ggplot2)
library(lubridate)
library(plyr)

# Load processed outcome data ---------------------------------------------

# To load, open the .Rproj file, which sets the working directory to be
# /DHS-TANFRecertification-Public/, the top-level directory of the repository.

load("data/df_bayes.RData")


# Examine date columns ----------------------------------------------------

date_match <- df_baseline_outcomes %>%
  select(contains("notice_date")) %>%
  filter(!is.na(notice_date.y)) %>% as.data.frame() %>%
  mutate(equal = notice_date.x == notice_date.y)

sum(!date_match$equal, na.rm = TRUE)
# 73.  Sometimes x > y, sometimes x < y.


# Create calendar for business days/holidays ------------------------------

create.calendar(name='MyCalendar',
                holidays = as_date(c("2017-01-01", "2017-07-04", "2017-10-09",
                                     "2017-11-23", "2018-01-01")),
                weekdays = c('sunday', 'saturday'),
                adjust.from = adjust.none, adjust.to = adjust.previous)

# Create day-count-relevant variables ----------------------------------------------

df_baseline_outcomes <- df_baseline_outcomes %>%
  mutate(appearance_day_of_week = wday(date_of_recert, label = TRUE),
         #recert_vs_appt_secs = date_of_recert - appt_date2,
         recert_vs_appt_bizdays = bizdays(appt_date2, date_of_recert,
                                          cal = "MyCalendar"),
         # to do: adjust recert dates to "today or previous, if not bus. day"
         #recert_vs_appt_days = day(recert_vs_appt_secs),
         recert_vs_appt_days = (date_of_recert - appt_date2),
         recert_day_of_month = mday(date_of_recert),
         renewal_month = month(df_baseline_outcomes$date_of_recert, label = TRUE, abbr = FALSE),
         #recert_vs_last_day = day(date_of_recert - renewal_date))
         recert_vs_last_day = (date_of_recert - renewal_date))


# Create pretty condition names -------------------------------------------

df_baseline_outcomes <- df_baseline_outcomes %>%
  mutate(condition_pretty = fct_relevel(condition, "No_Letter", "Specific_Appt", "Open_Appt"),
         condition_pretty = fct_recode(condition_pretty,
                                       'Letter with Open Date' = "Open_Appt",
                                       'Letter with Specific Date' = "Specific_Appt",
                                       'Standard\nCommunications Only' = "No_Letter"))

# Analysis ----------------------------------------------------------------

# Distribution of assigned appointments to days of week?

# ggplot(df_baseline_outcomes, aes(appt_day_of_week)) + geom_bar()
# dplyr::count(df_baseline_outcomes, appt_day_of_week)

# When do people come in?

bus_days <- c("Mon", "Tue", "Wed", "Thu", "Fri")

ggplot(df_baseline_outcomes %>% filter(appearance_day_of_week %in% bus_days),
       aes(appearance_day_of_week)) + geom_bar()

dplyr::count(df_baseline_outcomes, appearance_day_of_week) %>%
  mutate(prop = n/sum(n))

df_summ_appearance_day <- dplyr::count(df_baseline_outcomes, appearance_day_of_week) %>%
  filter(!is.na(appearance_day_of_week)) %>%
  mutate(prop = n / sum(n)) %>%
  mutate(is_wednesday = appearance_day_of_week %in% "Wed") #?

df_summ_appearance_day

# Test any difference in weekdays:

df_summ_appearance_day %>% filter(appearance_day_of_week != "Sat") %>%
  select(n) %>% chisq.test()

# Test Wednesday greatest:

# df_summ_appearance_day %>% group_by(is_wednesday) %>% summarise(m = mean(n))



# When appear, relative to appt date, by condition ------------------------

#pdf("figs/recert_hist_facets_appt_0.pdf", width = 15)
# NB: Order of count denom is [Standard, Open, Specific].
ggplot(df_baseline_outcomes, aes(recert_vs_appt_days, y = (100 * (after_stat(count))/c(1172, 1176, 1172)))) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(-45, 37),
                     breaks = c(-25, 0, 25),
                     labels = c("-25", "0\nAppointment", "25")) +
  labs(x = "Recert Date - Appointment Date, in Days", y = "Percentage Recertifying") +
  facet_wrap(~ condition_pretty)
#dev.off()

# Summarise

df_baseline_outcomes %>% group_by(condition) %>%
  summarise(mean = mean(recert_vs_appt_days, na.rm = TRUE),
            median = median(recert_vs_appt_days, na.rm = TRUE))


# When appear, relative to recert deadline, by condition ------------------

#pdf("figs/recert_hist_facets_60_0.pdf", width = 15)
df_baseline_outcomes |>
  ggplot(aes(recert_vs_last_day)) +
  geom_segment(x = -30, xend = -30, y = 0, yend = 36, alpha = 1, colour = "grey", linetype = 2) +
  geom_histogram(binwidth = 1, aes(y = after_stat(count))) +
  scale_x_continuous(limits = c(-60, 1),
                     breaks = c(-60, -30, -15, 0),
                     labels = c("-60\nInitial\nNotice",
                                "-30\nStart Recert\nMonth",
                                "-15\nTermination\nNotice", 0)) +
  annotate("rect", xmin = -35, xmax = -16, ymin = 0, ymax = 36, alpha = .2) +
  annotate("text", x = -25, y = 34, label = "Appt Window", cex = 4) +
  labs(x = "Recert Date - Deadline Date, in Days", y = "") + #Percent Recertifying") +
  facet_wrap(~ condition_pretty)
#dev.off()


# CDF for Recert Appearance vs. Deadline Date -----------------------------

df_baseline_outcomes |>
  filter((recert_vs_last_day >= -60) & (recert_vs_last_day < 2)) %>%
  ggplot(aes(recert_vs_last_day, color = condition)) + stat_ecdf()

# Manual transforms for cumulative prop recertified (calendar days):

df_cdf_deadline <- df_baseline_outcomes %>%
  filter((recert_vs_last_day >= -60) & (recert_vs_last_day < 3)) %>%
  filter(!is.na(recert_vs_last_day))

df_cdf_deadline$cum_cond_n_recerted <- NA

for(i in 1:nrow(df_cdf_deadline)){

  tmp <- df_cdf_deadline %>%
    filter(condition == df_cdf_deadline$condition[i])

  df_cdf_deadline$cum_cond_n_recerted[i] <-
    sum(tmp$recert_vs_last_day <= df_cdf_deadline$recert_vs_last_day[i],
        na.rm = TRUE)
}

# For each condition, calculate cumulative proportion recertified

df_cdf_deadline$cum_prop_recerted <- NA

for(i in c("No_Letter", "Open_Appt", "Specific_Appt")){
  df_cdf_deadline$cum_prop_recerted[df_cdf_deadline$condition == i] <-
    df_cdf_deadline$cum_cond_n_recerted[df_cdf_deadline$condition == i] /
    sum(df_baseline_outcomes$condition == i,  na.rm = TRUE)
}

# End manual transformation

# Plot

df_cdf_deadline <- df_cdf_deadline %>%
  mutate(condition_pretty = fct_relevel(condition_pretty,
                                        "Letter with Open Date",
                                        "Letter with Specific Date",
                                        "Standard\nCommunications Only"))

# Color version:

#pdf("figs/recert_cdf_lastday.pdf")
df_cdf_deadline %>%
  ggplot(aes(recert_vs_last_day, y = cum_prop_recerted * 100,
             color = condition_pretty)) +
  geom_step() +
  geom_vline(xintercept = -30, linetype = 2) +
  labs(x = "Recertification Date - Deadline, in Days",
       y = "Percent Recertifying") +
  scale_x_continuous(breaks=c(-60, -30, -15, 0),
                     labels=c("-60\nInitial Notice", "-30\nStart Recert Month",
                              "-15\nTermination Notice", "0\nDeadline"),
                     limits = c(-60, 3)) +
  ylim(0, 43) +
  theme(legend.justification=c(1, 0), legend.position=c(.95, .05)) +
  scale_color_manual(name = "Condition",
                       values = c("#0D47A1", "#82B1FF", "#B71C1C")) +
  annotate("rect", xmin = -35, xmax = -16, ymin = 0, ymax = 41, alpha = .2) +
  annotate("text", x = -25, y = 40, label = "Appt Window", cex = 5)
#dev.off()


# B/W linetype, no window, transparent bg version:

#pdf("figs/recert_cdf_lastday_bw.pdf")
df_cdf_deadline %>%
  ggplot(aes(recert_vs_last_day, y = cum_prop_recerted * 100,
             linetype = condition_pretty)) +
  geom_step() +
  geom_vline(xintercept = -30, linetype = 2) +
  labs(x = "Recertification Date - Deadline, in Days",
       y = "Percent Recertifying") +
  scale_x_continuous(breaks=c(-60, -30, -15, 0),
                     labels=c("-60\nInitial Notice", "-30\nStart Recert Month",
                              "-15\nTermination Notice", "0\nDeadline"),
                     limits = c(-60, 3)) +
  ylim(0, 43) +
  theme(legend.justification=c(1, 0), legend.position=c(.95, .05),
        panel.background = element_rect(fill = "transparent")) +
  scale_linetype_manual(name = "Condition", values = c(1, 2, 3)) +
  annotate("text", x = -25, y = 40, label = "Appt Window", cex = 5) +
  annotate("segment", x = c(-31.5, -18.5), xend = c(-35, -16),
           y = 40, yend = 40, size = 1, alpha = 1,
           arrow = arrow(length = unit(.2, "cm")), linejoin = "mitre")
#dev.off()


# CDF for Recert Appearance vs. Appointment Date --------------------------

df_baseline_outcomes %>%
  filter((recert_vs_appt_days >= -45) & (recert_vs_appt_days < 37)) %>%
  ggplot(aes(recert_vs_appt_days, color = condition)) + stat_ecdf()

# Manual transforms for cumulative prop recertified:

# (in Business Days)

df_cdf <- df_baseline_outcomes %>%
  filter((recert_vs_appt_days >= -45) & (recert_vs_appt_days < 37)) %>%
  filter(!is.na(recert_vs_appt_days))

df_cdf$cum_cond_n_recerted <- NA

for(i in 1:nrow(df_cdf)){

  tmp <- df_cdf %>%
    filter(condition == df_cdf$condition[i])

  df_cdf$cum_cond_n_recerted[i] <-
    sum(tmp$recert_vs_appt_bizdays <= df_cdf$recert_vs_appt_bizdays[i],
        na.rm = TRUE)
}

# For each condition, calculate cumulative proportion recertified

df_cdf$cum_prop_recerted <- NA

for(i in c("No_Letter", "Open_Appt", "Specific_Appt")){
  df_cdf$cum_prop_recerted[df_cdf$condition == i] <-
    df_cdf$cum_cond_n_recerted[df_cdf$condition == i] /
    sum(df_baseline_outcomes$condition == i,  na.rm = TRUE)
}

# End manual transformation

# Plot

df_cdf <- df_cdf %>%
  mutate(condition_pretty = fct_relevel(condition_pretty,
                                        "Letter with Open Date",
                                        "Letter with Specific Date",
                                        "Standard\nCommunications Only"))

# Color version:

#pdf("figs/recert_cdf.pdf")
df_cdf %>%
  ggplot(aes(recert_vs_appt_bizdays, y = cum_prop_recerted * 100,
             color = condition_pretty)) +
  geom_step() +
  geom_vline(xintercept = -0, linetype = 2) +
  labs(x = "Recertification Date - Appointment Date, in Business Days",
       y = "Percent Recertifying") +
  scale_x_continuous(breaks=c(-20, 0, 20),
                   labels=c("-20\nDays Before Appt", "Appointment Day", "20\nDays After Appt"),
                   limits = c(-22, 26)) +
  ylim(0, 43) +
  theme(legend.justification=c(1, 0), legend.position=c(.95, .05)) +
  scale_color_manual(name = "Condition",
                     values = c("#0D47A1", "#82B1FF", "#B71C1C"))
#dev.off()


# Version: B/W linetype, transparent bg

#pdf("figs/recert_cdf_bw.pdf")
df_cdf %>%
  ggplot(aes(recert_vs_appt_bizdays, y = cum_prop_recerted * 100,
             linetype = condition_pretty)) +
  geom_step() +
  geom_vline(xintercept = -0, linetype = 2) +
  labs(x = "Recertification Date - Appointment Date, in Business Days",
       y = "Percent Recertifying") +
  scale_x_continuous(breaks=c(-20, 0, 20),
                     labels=c("-20\nDays Before Appt", "Appointment Day", "20\nDays After Appt"),
                     limits = c(-22, 26)) +
  ylim(0, 43) +
  theme(legend.justification=c(1, 0), legend.position=c(.95, .05),
        panel.background = element_rect(fill = "transparent")) +
  scale_linetype_manual(name = "Condition", values = c(1, 2, 3))
#dev.off()


# What prop before appt date? ---------------------------------------------

df_baseline_outcomes %>%
  summarise(prop_before_appt = mean(recert_vs_appt_days < 0, na.rm = TRUE))

# prop of recertifiers:
df_baseline_outcomes %>% group_by(condition) %>%
  dplyr::summarise(prop_before_appt = mean(recert_vs_appt_days < 0, na.rm = TRUE))

# prop of all in condition, explicit:
df_baseline_outcomes %>% group_by(condition) %>%
  dplyr::summarise(count_before_appt = sum(recert_vs_appt_days < 0, na.rm = TRUE),
                   n = n(),
                   prop_before_appt = count_before_appt / n)


# What prop on appt date? -------------------------------------------------

df_baseline_outcomes %>%
  summarise(prop_on_appt = mean(recert_vs_appt_days == 0, na.rm = TRUE))

df_baseline_outcomes %>% group_by(condition) %>%
  dplyr::summarise(prop_on_appt = mean(recert_vs_appt_days == 0, na.rm = TRUE))

# prop of all in condition, explicit:
df_baseline_outcomes %>% group_by(condition) %>%
  dplyr::summarise(count_on_appt = sum(recert_vs_appt_days == 0, na.rm = TRUE),
                   n = n(),
                   prop_on_appt = count_on_appt / n)

# Only successful recerts: on or before due date?  TE.


# Prop of recertifiers recertifying after deadline ------------------------

df_baseline_outcomes %>% summarise(prop_late = mean(recert_vs_last_day > 0,
                                                    na.rm = TRUE))


# Among on-time opens, early appt dates wait toward end of month,
# while late appt dates are early.

df_baseline_outcomes %>% filter(recert_vs_last_day <= 0) %>%
  group_by(condition, appt_in_recert_month =
             month(appt_date2, label = TRUE, abbr = FALSE) == recert_month) %>%
  summarise(mean_recert_vs_appt_day = mean(recert_vs_appt_days, na.rm = TRUE))

# Day of month

df_on_time <- df_baseline_outcomes %>% filter(date_of_recert <= renewal_date)

ggplot(df_on_time, aes(recert_day_of_month)) + geom_bar()

# - Final/first bus. day of month. ?

#  In month of recert?  (A: about 57%)

df_baseline_outcomes %>%
  dplyr::count(recert_in_month = renewal_month == recert_month) %>%
  filter(!is.na(recert_in_month)) %>%
  mutate(prop = n / sum(n))

# Open appt least likely to appear in month of recert:
# Spec appt most likely to appear in month of recert, but
# same as no letter group:

df_baseline_outcomes %>%
  group_by(condition, recert_in_month = renewal_month == recert_month) %>%
  dplyr::summarise(n = n()) %>%
  filter(!is.na(recert_in_month)) %>%
  mutate(prop = n / sum(n))

# Possible: test whether specific anchor (and/or reminder)
# leads to recert within recert month.
