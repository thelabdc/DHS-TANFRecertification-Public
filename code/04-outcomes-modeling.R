# TANF Outcomes Modeling
# Ryan T. Moore
# First: 20 March 2018
# Last: 23 October 2018

# NB: The objects in df_modeling.RData (and other .RData's) all have
# same object name: df_baseline_outcomes.

library(ggplot2)
library(dplyr)
library(coefplot)
library(lubridate)
library(hms)
library(forcats)

# Load processed outcome data ---------------------------------------------

load("data/df_modeling.RData")

# TE for Open vs. Specific Appt -------------------------------------------

lm_open_spec <- lm(successful_recert ~ open_appt + specific_appt, 
                   data = df_baseline_outcomes)
coefplot(lm_open_spec)


# Linear Models for TE given Service Center -------------------------------

lm_success <- lm(successful_recert ~ service_center + any_letter, 
                 data = df_baseline_outcomes)

summary(lm_success)
coefplot(lm_success)


lm_success_sc_te <- lm(successful_recert ~ service_center*any_letter, 
                 data = df_baseline_outcomes)

summary(lm_success_sc_te)
coefplot(lm_success_sc_te)


# Success by Svc Center (Rates) -------------------------------------------

df_svc_summ <- df_baseline_outcomes %>% group_by(service_center) %>%
  filter(n() > 5) %>% 
  dplyr::summarise(success_rate = mean(successful_recert), 
            lower = success_rate - qnorm(.975)*sqrt(success_rate * (1 - success_rate)/n()),
            upper = success_rate + qnorm(.975)*sqrt(success_rate * (1 - success_rate)/n()))

df_svc_summ$service_center <- fct_relevel(df_svc_summ$service_center, 
                                          df_svc_summ$service_center[order(df_svc_summ$success_rate)])

ggplot(df_svc_summ, aes(x = service_center)) + 
  geom_hline(yintercept = mean(df_baseline_outcomes$successful_recert), 
             linetype = 2, color = "gray") +
  geom_point(aes(y = success_rate), size = 4) +
  geom_errorbar(aes(ymin = lower, ymax = upper, width = .03)) + 
  lims(y = c(0, .5)) +
  labs(x = "Service Center", y = "Proportion Successfully Recertifying")


# TE by day of week (specific appointments only) --------------------------

df_baseline_outcomes$appt_date2 <- parse_date_time(df_baseline_outcomes$appt_date,
                                              orders = c("%m/%d/%Y", "%Y-%m-%d",
                                                         "%Y-%m-%d %H:%M:%S",
                                                         "%m/%d/%Y %h:%M"))

df_baseline_outcomes$appt_date2 <- as.Date(df_baseline_outcomes$appt_date2)

df_baseline_outcomes <- df_baseline_outcomes %>% 
  mutate(appt_day_of_week = wday(appt_date2, label = TRUE))


# Any letter vs. no letter:
df_days <- df_baseline_outcomes %>% group_by(appt_day_of_week) %>% 
  dplyr::summarise(te_letter = t.test(successful_recert ~ any_letter)$estimate[2] -
              t.test(successful_recert ~ any_letter)$estimate[1],
            ci_min = -t.test(successful_recert ~ any_letter)$conf.int[2],
            ci_max = -t.test(successful_recert ~ any_letter)$conf.int[1])
df_days

#pdf("figs/te_days.pdf")
ggplot(df_days, aes(appt_day_of_week, te_letter)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_min, ymax = ci_max), width = .03) + 
  geom_hline(yintercept = 0, col = "gray", alpha = .8, linetype = 2) +
  labs(y = "TE of Any Letter")
#dev.off()


# Among letter recips, open vs. specific:

df_days_open <- df_baseline_outcomes %>% group_by(appt_day_of_week) %>% 
  filter(any_letter) %>% 
  dplyr::summarise(te_letter = t.test(successful_recert ~ open_appt)$estimate[2] -
              t.test(successful_recert ~ open_appt)$estimate[1],
            ci_min = -t.test(successful_recert ~ open_appt)$conf.int[2],
            ci_max = -t.test(successful_recert ~ open_appt)$conf.int[1])

ggplot(df_days_open, aes(appt_day_of_week, te_letter)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_min, ymax = ci_max), width = 0.03) + 
  geom_hline(yintercept = 0, col = "gray", alpha = .8, linetype = 2) +
  labs(y = "TE of Open vs. Specific")


# Specific letter vs. No letter:

df_days_spec <- df_baseline_outcomes %>% group_by(appt_day_of_week) %>% 
  filter(any_letter == 0 | specific_appt == 1) %>% 
  dplyr::summarise(te_letter = t.test(successful_recert ~ specific_appt)$estimate[2] -
              t.test(successful_recert ~ specific_appt)$estimate[1],
            ci_min = -t.test(successful_recert ~ specific_appt)$conf.int[2],
            ci_max = -t.test(successful_recert ~ specific_appt)$conf.int[1])

ggplot(df_days_spec, aes(appt_day_of_week, te_letter)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_min, ymax = ci_max), width = 0.03) + 
  geom_hline(yintercept = 0, col = "gray", alpha = .8, linetype = 2) +
  labs(y = "TE of Specific vs. No Letter")


# Complier analysis using Return to Sender data -------------------------------

df_baseline_outcomes <- df_baseline_outcomes %>% 
  mutate(received_letter = ifelse(is.na(return_to_sender) & (any_letter == TRUE), 
                                  1, 0))

# Wald estimator:
lm_y_on_assg <- lm(successful_recert ~ any_letter, data = df_baseline_outcomes)
lm_recpt_on_assg <- lm(received_letter ~ any_letter, data = df_baseline_outcomes)
itt_letter <- coef(lm_y_on_assg)["any_letterTRUE"]
itt_recpt_on_assg <- coef(lm_recpt_on_assg)["any_letterTRUE"]

wald_itt <- itt_letter/itt_recpt_on_assg

# Identical 2SLS estimator:
lm_2sls <- lm(df_baseline_outcomes$successful_recert ~ fitted.values(lm_recpt_on_assg))
itt_2sls <- coef(lm_2sls)[2]

# Write summary: 
summ_rts <- dplyr::count(df_baseline_outcomes, return_to_sender)
n_rts_stamp <- sum(summ_rts$n[summ_rts$return_to_sender %in% "RTS-NoStamp"])
n_rts_addr <- sum(summ_rts$n[summ_rts$return_to_sender %in% "RTS-WrongAddress"])

n_letters_lost_to_follow_up <- 14

cat("Of the ", sum(df_baseline_outcomes$any_letter) + n_letters_lost_to_follow_up, 
    " letters we sent out, we know that ",
    n_rts_stamp + n_rts_addr, " did not reach the intended recipient.  The postal service ",
    "returned these letters to the sender either due to an incorrect address (", n_rts_addr, ") ",
    "or due to missing a postal stamp (", n_rts_stamp, ").  This group ",
    "represents a relatively small proportion of the treatment group (about ",
    round((n_rts_addr + n_rts_stamp)/sum(df_baseline_outcomes$any_letter), 2)*100,
    "% of the intended recipients).  Thus, when we calculate the effect of the letters ",
    "on only those who had a chance to receive them -- a complier average causal effect -- ",
    "we obtain results similar to our overall average treatment effect.  Compliers ",
    "were about ", round(wald_itt, 3)*100, " percentage points more likely to ",
    "successfully recertify than those in the control group.", sep = "")
    
# RTS proportion
(n_rts_addr + n_rts_stamp) / sum(df_baseline_outcomes$any_letter)

# Check of failure to treat distributions:
rts_by_letter <- df_baseline_outcomes %>% filter(any_letter) %>% 
  group_by(open_appt) %>% dplyr::count(return_to_sender) %>% mutate(prop = n/sum(n))

rts_open_stamp <- rts_by_letter[rts_by_letter$open_appt == TRUE & 
                                  rts_by_letter$return_to_sender == "RTS-NoStamp",
                                "prop"] %>% na.omit() %>% unlist()
rts_open_addr <- rts_by_letter[rts_by_letter$open_appt == TRUE & 
                              rts_by_letter$return_to_sender == "RTS-WrongAddress",
                            "prop"] %>% na.omit() %>% unlist()
rts_spec_stamp <- rts_by_letter[rts_by_letter$open_appt == FALSE & 
                                  rts_by_letter$return_to_sender == "RTS-NoStamp",
                                "prop"] %>% na.omit() %>% unlist()
rts_spec_addr <- rts_by_letter[rts_by_letter$open_appt == FALSE & 
                                 rts_by_letter$return_to_sender == "RTS-WrongAddress",
                               "prop"] %>% na.omit() %>% unlist()


# Write sentences ---------------------------------------------------------

cat("Letters with open appointments were slightly more likely to reach the intended",
    " recipients.  Among those with open appointment letters, ", 
    round(rts_open_addr*100, 1),
    " percent were not delivered due to an incorrect address; an additional ",
    round(rts_open_stamp*100, 1), " percent were not delivered due to a missing stamp.", 
    "  The analogous figures for those with specific appointment letters was ",
    round(rts_spec_addr*100, 1), " and ", round(rts_spec_stamp*100, 1), 
    " percent, respectively.  ", sep = "")


# Save data ---------------------------------------------------------------

save(df_baseline_outcomes, 
     file = "data/df_bayes.RData")

# Clean up ----------------------------------------------------------------

rm(df_days, df_days_open, df_days_spec, df_svc_summ, itt_2sls, itt_letter, 
   itt_recpt_on_assg, lm_2sls, lm_open_spec, lm_recpt_on_assg, lm_success,
   lm_success_sc_te, lm_y_on_assg, n_rts_addr, n_rts_stamp, rts_by_letter,
   rts_open_addr, rts_open_stamp, rts_spec_addr, rts_spec_stamp, summ_rts,
   wald_itt)
