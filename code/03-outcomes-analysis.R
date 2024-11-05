# TANF Outcomes Analysis
# Ryan T. Moore
# First: 16 January 2018
# Last: 2024-11-05

library(dplyr)
library(ggplot2)
library(Rmisc)
library(stringr)
library(tibble)

# Load processed outcome data ---------------------------------------------

load("data/df_analysis.RData")

# Define outcomes and indicators for letter conditions --------------------

# Define "success", "completed", and "started" statuses and create outcome:

# Started:

df_baseline_outcomes$started <- 1
df_baseline_outcomes$started[(df_baseline_outcomes$recert_status == "Notice Sent")] <- 0
df_baseline_outcomes$started[(df_baseline_outcomes$denial_reason == "No response from client" &
                                df_baseline_outcomes$recert_status == "Denied")] <- 0

# Completed:
df_baseline_outcomes$completed <- df_baseline_outcomes$started
df_baseline_outcomes$completed[(df_baseline_outcomes$recert_status == "Received")] <- 0
df_baseline_outcomes$completed[(df_baseline_outcomes$denial_reason == "Verifications not provided" &
                                  df_baseline_outcomes$recert_status == "Denied")] <- 0

# Successful:
success_conds <- c("Processed")

df_baseline_outcomes <- df_baseline_outcomes %>%
  mutate(successful_recert = ifelse(recert_status %in% success_conds, 1, 0))

# Check:
# df_baseline_outcomes %>% dplyr::count(recert_status, denial_reason, started, completed, successful_recert)


# Define indicators for any letter, open appt, specific appt:

df_baseline_outcomes <- df_baseline_outcomes %>%
  mutate(any_letter = condition %in% c("Open_Appt", "Specific_Appt"),
         open_appt = condition == "Open_Appt",
         specific_appt = condition == "Specific_Appt"
  )


# Tests of Succcessful Recertifications ------------------------------------

test_any_letter <- t.test(successful_recert ~ any_letter,
                          data = df_baseline_outcomes)

test_open_vs_specific <- t.test(successful_recert ~ open_appt,
                                data = df_baseline_outcomes %>%
                                  filter(any_letter))


# Tests of Succcessful Recertifications, removing duplicated IC Case IDs
# (from multiple months)

dup_iccs <- df_baseline_outcomes %>% dplyr::count(ic_case_id) %>%
  filter(n > 1) %>% select(ic_case_id)

test_any_letter_unique <- t.test(successful_recert ~ any_letter,
                          data = filter(df_baseline_outcomes,
                                        !(ic_case_id %in% dup_iccs$ic_case_id)))

test_open_vs_specific_unique <- t.test(successful_recert ~ open_appt,
                                data = df_baseline_outcomes %>%
                                  filter(any_letter) %>%
                                  filter(!(ic_case_id %in% dup_iccs$ic_case_id)))

# To compare,
test_any_letter
test_any_letter_unique

test_open_vs_specific
test_open_vs_specific_unique


# Tests of Completed Recertifications ----------------------------------

test_any_letter_complete <- t.test(completed ~ any_letter,
                          data = df_baseline_outcomes)

test_open_vs_specific_complete <- t.test(completed ~ open_appt,
                                data = df_baseline_outcomes %>%
                                  filter(any_letter))

# Tests of Started Recertifications ------------------------------------

test_any_letter_started <- t.test(started ~ any_letter,
                          data = df_baseline_outcomes)

test_open_vs_specific_started <- t.test(started ~ open_appt,
                                data = df_baseline_outcomes %>%
                                  filter(any_letter))


# Analysis of Successful Recertifications ---------------------------------

# Any Letter

# Extract success rates:

success_no_letter <- unname(test_any_letter$estimate["mean in group FALSE"]) * 100
success_any_letter <-  unname(test_any_letter$estimate["mean in group TRUE"]) * 100
te_any_letter <- success_any_letter - success_no_letter

# Write sentences:

cat("Among those who received the usual notifications, ",
    round(success_no_letter, 1),
    "% successfully recertified. However, among those receiving an additional letter, ",
    round(success_any_letter, 1),
    "% successfully retained their benefits, a difference of ",
    round(te_any_letter, 1), " percentage points, ",
    "an increase in recertifications of ",
    round(te_any_letter/success_no_letter * 100, 0),
    " percent over the baseline. The 95% confidence interval around the difference of ",
    round(te_any_letter, 1), " covers (",
    round(-max(test_any_letter$conf.int) * 100, 1), ", ",
    round(-min(test_any_letter$conf.int) * 100, 1), ").",
    sep = "")


# Open vs. Specific Letter

# Extract success rates:

success_specific <- unname(test_open_vs_specific$estimate["mean in group FALSE"]) * 100
success_open <-  unname(test_open_vs_specific$estimate["mean in group TRUE"]) * 100
te_open_letter <- success_open - success_specific

# Write sentences:

cat("Evidence also suggests that open appointment letters which only ",
    "note the overall deadline generate higher recertification rates. ",
    "Among those who received the open appointment letter, ",
    round(success_open, 1),
    "% successfully recertified. This rate was only ",
    round(success_specific, 1),
    "% among those receiving letters with specific appointment dates. ",
    "This represents a difference of about ",
    round(te_open_letter, 1), " percentage points, ",
    "an increase in recertifications of ",
    round(te_open_letter/success_specific * 100, 0),
    " percent over the specific letter. The 95% confidence interval around the difference of ",
    round(te_open_letter, 1), " percentage points covers (",
    round(-max(test_open_vs_specific$conf.int) * 100, 1), ", ",
    round(-min(test_open_vs_specific$conf.int) * 100, 1), ").",
    sep = "")



# Estimate scale up -------------------------------------------------------

n_obs_year <- 12106
additional_fams <- round(n_obs_year * te_any_letter / 100, 0)
additional_fams_open <- round(n_obs_year * (success_open - success_no_letter)/100, 0)


cat("At current TANF levels and using current administrative procedures, ",
    "sending one additional letter could amount to ",
    additional_fams,
    " additional families retaining benefits over the course of a year.",
    " Using the open letter could amount to as many as ",
    additional_fams_open,
    " additional families retaining benefits.",
    sep = "")

additional_fams_experiment <-
  ((success_open - success_no_letter) / 100) *
  sum(df_baseline_outcomes$condition == "Open_Appt") +
  ((success_specific - success_no_letter) / 100) *
  sum(df_baseline_outcomes$condition == "Specific_Appt")

cat("We estimate that about", round(additional_fams_experiment, 0),
    "families retained benefits as a direct result of the experiment.")


# Cost-benefit ------------------------------------------------------------

1087 / round(additional_fams_experiment, 0)



# Plot results ------------------------------------------------------------

# Any letter:

df_test_any <- data.frame(Condition = c("No Letter", "Sent Letter"),
                          Prop_Recert = c(unname(test_any_letter$estimate)))

test_any_summ <- summarySE(df_baseline_outcomes, measurevar = "successful_recert",
                           groupvars = "any_letter")
test_any_summ_percent <- test_any_summ %>%
  mutate_at(vars(one_of(c("successful_recert", "sd", "se", "ci"))),
            list(~ . * 100))

# Experimental plot with both estimates and difference:
test_any_summ_exp <- test_any_summ |>
  mutate(any_letter = as.character(any_letter)) |>
  add_row(any_letter = "Difference",
          successful_recert = te_any_letter/100,
          ci = (test_any_letter$conf.int[2] - test_any_letter$conf.int[1]) / 2)

test_any_summ_exp$any_letter <- factor(test_any_summ_exp$any_letter,
                                       levels = c("FALSE", "TRUE", "Difference"))


# Primary figure, Any Letter, percentages ---------------------------------------------

primary_y_lower <- -3
primary_y_upper <- 50
primary_point_size <- 7
primary_x_lab <- ""
primary_x_scale_size <- 18
primary_x_scale_angle <- 0
primary_x_scale_face <- "bold"
primary_x_scale_color <- "black"

test_any_summ_percent_exp <- test_any_summ_exp %>%
  mutate_at(vars(successful_recert, sd, se, ci), list(~ . * 100))

# Primary plot with both estimates and difference:
fig_1_any <- ggplot(test_any_summ_percent_exp, aes(x = any_letter,
                                                   y = successful_recert)) +
  labs(x = primary_x_lab, y = "Percentage Recertifying")

#pdf("figs/primary_any_segs_w_diff.pdf")
fig_1_any + geom_errorbar(aes(ymin = successful_recert - ci,
                              ymax = successful_recert + ci),
                          width = 0.03, linetype = c(1, 1, 1)) +
  geom_point(shape = c(19, 19, 15), size = primary_point_size) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  ylim(primary_y_lower, primary_y_upper) +
  theme(text = element_text(size = primary_x_scale_size,
                            angle = primary_x_scale_angle,
                            face = primary_x_scale_face,
                            color = primary_x_scale_color),
        axis.text.x = element_text(color = primary_x_scale_color, hjust = 0.5, vjust = 0.5)) +
  scale_x_discrete(labels = c("Standard\nCommunications\nOnly",
                              "Plus\nBehaviorally-Informed\nReminder Letter", "Difference"))
#dev.off()


# Primary figure, Open vs. Specific, percentages --------------------------
df_tr_group <- df_baseline_outcomes %>%
  filter(any_letter == TRUE)

test_open_summ <- summarySE(df_tr_group, measurevar = "successful_recert",
                           groupvars = "open_appt")

test_open_summ_exp <- test_open_summ |>
  mutate(open_appt = as.character(open_appt)) |>
  add_row(open_appt = "Difference",
          successful_recert = te_open_letter/100,
          ci = (test_open_vs_specific$conf.int[2] -
                  test_open_vs_specific$conf.int[1]) / 2)

test_open_summ_exp$open_appt <- factor(test_open_summ_exp$open_appt,
                                       levels = c("FALSE", "TRUE", "Difference"))

test_open_summ_percent_exp <- test_open_summ_exp %>%
  mutate_at(vars(one_of(c("successful_recert", "sd", "se", "ci"))),
            list(~ . * 100))

# Primary plot with both estimates and difference:

fig_1_open <- ggplot(test_open_summ_percent_exp, aes(x = open_appt,
                                                   y = successful_recert)) +
  labs(x = primary_x_lab, y = "Percentage Recertifying")

#pdf("figs/primary_open_segs_w_diff.pdf")
fig_1_open + geom_errorbar(aes(ymin = successful_recert - ci,
                               ymax = successful_recert + ci),
                           width = 0.03, linetype = c(1, 1, 1)) +
  geom_point(shape = c(19, 19, 15), size = primary_point_size) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  ylim(primary_y_lower, primary_y_upper) +
  theme(text = element_text(size = primary_x_scale_size,
                            angle = primary_x_scale_angle,
                            face = primary_x_scale_face,
                            color = primary_x_scale_color),
        axis.text.x = element_text(color = primary_x_scale_color)) +
  scale_x_discrete(labels = c("Letter with\nSpecific Date",
                              "Letter with\nOpen Date", "Difference"))
#dev.off()

# Clean up ----------------------------------------------------------------

rm(additional_fams, additional_fams_open,
   df_test_any,
   n_obs_year, success_any_letter, success_conds, success_no_letter,
   success_open, success_specific, te_any_letter, te_open_letter,
   test_any_letter, test_any_summ, test_any_summ_exp, test_any_summ_percent,
   test_open_vs_specific)


# Analysis of Completed Recertifications ----------------------------------

# Any Letter

# Extract success rates:

success_no_letter <- unname(test_any_letter_complete$estimate["mean in group FALSE"]) * 100
success_any_letter <-  unname(test_any_letter_complete$estimate["mean in group TRUE"]) * 100
te_any_letter <- success_any_letter - success_no_letter

# Write sentences:

cat("Among those who received the usual notifications, ",
    round(success_no_letter, 1),
    "% successfully completed recertification, whether or not they were eligible to ",
    "retain benefits. However, among those receiving an additional letter, ",
    round(success_any_letter, 1),
    "% successfully completed recertification, a difference of ",
    round(te_any_letter, 1), " percentage points, ",
    "an increase in completions of ",
    round(te_any_letter/success_no_letter * 100, 0),
    " percent over the baseline. The 95% confidence interval around the difference of ",
    round(te_any_letter, 1), " percentage points covers (",
    round(-max(test_any_letter_complete$conf.int) * 100, 1), ", ",
    round(-min(test_any_letter_complete$conf.int) * 100, 1), ").",
    sep = "")


# Open vs. Specific Letter

# Extract success rates:

success_specific <- unname(test_open_vs_specific_complete$estimate["mean in group FALSE"]) * 100
success_open <-  unname(test_open_vs_specific_complete$estimate["mean in group TRUE"]) * 100
te_open_letter <- success_open - success_specific

# Write sentences:

cat("Evidence also suggests that open appointment letters which only ",
    "note the overall deadline generate higher completion rates. ",
    "Among those who received the open appointment letter, ",
    round(success_open, 1),
    "% successfully completed the process. This rate was only ",
    round(success_specific, 1),
    "% among those receiving letters with specific appointment dates. ",
    "This represents a difference of about ",
    round(te_open_letter, 1), " percentage points, ",
    "an increase in completions of ",
    round(te_open_letter/success_specific * 100, 0),
    " percent over the specific letter. The 95% confidence interval around the difference of ",
    round(te_open_letter, 1), " percentage points covers (",
    round(-max(test_open_vs_specific_complete$conf.int) * 100, 1), ", ",
    round(-min(test_open_vs_specific_complete$conf.int) * 100, 1), ").",
    sep = "")

# Clean up

rm(success_any_letter, success_no_letter, success_open, success_specific, te_any_letter,
   te_open_letter, test_any_letter_complete, test_open_vs_specific_complete)



# Analysis of Started Recertifications ------------------------------------

# Any Letter

# Extract success rates:

success_no_letter <- unname(test_any_letter_started$estimate["mean in group FALSE"]) * 100
success_any_letter <-  unname(test_any_letter_started$estimate["mean in group TRUE"]) * 100
te_any_letter <- success_any_letter - success_no_letter

# Write sentences:

cat("Among those who received the usual notifications, ",
    round(success_no_letter, 1),
    "% started the recertification process, whether or not they eventually completed the ",
    "process or were eligible to ",
    "retain benefits. However, among those receiving an additional letter, ",
    round(success_any_letter, 1),
    "% successfully started the process, a difference of ",
    round(te_any_letter, 1), " percentage points, ",
    "an increase in attempts of ",
    round(te_any_letter/success_no_letter * 100, 0),
    " percent over the baseline. The 95% confidence interval around the difference of ",
    round(te_any_letter, 1), " percentage points covers (",
    round(-max(test_any_letter_started$conf.int) * 100, 1), ", ",
    round(-min(test_any_letter_started$conf.int) * 100, 1), ").",
    sep = "")


# Open vs. Specific Letter

# Extract success rates:

success_specific <- unname(test_open_vs_specific_started$estimate["mean in group FALSE"]) * 100
success_open <-  unname(test_open_vs_specific_started$estimate["mean in group TRUE"]) * 100
te_open_letter <- success_open - success_specific

# Write sentences:

cat("Evidence also suggests that open appointment letters which only ",
    "note the overall deadline generate higher rates of initiating the recertification ",
    "process. Among those who received the open appointment letter, ",
    round(success_open, 1),
    "% successfully initiated the process. This rate was only ",
    round(success_specific, 1),
    "% among those receiving letters with specific appointment dates. ",
    "This represents a difference of about ",
    round(te_open_letter, 1), " percentage points, ",
    "an increase in attempts of ",
    round(te_open_letter/success_specific * 100, 0),
    " percent over the specific letter. The 95% confidence interval around the difference of ",
    round(te_open_letter, 1), " percentage points covers (",
    round(-max(test_open_vs_specific_started$conf.int) * 100, 1), ", ",
    round(-min(test_open_vs_specific_started$conf.int) * 100, 1), ").",
    sep = "")


# Clean up

rm(success_any_letter, success_no_letter, success_open, success_specific,
   te_any_letter, te_open_letter, test_any_letter_started,
   test_open_vs_specific_started)

rm(df_tr_group, dup_iccs, fig_1_any, fig_1_open, test_any_letter_unique,
   test_any_summ_percent_exp, test_open_summ, test_open_summ_exp,
   test_open_summ_percent_exp, test_open_vs_specific_unique)

rm(primary_point_size, primary_x_lab, primary_x_scale_angle,
   primary_x_scale_color, primary_x_scale_face, primary_x_scale_size,
   primary_y_lower, primary_y_upper)

# Save data with all outcomes defined  ----------------------------------------

save(df_baseline_outcomes,
     file = "data/df_modeling.RData")

