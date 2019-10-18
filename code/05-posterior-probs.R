# Posterior Probability Calculations
# Ryan T. Moore
# First: 3 April 2018
# Last: 25 September 2018

# Preliminaries -----------------------------------------------------------

library(dplyr)

# Load processed outcome data ---------------------------------------------

load("data/df_bayes.RData")

# Function to calculate posterior probability ---------------------------

pr_a_gr_b <- function(a, b, c, k_a){
  
  final_prob <- 0
  
  for(i in 0:k_a){
    
    lnum <- lfactorial(a + i -1) + lfactorial(b + c - 1) - lfactorial(a + i + b + c - 1) 
    ldenom1 <- log(b + i)
    ldenom2 <- lfactorial(i) + lfactorial(b - 1) - lfactorial(i + b)
    ldenom3 <- lfactorial(a - 1) + lfactorial(c - 1) - lfactorial(a + c - 1)
    
    laddend <- lnum - (ldenom1 + ldenom2 + ldenom3)
    
    final_prob <- final_prob + exp(laddend)
  }
  
  return(final_prob)
  
}


# Posterior prob that Open greater than Specific --------------------------

n_spec <- nrow(df_baseline_outcomes %>% filter(specific_appt))

k_spec <- sum((df_baseline_outcomes %>% 
               filter(specific_appt == TRUE))$successful_recert)

n_open <- nrow(df_baseline_outcomes %>% filter(open_appt))

k_open <- sum((df_baseline_outcomes %>% filter(open_appt == TRUE))$successful_recert)

a <- k_spec + 1

b <- n_open - k_open + 1

c <- n_spec - k_spec + 1

pr_open_gr_specific <- pr_a_gr_b(a, b, c, k_open)


# Define Any vs. No Letter ------------------------------------------------

n_noletter <- nrow(df_baseline_outcomes %>% filter(any_letter == FALSE))

k_noletter <- sum((df_baseline_outcomes %>% 
                 filter(any_letter == FALSE))$successful_recert)

n_anyletter <- nrow(df_baseline_outcomes %>% filter(any_letter))

k_anyletter <- sum((df_baseline_outcomes %>% filter(any_letter == TRUE))$successful_recert)

a <- k_noletter + 1

b <- n_anyletter - k_anyletter + 1

c <- n_noletter - k_noletter + 1

pr_any_gr_noletter <- pr_a_gr_b(a, b, c, k_anyletter)


# Write English ---------------------------------------------------------------

cat("As described in our pre-analysis plan, we calculate the Bayesian posterior ",
    "probability that one treatment condition results in higher recertification ",
    "rates than another.  We estimate that the open appointment date letters are ",
    "more effective than the letters with the specific appointment date to be about ",
    round(pr_open_gr_specific, 2), ".  The posterior probability that any letter is ",
    "more effective than no letter is approximately ", 
    if(pr_any_gr_noletter > 0.999){1}else(round(pr_any_gr_noletter, 2)),
    ".", sep = "")

