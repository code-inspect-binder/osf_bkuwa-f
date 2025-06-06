################################################################################

# Replication and extension of Amit et al. (2013, Experiment 2)

# Pretest, Study 1b

# Analyses of perception measures

################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(readr)

# Load data --------------------------------------------------------------------

raw <- read_csv("pretest_data_study1b.csv") %>% 
  slice(-1, -2) %>% 
  filter(Status == 0 & Finished == 1) %>% 
  type_convert()

# Convert perception ratings to original scale units ---------------------------

raw$percep_informative_0 <- raw$percep_informative-4
raw$percep_details_0 <- raw$percep_details-4

# Descriptives for perception ratings ------------------------------------------

raw %>%
  summarise(mean = mean(percep_informative_0), sd = sd(percep_informative_0))

raw %>%
  summarise(mean = mean(percep_details_0), sd = sd(percep_details_0))

# One-sample t-tests for perception ratings ------------------------------------

t.test(raw$percep_informative_0)
t.test(raw$percep_details_0)

# Effect sizes (d) for perception ratings --------------------------------------

d_inform = mean(raw$percep_informative_0)/sd(raw$percep_informative_0)
d_inform

d_details = mean(raw$percep_details_0)/sd(raw$percep_details_0)
d_details

