# *****************************************************************************
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# Create interrupted time series data set for Mark
# 03-25-2025
# *****************************************************************************

# %% Load libraries --------------------------------------------------------------
library(magrittr, include = "%<>%")
pacman::p_load(tidyverse,
               gtsummary,
               here,
               furrr,
               openxlsx)

# %% Load data ----------------------------------------------------------------
# Start with the original ee_ene data set, but then re-assign the index dates
# and create new variables. This is the output from create_ee_vs_ene_data.R
data <- read_csv("D:\\PATHWEIGH\\delivery_20240917\\data\\aim3_data_20240917.csv",
                 col_types = cols(`Cystatin C` = col_double()))

# Convert columns to factor
data %<>%
  mutate(across(Arb_PersonId:Arb_EncounterId, ~ as.factor(.x)))
                 
# %% Reassign the index dates -------------------------------------------------
# Get the first of the index date out of both phases
index_dates <- 
  data %>%
  arrange(Arb_PersonId, IndexDate) %>% # The first index date will become the one and only index date
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  ungroup() %>%
  select(Arb_PersonId, IndexDate)


# create interupted time series (its) data
its_data <- data

# Remove the index date column from its_data so that a new one can be added
its_data %<>% 
  select(-IndexDate, -N_months_post_lv, -LastVisit, -N_months_post_lv_cat, -LastVisit_Weight)

# Merge in with index_dates where it's only one index date per person
its_data %<>%
  left_join(., index_dates, by = "Arb_PersonId")


# Check to see if there are any missing index dates, should == 0
its_data %>%
  filter(is.na(IndexDate)) %>%
  nrow()

# Check to see how many unique IndexDates per patient
# Each patient gets one and only one IndexDate
# Should = 0
its_data %>%
  group_by(Arb_PersonId) %>%
  summarise(n_unique = n_distinct(IndexDate)) %>%
  filter(n_unique > 1) %>%
  nrow()


# %% Number of days since index date ---------------------------------------------
its_data %<>%
  mutate(N_days_post_id = EncounterDate - IndexDate)

# Check how many N_days_post_id are negative
# should be 0
its_data %>%
  filter(N_days_post_id < 0) %>%
  nrow()


# %% The number of days after the first weight measurement in intervention -------
first_visit_int <- its_data %>%
  filter(Intervention == 1) %>%
  arrange(Arb_PersonId, EncounterDate) %>%
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  ungroup() %>%
  select(Arb_PersonId, EncounterDate) %>%
  rename(first_visit_int = EncounterDate)

# Merge in the date of the first visit in the intervention
its_data %<>%
  left_join(., first_visit_int, by = "Arb_PersonId")


# Create N_days_post_int
its_data %<>%
  mutate(N_days_post_int = EncounterDate - first_visit_int)

# Check for negative values
# This is expected to produce negative values for visits
# Would also produce missing values for those that did not have an index visit
# in the intervention
its_data %>%
  filter(N_days_post_int < 0) %>%
  nrow()

# Address any NEGATIVE values, indicates the the encounter was before the first
# visit in the intervention. All visits prior to the first visit in the 
# intervention will be 0
its_data %<>%
  mutate(N_days_post_int = ifelse(N_days_post_int < 0, 0, N_days_post_int))

# Address any MISSING values, indicates that the patient never had a visit in
# the intervention, these are folks with visits only in the control phase
its_data %<>%
  mutate(N_days_post_int = ifelse(is.na(N_days_post_int), 0, N_days_post_int))


# %% Create Index_int ---------------------------------------------------------
# Binary indicator for patients who only have visits in the intervention phase

# Tabulate the number of visits in each phase for each patient
n_visits_per_pt <- 
  data %>%
  group_by(Arb_PersonId, Intervention.factor) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = Intervention.factor, values_from = n) %>%
  mutate(across(Control:Intervention, ~ifelse(is.na(.x), 0, .x)))

# Get the patient ids of those who only have visits in the intervention phase
int_only_pts <- 
  n_visits_per_pt %>%
  filter(Control == 0) %>%
  pull(Arb_PersonId)

# Create Index_int
its_data %<>%
  mutate(Index_int = ifelse(Arb_PersonId %in% int_only_pts, 1, 0))

# Check that for Index_int, N_days_post_id is equal to N_days_post_int
# There should not be any rows where those two value are not equal
# to eachother
its_data %>%
  filter(Index_int == 1) %>%
  filter(N_days_post_id != N_days_post_int) %>%
  nrow()


# /////////////////////////////////////////////////////////////////////////////
# The number of days after the first WPV in control ---------------------------
first_wpv_con <- its_data %>%
  filter(Intervention == 0, WP_Visit == 1) %>%
  arrange(Arb_PersonId, EncounterDate) %>%
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  ungroup() %>%
  select(Arb_PersonId, EncounterDate) %>%
  rename(first_wpv_con = EncounterDate)

# Merge in the first_wpv_con dates
its_data %<>%
  left_join(., first_wpv_con, by = "Arb_PersonId")

# Calculate the number of days after the first WPV
its_data %<>%
  mutate(N_days_post_wpv = EncounterDate - first_wpv_con)

# Check for negative values, indicates that the encounter occurred before the first wpv in control
its_data %>% 
  filter(N_days_post_wpv < 0) %>% 
  nrow()

# Address the negative values
its_data %<>%
  mutate(N_days_post_wpv = ifelse(N_days_post_wpv < 0, 0, N_days_post_wpv))


# Address the missing values, indicates that the patient never had a wpv in the
# control phase
its_data %<>%
  mutate(N_days_post_wpv = ifelse(is.na(N_days_post_wpv), 0, N_days_post_wpv))


# The number of days after the first WPV in intervention
first_wpv_int <- its_data %>%
  filter(Intervention == 1, WP_Visit == 1) %>%
  arrange(Arb_PersonId, EncounterDate) %>%
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  ungroup() %>%
  select(Arb_PersonId, EncounterDate) %>%
  rename(first_wpv_int = EncounterDate)

its_data %<>%
  left_join(., first_wpv_int, by = "Arb_PersonId")

its_data %<>%
  mutate(N_days_post_wpv_int = EncounterDate - first_wpv_int)

# Check for negative values
its_data %>%
  filter(N_days_post_wpv_int < 0) %>%
  nrow()

# Address the negative values, indicates that the encounter occurred before the first wpv in intervention
its_data %<>%
  mutate(N_days_post_wpv_int = ifelse(N_days_post_wpv_int < 0, 0, N_days_post_wpv_int))

# Address the missing values for those that never had a wpv in intervention
its_data %<>%
  mutate(N_days_post_wpv_int = ifelse(is.na(N_days_post_wpv_int), 0, N_days_post_wpv_int))


# /////////////////////////////////////////////////////////////////////////////
# Number of days since first PATHWEIGH visit ----------------------------------
pw_dates <- its_data %>%
  filter(PW_Visit == 1) %>%
  arrange(Arb_PersonId, EncounterDate) %>%
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  ungroup() %>%
  select(Arb_PersonId, EncounterDate) %>%
  rename(first_pw_date = EncounterDate)

# Merge the first pw date to the main data set
its_data %<>%
  left_join(., pw_dates, by = "Arb_PersonId")

# Calculate the N_days_post_PW
its_data %<>%
  mutate(N_days_post_pw = EncounterDate - first_pw_date)

# Check how many are negative values
its_data %>%
  filter(N_days_post_pw < 0) %>%
  nrow()

# Address the negative values
its_data %<>%
  mutate(N_days_post_pw = ifelse(N_days_post_pw < 0, 0, N_days_post_pw))

# For those that never got a PW visits, they will have NA for N_days_post_pw
# Fill those in with 0s
its_data %>%
  filter(is.na(first_pw_date)) %>%
  nrow()

its_data %<>%
  mutate(N_days_post_pw = ifelse(is.na(N_days_post_pw), 0, N_days_post_pw))


# %% Cumulative count of WPVs -------------------------------------------------
# WPV is a sum variable, WP_Visit is a binary variable
# Use WP_Visit
WPV_counts <- 
  its_data %>%
  filter(WP_Visit == 1) %>%
  arrange(Arb_PersonId, EncounterDate) %>%
  group_by(Arb_PersonId) %>%
  mutate(WPV_count = row_number()) %>% # Gets the row number
  ungroup() %>%
  select(Arb_EncounterId, WPV_count)

# Clean up the count variable and ensure that it is counting the number of 
# prior WPVs
its_data %<>%
  left_join(., WPV_counts, by = "Arb_EncounterId") %>%
  arrange(Arb_PersonId, EncounterDate) %>%
  group_by(Arb_PersonId) %>%
  fill(WPV_count, .direction = "down") %>%
  ungroup() %>%
  mutate(WPV_count = ifelse(EncounterDate == first_wpv_con, 0, WPV_count)) %>%
  mutate(WPV_count = ifelse(WPV_count > 1, WPV_count - 1, WPV_count)) %>%
  mutate(WPV_count = ifelse(is.na(WPV_count), 0, WPV_count)) 
  # filter(Arb_PersonId %in% c(2582832, 20988739)) %>%
  # select(Arb_PersonId, Arb_EncounterId, EncounterDate, Intervention, WP_Visit, WPV_count)

  

# WPV_count_int ---------------------------------------------------------------
WPV_counts_int <- 
  its_data %>%
  filter(Intervention == 1) %>%
  filter(WP_Visit == 1) %>%
  arrange(Arb_PersonId, EncounterDate) %>%
  group_by(Arb_PersonId) %>%
  mutate(WPV_count_int = row_number()) %>% # Gets the row number
  ungroup() %>%
  select(Arb_EncounterId, WPV_count_int)


its_data %<>%
  left_join(., WPV_counts_int, by = "Arb_EncounterId") %>%
  arrange(Arb_PersonId, EncounterDate) %>%
  group_by(Arb_PersonId) %>%
  fill(WPV_count_int, .direction = "down") %>%
  ungroup() %>%
  mutate(WPV_count_int = ifelse(EncounterDate == first_wpv_int, 0, WPV_count_int)) %>%
  mutate(WPV_count_int = ifelse(WPV_count_int > 1, WPV_count_int - 1, WPV_count_int)) %>%
  mutate(WPV_count_int = ifelse(is.na(WPV_count_int), 0, WPV_count_int))
  # filter(Arb_PersonId %in% c(2582832, 20988739)) %>%
  # select(Arb_PersonId, Arb_EncounterId, EncounterDate, Intervention, WP_Visit, WPV_count_int)



# *** May need to fix weight_bl to be the first visit with a recorded weight
weight_bls <- its_data %>%
  arrange(Arb_PersonId, EncounterDate) %>%
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  ungroup() %>%
  select(Arb_PersonId, Weight_bl)


its_data %<>%
  select(-Weight_bl) %>%
  left_join(., weight_bls, by = "Arb_PersonId")


# /////////////////////////////////////////////////////////////////////////////
# Check the max index date
its_data %>%
  pull(IndexDate) %>%
  max()



# /////////////////////////////////////////////////////////////////////////////
# *** Will need to check if the first visit needs to have an NPI?
# According to the SAP, no such specification was requested
# Reassign the IndexVisit to the first visit with weight as opposed to the 1st
# WPV
index_visits <- its_data %>%
  arrange(Arb_PersonId, EncounterDate) %>%
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  ungroup() %>%
  select(Arb_EncounterId) %>%
  mutate(IndexVisit = 1)

its_data %<>%
  select(-IndexVisit) %>%
  left_join(., index_visits, by = "Arb_EncounterId") %>%
  mutate(IndexVisit = ifelse(is.na(IndexVisit), 0, IndexVisit))


# Test data frame ----------------------------------------------
# Set the EncounterDate to as.Date()

test <- 
its_data %>%
  select(Arb_PersonId, EncounterDate, Arb_EncounterId, IndexDate, IndexVisit,
         Race_Ethnicity, Intervention, EE, Weight_bl, Weight_dv,
         WP_Visit, PW_Visit,
         starts_with("N_days"),
         WPV_count, WPV_count_int
      ) %>%
  arrange(Arb_PersonId) %>%
  head(100)

# write_csv(test, file = "D:\\PATHWEIGH\\delivery_20240917\\data\\aim3_its_test_data_20240917.csv")


# Write out the interrupted time series data set ------------------------------
write_csv(its_data, file = "D:\\PATHWEIGH\\delivery_20240917\\data\\aim3_its_data_20240917.csv")