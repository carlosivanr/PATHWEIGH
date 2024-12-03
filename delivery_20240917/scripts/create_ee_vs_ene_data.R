# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# Create interrupted time series data set for Mark
# 12-02-2024

# Capture the visits from ee and ene patients after eligibility or enrollment
# criteria are met, dropping all visits without a recorded weight value.
# Filters for patients that have a subsequent weight value. However it
# does not consider wether there are subsequen weight values in each phase.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load libraries --------------------------------------------------------------
library(magrittr, include = "%<>%")
pacman::p_load(tidyverse,
                gtsummary)

# Load the 2024-09-17 data set ------------------------------------------------

# File loading function that will allow loading a file and assign it to a
# different name in the global environment
load_rdata <- function(file_name) {
  #loads an RData file, and returns it
  load(file_name)
  get(ls()[ls() != "file_name"])
}

# Load the processed ee_ene, that already has labs, procedures, and
# comorbidities to avoid capturing them again when the script is run
data_file <- "D:/PATHWEIGH/delivery_20240917/data/ee_ene_20240917.RData"

data <- load_rdata(data_file)

# # Load the all_visits data frame to get the visits that are not in ee_ene
# load("D:/PATHWEIGH/delivery_20240917/data/all_visits_20240917.RData")

# # Keep only the visits from the ee_ene patients
# visits %<>%
#   filter(Arb_PersonId %in% data$Arb_PersonId)

# # Keep only the visits that are not already in ee_ene
# visits %<>%
#   filter(!Arb_EncounterId %in% data$Arb_EncounterId)

# # Stack the visits with data and sort them
# data <- bind_rows(data, visits) %>%
#   group_by(Arb_PersonId) %>%
#   arrange(EncounterDate) %>%
#   ungroup()

# Get the minimum date to ensure the visits are from 03-17-2020 and beyond
min(data$EncounterDate)

# Filter out visits where a weight is not recorded ----------------------------
data %<>%
  drop_na(Weight_kgs)

# Ensure everyone has a follow up weight value --------------------------------
ids_to_keep <- data %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  filter(n > 1) %>%
  pull(Arb_PersonId)

data %<>%
  filter(Arb_PersonId %in% ids_to_keep)

# Create WP_Visit -------------------------------------------------------------
data %<>%
  mutate(WP_Visit = ifelse(WPV > 0, 1, 0))

table(data$WP_Visit, useNA ="ifany")

# Create PW_Visit
data %<>% 
  mutate(PW_Visit = if_else(WPV_WMQ == 1 | WPV_IP == 1 | WPV_TH == 1 | WPV_smart == 1, 1, 0))

# Create a variable to indicate if it is a subsequent WPV ---------------------
# First create a data frame where the input is grouped, then arranged by date,
# then the row number is added. The first WPV is initially set to 1, so any row
# that = 1 is set to 0, else it is set to 1 to indicate a subsequent WPV. Then
# select only the Arb_EncounterId and subsequent WPV indicator.
subsequent_wpv <-
  data %>% 
  filter(WPV > 0) %>%
  select(Arb_PersonId, Arb_EncounterId, EncounterDate, WPV) %>%
  group_by(Arb_PersonId) %>% 
  arrange(EncounterDate) %>%
  mutate(subsequent_WPV = row_number()) %>%
  ungroup() %>%
  mutate(subsequent_WPV = ifelse(subsequent_WPV == 1, 0, 1)) %>%
  select(Arb_EncounterId, subsequent_WPV)

# Merge the subsequent WPV indicator by Arb_EncounterId
data <-
  left_join(data,
            subsequent_wpv,
            by = "Arb_EncounterId")

# After merging, visits that were not WPVs will have a missing value. Set these
# rows to 0 to indicate they were not a subsequent wpv. In fact, these will not
# be WPVs at all.
data %<>%
  mutate(subsequent_WPV = ifelse(is.na(subsequent_WPV), 0, subsequent_WPV))

# Check for NAs in the newly created variables
table(data$subsequent_WPV, useNA = "ifany")
table(data$PW_Visit, useNA = "ifany")
table(data$WP_Visit, useNA = "ifany")

# Check if the cohort variable is time invariant
# Some will need to be assigned to a cohort if cohort is to be used
# *** go back and remove Cohort from Visits, then fillna()
data %>%
  group_by(Arb_PersonId) %>%
  summarise(n_cohorts = n_distinct(Cohort)) %>%
  filter(n_cohorts > 1)


# Create eligible and enrolled ------------------------------------------------
table(data$EE, useNA = "ifany")

# Not needed if we are not using visits before the index
# data %<>%
#   group_by(Arb_PersonId) %>%
#   fill(EE, .direction = "updown") %>%
#   ungroup()


# Create a table 1 to compare ee_ene
data %<>%
  arrange(Arb_PersonId, EncounterDate)

data %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  slice_head() %>%
  ungroup() %>%
  select(Age, Weight_kgs, Sex, Race_Ethnicity, Insurance, BMI,
         Systolic_blood_pressure, Diastolic_blood_pressure, EE) %>%
  mutate(EE = ifelse(EE == 1, "EE", "ENE")) %>%
  tbl_summary(by = "EE",
              statistic = list(all_continuous() ~ c("{mean} ({sd})"))) %>%
  as_gt() %>%
  gt::gtsave(
    filename = "D:/PATHWEIGH/delivery_20240917/scripts/ee_ene_draft.docx"
  )