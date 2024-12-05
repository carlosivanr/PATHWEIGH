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
source("D:/PATHWEIGH/emr_data_processing/functions/load_rdata.R")

# Load the processed ee_ene, that already has labs, procedures, and
# comorbidities to avoid capturing them again when the script is run
data_file <- "D:/PATHWEIGH/delivery_20240917/data/ee_ene_20240917.RData"

data <- load_rdata(data_file)

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

# After those with at least one subsequent visit are removed
# the ENE for ene drops to 206,365
data %>%
  filter(EE == 0) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  slice_head() %>%
  ungroup() %>%
  nrow()

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

# For the EE, come up with a different Index visit algorithm ------------------
# 1. --------------------------------------------------------------------------
# Load the all_visits data frame to get the visits that are not in ee_ene
load("D:/PATHWEIGH/delivery_20240917/data/all_visits_20240917.RData")

# Reload data frame because we want to change the group in EE
data <- load_rdata(data_file)

# Keep only the visits from the ee_ene patients
visits %<>%
  filter(Arb_PersonId %in% data$Arb_PersonId)

# 2. Drop any visits with missing weight
visits %<>%
  drop_na(Weight_kgs)

# 3. Assign the EE variable
visits %<>%
  mutate(EE = ifelse(Arb_PersonId %in% (data %>% filter(EE == 1) %>% select(Arb_PersonId) %>% distinct() %>% pull(Arb_PersonId)), 1, 0))

visits %<>%
  arrange(Arb_PersonId, EncounterDate)

# 4. Check ene in visits vs ene in data 
# 277,242 at this point
visits %>%
  filter(EE == 0) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

data %>%
  filter(EE == 0) %>%
  # drop_na(Weight_kgs) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()


# 5. Split the data frame into ee and ene, and work on EE ONLY
ee <- visits %>% filter(EE == 1)
ene <- visits %>% filter(EE == 0)

# 6. Redefine the index eligible variable to include any visit that meets criteria
ee %<>%
  mutate(IndexVisitEligible = ifelse(Age >= 18 & BMI >= 25, 1, 0),
         IndexVisitEligible = ifelse(is.na(IndexVisitEligible),
                                     0, IndexVisitEligible)
)

ee %<>%
  mutate(IndexVisitEligible = ifelse(is.na(ProviderNpi), 0, IndexVisitEligible),
         IndexVisitEligible = ifelse(is.na(Weight_kgs), 0, IndexVisitEligible))

# 7. filter data by index eligible visits, Group data by Arb_PersonId, sort by 
# encounter date, slice head, pull Arb_EncounterId and save (These will be the
# encounter ids of the index visits)
index_visits_ee <- ee %>%
  filter(IndexVisitEligible == 1) %>%
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  pull(Arb_EncounterId)

# 8. Set the index visit if the Arb_EncounterId is in the pulled vector of Arb_EncounterIds
ee %<>%
  mutate(IndexVisit = ifelse(Arb_EncounterId %in% index_visits_ee, 1, 0))

# 9. Set the IndexDate as that of the new IndexVisit's Encounter Date, then filter
# visits to only those on or after the IndexDate
ee %<>%
  mutate(IndexDate = ifelse(IndexVisit == 1, as.character(EncounterDate), NA)) %>%
  group_by(Arb_PersonId) %>%
  fill(IndexDate, .direction = "down") %>%
  ungroup() %>%
  # select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexVisitEligible, IndexVisit, IndexDate) %>%
  filter(EncounterDate >= IndexDate)

# 10. Restack the EE and ENE subsets
ee %<>% 
  mutate(IndexDate = as.Date(IndexDate))

# Filter the encounter Ids for those in data where EE == 0
ene %<>% 
  filter(Arb_EncounterId %in% (data %>% filter(EE == 0) %>% pull(Arb_EncounterId)))

# Create a new data frame with the redefined indexes
redef_index <- bind_rows(ee, ene)


# Then see which patients have a subsequent visit
# Ensure everyone has a follow up weight value --------------------------------
ids_to_keep <- redef_index %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  filter(n > 1) %>%
  pull(Arb_PersonId)

# Filter data by those patients that have a subsequent visit and tabulates
redef_index %<>%
  filter(Arb_PersonId %in% ids_to_keep)

# Check so see how many are in the redef_index data frame
redef_index %>%
  filter(EE == 0) %>%
  # drop_na(Weight_kgs) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()


# SO FAR THINGS LOOK GOOD
redef_index %>%
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
    filename = "D:/PATHWEIGH/delivery_20240917/scripts/ee_ene_redefined_index_draft.docx"
  )