# *****************************************************************************
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# Create interrupted time series data set for Mark
# 12-02-2024

# Capture the visits from ee and ene patients after eligibility or enrollment
# criteria are met, dropping all visits without a recorded weight value.
# Filters for patients that have a subsequent weight value. However it
# does not consider wether there are subsequen weight values in each phase.

# Questions for data structuring
# Index visits must be on or before 03/16/2024?
#   - There are patients where the index date is after 03/16/2024
# Filter out visits without an NPI? Bc I thought it was ok if NPI not available

# *****************************************************************************

# %% Load libraries --------------------------------------------------------------
library(magrittr, include = "%<>%")
pacman::p_load(tidyverse,
               gtsummary,
               here,
               furrr,
               openxlsx)


# Specify parameters -----------------------------------------------------------
RData <- "20240917.RData" # Used as an out to write files
# Set data delivery date as numerical
data_delivery_date <- 20240917

# For the proc_ene script
date_min <- as.Date("2020-03-17")
date_max <- as.Date(lubridate::ymd(data_delivery_date))

# Set the path to the emr_data_processing directory to load scripts/functions
# that are shared along all separate data deliveries
proj_root_dir <- str_c("delivery_", data_delivery_date)
proj_parent_dir <- str_remove(here(), proj_root_dir)
emr_dir <- str_c(proj_parent_dir, "/emr_data_processing/")


# %% Load the visits data frame -----------------------------------------------
load("D:/PATHWEIGH/delivery_20240917/data/all_visits_20240917.RData")

# Drop IndexDate, Eligible, and Enrolled columns, so that the new function can
# be applied to re-assign the IndexDates
visits %<>%
  select(-IndexDate, -IndexVisit, -Cohort, -Eligible, -Enrolled)

# Redefines the index date, only for the EE -----------------------------------
# Will also filter out rows without a weight and the not eligible not enrolled
# patients (NENE)
source("D:\\PATHWEIGH\\emr_data_processing\\subscripts\\set_index_date_ee_ene.R")
ee_ene <- set_index_date_ee_ene(visits)


## First prep the ee_ene data ----
source("D:\\PATHWEIGH\\emr_data_processing\\subscripts\\prep_ee_ene.R")
source("D:\\PATHWEIGH\\emr_data_processing\\subscripts\\censor_visits.R")
source("D:\\PATHWEIGH\\emr_data_processing\\subscripts\\set_last_visit.R")
source("D:\\PATHWEIGH\\emr_data_processing\\subscripts\\any_pw_visit.R")

ee_ene <- prep_ee_ene(ee_ene)


# Set data_file to the processed ee_ene data which contains all of the
# labs procedures, comorbidities, etc. if already processed.
data_file <- here(
  str_c("delivery_", data_delivery_date),
  "data",
  str_c("processed_ee_ene_aim3_", RData))


# If processed data exists, load it and merge it with the previous step's data
# in the pipeline. Otherwise process the data and store it for future use.
if (file.exists(data_file)) {
  # processed ee_ene data is saved as proc_ee_ene.Rdata. In order to avoid,
  # naming confusiong with proc_ee_ene() function. The following function
  # allows to load proc_ee_ene.RData as processed_ee_ene in the workspace.

  source("D:/PATHWEIGH/emr_data_processing/functions/load_rdata.R")

  # Load the processed ee_ene, that already has labs, procedures, and
  # comorbidities to avoid capturing them again when the script is run
  processed_ee_ene <- load_rdata(data_file)

  # If all of the encounters in ee_ene are in processed_ee_ene, then merge in
  # the labs, procedures, referrals, eoss, and comorbidities.
  if (all.equal(sort(ee_ene$Arb_EncounterId), sort(processed_ee_ene$Arb_EncounterId))) { # nolint: line_length_linter.
    
    # Load the column names that were added if the comorbidities were processed
    # previously processed. This will ensure that the column names that will be
    # merged will match with downstream steps
    load("D:/PATHWEIGH/delivery_20240917/data/new_col_names_aim3_20240917.RData")

    ee_ene <-
      left_join(ee_ene,
                (processed_ee_ene %>%
                   select(Arb_PersonId,
                          Arb_EncounterId,
                          EncounterDate, all_of(new_col_names))),
                by = c("Arb_PersonId", "Arb_EncounterId", "EncounterDate"))
  } else {
    stop("Not all visits in the input data frame match data that was previously processed. Review code.") # nolint: line_length_linter.
  }

} else {

  # Capture labs, meds, procedures -------------------------------------
  source("D:\\PATHWEIGH\\emr_data_processing\\functions\\read_pw_csv.R")
  plan(multisession, workers = 4)

  # Set the max globals size to 10 GB
  options(future.rng.onMisuse = "ignore",
          future.globals.maxSize = (10 * 1024^3))

  # PROC LABS MEDS PROCEDURES --------------------------------------------------
  # *** n.b. meds, bariatric procedures, and referrals captured at the last
  # visit are not valid, because their time window to capture these metrics is
  # from the date of the last visit to either the cross over date for control
  # phase visits, or 9-16-2024 for the intervention visits. Remnants of changes
  # to data capturing definitions made after an initial version.

  # Get the names before processing to be able to capture which columns were
  # added
  names_1_pre <- names(ee_ene)

  source(str_c(emr_dir, "subscripts/proc_labs_meds.R"))
  invisible(gc())
  ee_ene <- proc_labs_meds(ee_ene)

  # Get the names after processing to get the new columns
  names_1_post <- names(ee_ene)

  # Get the difference of the new names added to get the added columns
  names_1 <- names_1_post[!names_1_post %in% names_1_pre]

  # PROC EOSS ------------------------------------------------------------------
  names_2_pre <- names_1_post
  source(str_c(emr_dir, "subscripts/proc_eoss.R"))
  invisible(gc())
  ee_ene <- proc_eoss(ee_ene)

  names_2_post <- names(ee_ene)

  names_2 <- names_2_post[!names_2_post %in% names_2_pre]

  # PROC COMORBIDITIES ---------------------------------------------------------
  names_3_pre <- names_2_post

  source(str_c(emr_dir, "subscripts/proc_comorbidities.R"))
  invisible(gc())
  ee_ene <- proc_comorbidities(ee_ene)

  names_3_post <- names(ee_ene)

  names_3 <- names_3_post[!names_3_post %in% names_3_pre]

  # Save processed ee_ene added columns only for future use to merge in
  # labs, meds, procedures, eoss, and comorbidities

  # Collect all of the added column names and select them along with the
  # Arb_PersonId, Arb_EncounterId, and EncounterDate
  new_col_names <- c(names_1, names_2, names_3)


  # Save the output to avoid having to re-process the labs & comorbidities
  processed_ee_ene <-
    ee_ene %>%
    select(Arb_PersonId, Arb_EncounterId, EncounterDate,
            all_of(new_col_names))

  # save new_col_names
  save(new_col_names,
        file = here(str_c("delivery_", data_delivery_date),
                    "data",
                    str_c("new_col_names_aim3_", RData)))

  # save processed ee_ene
  save(processed_ee_ene,
        file = here(str_c("delivery_", data_delivery_date),
                    "data",
                    str_c("processed_ee_ene_aim3_", RData)))

  rm(processed_ee_ene)

}


# Create variables for days since first weight measurement and days since
# the first weight prioritized visit for the Eligible and Enrolled people only
# The eligible not enrolled patients will get zeros for number of days since
# wpv

# Load the 2024-09-17 data set ------------------------------------------------
source("D:/PATHWEIGH/emr_data_processing/functions/load_rdata.R")

# Load the processed ee_ene, that already has labs, procedures, and
# comorbidities to avoid capturing them again when the script is run
# data_file <- "D:/PATHWEIGH/delivery_20240917/data/ee_ene_20240917.RData"

data <- ee_ene

# Get the minimum date to ensure the visits are from 03-17-2020 and beyond
min(data$EncounterDate)

# Get the maximum index date
data %>%
  filter(IndexVisit == 1) %>%
  select(EncounterDate) %>%
  summarise(max = max(EncounterDate))

# Get the number of patients where the index date is beyond 03/16/2024
# There are 6,888 unique patients with an index date beyond 03/16/2024
data %>%
  filter(IndexVisit ==1, EncounterDate >= "2024-03-16") %>%
  select(Arb_PersonId) %>%
  distinct()

# Subsequent visits can occur up to 09-17-2024, but the index date needs
# to be before 03-17-2024
data %<>%
  filter(IndexDate <= "2024-03-16")

# Capture the patient ids that have 2 or more visits in the control phase
con_ids <- 
  data %>%
  filter(Intervention.factor == "Control") %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  ungroup() %>%
  filter(n > 1) %>%
  pull(Arb_PersonId)

# Capture the patient ids that have 2 or more visits in the intervention phase
int_ids <- 
  data %>%
  filter(Intervention.factor == "Intervention") %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  ungroup() %>%
  filter(n > 1) %>%
  pull(Arb_PersonId)

# Filter data to only those that have 2 visits in at least one phase
data %>%
  filter(Intervention.factor == "Control" & Arb_PersonId %in% con_ids |
         Intervention.factor == "Intervention" & Arb_PersonId %in% int_ids) %>%
  group_by(Arb_PersonId, Intervention.factor) %>%
  count() %>%
  arrange(Arb_PersonId)

data %<>%
  filter(Intervention.factor == "Control" & Arb_PersonId %in% con_ids |
         Intervention.factor == "Intervention" & Arb_PersonId %in% int_ids)

# After those with at least one subsequent visit are removed
# the ENE for ene drops to 183,060
data %>%
  filter(Enrolled == 0) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  slice_head() %>%
  ungroup() %>%
  nrow()

# Create WP_Visit -------------------------------------------------------------
# Identifies visits that were weight-prioritized meaning they were flagged for
# delivering some type of care for weight
data %<>%
  mutate(WP_Visit = ifelse(WPV > 0, 1, 0))

table(data$WP_Visit, useNA ="ifany")

# Create PW_Visit -------------------------------------------------------------
# Identifies anyone who had a pathweigh visit
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
# rows to 0 to indicate they were not a subsequent wpv.
data %<>%
  mutate(subsequent_WPV = ifelse(is.na(subsequent_WPV), 0, subsequent_WPV))

# Check for NAs in the newly created variables. There should not be any NAs
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
data %<>%
  mutate(EE = ifelse(Eligible == 1 & Enrolled == 1, 1, 0))

table(data$EE, useNA = "ifany")

# Create N_days_since_id and N_days_since_wpv----------------------------------
enrollment_dates <- data %>%
  filter(Enrolled == 1, WP_Visit == 1) %>%
  arrange(Arb_PersonId, Intervention.factor, EncounterDate) %>%
  group_by(Arb_PersonId, Intervention.factor) %>%
  slice_head() %>%
  ungroup() %>%
  select(Arb_PersonId, Intervention.factor, EncounterDate) %>%
  mutate(EnrollmentDate = EncounterDate) %>%
  select(-EncounterDate)

data %<>%
  left_join(., enrollment_dates, by = c("Arb_PersonId", "Intervention.factor"))

names(data)

data %<>%
  # tail() %>%
  mutate(N_days_post_id = as.numeric(EncounterDate - IndexDate)) %>%
  mutate(N_days_post_wpv = as.numeric(EncounterDate - EnrollmentDate)) %>%
  # select(Arb_PersonId, EncounterDate, IndexDate, EnrollmentDate, 
        #  Intervention.factor, N_days_post_id, N_days_post_wpv) %>%
  # filter(is.na(N_days_post_wpv)) %>%
  mutate(N_days_post_wpv = if_else(is.na(N_days_post_wpv), 0, N_days_post_wpv))

# data %>%
#   group_by(Arb_PersonId) %>%
#   arrange(EncounterDate) %>%
#   slice_head() %>%
#   ungroup() %>%
#   select(Age, Weight_kgs, Sex, Race_Ethnicity, Insurance, BMI,
#          Systolic_blood_pressure, Diastolic_blood_pressure, EE) %>%
#   mutate(EE = ifelse(EE == 1, "EE", "ENE")) %>%
#   tbl_summary(by = "EE",
#               statistic = list(all_continuous() ~ c("{mean} ({sd})"))) %>%
#   as_gt() %>%
#   gt::gtsave(
#     filename = "D:/PATHWEIGH/delivery_20240917/scripts/ee_ene_draft.docx"
#   )
