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
# Start with all visits to process the ee vs ene data differently because of a
# change in the way the index date is assigned. Instead of the first visit
# with a weight AND a provider with a valid NPI, we will use first visits with
# a weight.
load("D:/PATHWEIGH/delivery_20240917/data/all_visits_20240917.RData")

# Drop IndexDate, Eligible, and Enrolled columns, so that the Index date can
# be re-assigned. These are all variables that get created in the
# set_index_date and prep_ee_ene functions
visits %<>%
  select(-IndexDate, -IndexVisit, -Cohort, -Eligible, -Enrolled)

# Redefines the index date, only for the EE -----------------------------------
# Will also filter out rows without a weight and the not eligible not enrolled
# patients (NENE)
# This function is a modified version of the function used in the Aim1 pipeline
source("D:\\PATHWEIGH\\emr_data_processing\\subscripts\\set_index_date_ee_ene.R")

# Set the index dates
ee_ene <- set_index_date_ee_ene(visits)


## Load processing functions to prep ee_enee data ----
# After the index date is set, prep_ee_ene is re-used from the Aim1 pipeline
# but requires the subsequent functions to work properly
source("D:\\PATHWEIGH\\emr_data_processing\\subscripts\\prep_ee_ene.R")
source("D:\\PATHWEIGH\\emr_data_processing\\subscripts\\censor_visits.R")
source("D:\\PATHWEIGH\\emr_data_processing\\subscripts\\set_last_visit.R")
source("D:\\PATHWEIGH\\emr_data_processing\\subscripts\\any_pw_visit.R")

## Prepare ee_ene dataset
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

# %% Filter visits to fall within the specified dates -------------------------
# Create variables for days since first weight measurement and days since
# the first weight prioritized visit for the Eligible and Enrolled people only
# The eligible not enrolled patients will get zeros for number of days since
# wpv
data <- ee_ene

# Get the minimum date to ensure the visits are from 03-17-2020 and beyond
if (min(data$EncounterDate) != "2020-03-17") {
  warning("Minimum encounter date is not 2020-03-17")
}

# Get the maximum index date. The maximum encounter date for an index visit
# is 2024-03-17
max_date <- data %>%
  filter(IndexVisit == 1) %>%
  select(EncounterDate) %>%
  pull(EncounterDate) %>%
  max()

if(max_date > "2024-09-17") {
  warning("Maximum index date encounter date is beyond 2024-09-17")
}

# Get the number of patients where the index date is beyond 03/16/2024
# There are 6,887 unique patients with an index date beyond 03/16/2024
data %>%
  filter(IndexVisit == 1, EncounterDate > "2024-03-16") %>%
  select(Arb_PersonId) %>%
  distinct()

# Subsequent visits can occur up to 09-17-2024, but the index date needs
# to be before 03-17-2024 for intervention
data %>%
  filter(Intervention == 1) %>%
  filter(IndexVisit == 1) %>%
  pull(EncounterDate) %>%
  max()

# Get the Ids of the 6,887
ids_to_exclude <- 
  data %>%
    filter(Intervention == 1) %>%
    filter(IndexVisit == 1) %>%
    filter(EncounterDate >= "2024-03-17") %>%
    pull(Arb_PersonId)

# Exclude any patient and their visits in the intervention phase where the
# index date was after to 2024-03-16. Index Date is linked to each encounter 
data %<>%
  filter(IndexDate < "2024-03-17")

# Filter out patients with censored visits. These are any visits that occurr in
# the control phase after the patient was exposed to intervention and any visits
# thereafter
data %<>%
  filter(Censored == 0)

# Each patient must have 2 or more weight measurements total
# Check how many visits do not have a recorded weight
data %>%
  filter(is.na(Weight_kgs)) %>%
  nrow()

# Get the ids of patients that have 2 or more visits during the entire study
# period, after filtering out patients who had an index visit after 03-16-2024
ids_to_keep <- data %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  ungroup() %>%
  filter(n > 1) %>%
  pull(Arb_PersonId)

# Filter the data to keep only those with two or more visits after the new
# index date, but the index date must be before 03-17-2024
data %<>%
  filter(Arb_PersonId %in% ids_to_keep)

# Check total number of patients
# By this point there are 274,336 unique patients in the study.
# 03/25/2025 - I'm getting 274,182
data %>% 
  summarise(n = n_distinct(Arb_PersonId))


# %% DATA CHECKS FOR MARK 03/06/2025 ------------------------------------------
# /////////////////////////////////////////////////////////////////////////////
# The values in the following table summaries were used to manually input into
# a file (EE_ENE) for Mark Gritz on 03/06/2025
# Number of patients in the study by number of visits
ids_2_or_more <- data %>% 
  group_by(Arb_PersonId) %>%
  count() %>%
  filter(n > 1) %>%
  pull(Arb_PersonId)

# Tabulate the number of visits in each phase for each patient
n_visits_per_pt <- 
  data %>%
  group_by(Arb_PersonId, Intervention.factor) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = Intervention.factor, values_from = n) %>%
  mutate(across(Control:Intervention, ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(n = Control + Intervention) %>%
  mutate(n = ifelse(n >= 8, "8+", n))

# Total number of patients by number of visits
n_visits_per_pt %>%
  select(n) %>%
  tbl_summary()


# First and last weight in control period only
# i.e. all visits in control
n_visits_per_pt %>%
  filter(Intervention == 0) %>%
  select(n) %>%
  tbl_summary()

# First and last weight in intervention period only
n_visits_per_pt %>%
  filter(Control == 0) %>%
  select(n) %>%
  tbl_summary()

# At least 2 weight measurements in control and only 1 measure in intervention
n_visits_per_pt %>%
  filter(Control >= 2, Intervention == 1) %>%
  select(n) %>%
  tbl_summary()

# Only 1 measure in control and at least 2 weight measurements in intervention
n_visits_per_pt %>%
  filter(Control == 1, Intervention >= 2) %>%
  select(n) %>%
  tbl_summary()

# At least 2 weight measurements in control and at least 2 weight measurements in intervention
n_visits_per_pt %>%
  filter(Control >= 2, Intervention >= 2) %>%
  select(n) %>%
  tbl_summary()

# One visit in control, one visit in intervention
n_visits_per_pt %>%
  filter(Control == 1, Intervention == 1) %>%
  select(n) %>%
  tbl_summary()

# -------------------- EE only ------------------------------------------------
# Pull the EE ids from the visits level to make a time invariant variable at
# the patient level
ee_ids <- data %>%
  filter(Eligible == 1, Enrolled == 1) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  pull(Arb_PersonId)

# Create the EE variable
data %<>%
  mutate(EE = ifelse(Arb_PersonId %in% ee_ids, 1, 0))

# Tabulate the number of visits in each phase for each patient
n_visits_per_pt <- 
  data %>%
  filter(EE == 1) %>%
  group_by(Arb_PersonId, Intervention.factor) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = Intervention.factor, values_from = n) %>%
  mutate(across(Control:Intervention, ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(n = Control + Intervention) %>%
  mutate(n = ifelse(n >= 8, "8+", n))

# Total number of patients by number of visits
n_visits_per_pt %>%
  select(n) %>%
  tbl_summary()

# First and last weight in control period only
# i.e. all visits in control
n_visits_per_pt %>%
  filter(Intervention == 0) %>%
  select(n) %>%
  tbl_summary()

# First and last weight in intervention period only
n_visits_per_pt %>%
  filter(Control == 0) %>%
  select(n) %>%
  tbl_summary()

# At least 2 weight measurements in control and only 1 measure in intervention
n_visits_per_pt %>%
  filter(Control >= 2, Intervention == 1) %>%
  select(n) %>%
  tbl_summary()

# Only 1 measure in control and at least 2 weight measurements in intervention
n_visits_per_pt %>%
  filter(Control == 1, Intervention >= 2) %>%
  select(n) %>%
  tbl_summary()

# At least 2 weight measurements in control and at least 2 weight measurements in intervention
n_visits_per_pt %>%
  filter(Control >= 2, Intervention >= 2) %>%
  select(n) %>%
  tbl_summary()

# One visit in control, one visit in intervention
n_visits_per_pt %>%
  filter(Control == 1, Intervention == 1) %>%
  select(n) %>%
  tbl_summary()

# -------------------- ENE only ------------------------------------------------
# Tabulate the number of visits in each phase for each patient
n_visits_per_pt <- 
  data %>%
  filter(EE == 0) %>%
  group_by(Arb_PersonId, Intervention.factor) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = Intervention.factor, values_from = n) %>%
  mutate(across(Control:Intervention, ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(n = Control + Intervention) %>%
  mutate(n = ifelse(n >= 8, "8+", n))

# Total number of patients by number of visits
n_visits_per_pt %>%
  select(n) %>%
  tbl_summary()

# First and last weight in control period only
# i.e. all visits in control
n_visits_per_pt %>%
  filter(Intervention == 0) %>%
  select(n) %>%
  tbl_summary()

# First and last weight in intervention period only
n_visits_per_pt %>%
  filter(Control == 0) %>%
  select(n) %>%
  tbl_summary()

# At least 2 weight measurements in control and only 1 measure in intervention
n_visits_per_pt %>%
  filter(Control >= 2, Intervention == 1) %>%
  select(n) %>%
  tbl_summary()

# Only 1 measure in control and at least 2 weight measurements in intervention
n_visits_per_pt %>%
  filter(Control == 1, Intervention >= 2) %>%
  select(n) %>%
  tbl_summary()

# At least 2 weight measurements in control and at least 2 weight measurements in intervention
n_visits_per_pt %>%
  filter(Control >= 2, Intervention >= 2) %>%
  select(n) %>%
  tbl_summary()

# One visit in control, one visit in intervention
n_visits_per_pt %>%
  filter(Control == 1, Intervention == 1) %>%
  select(n) %>%
  tbl_summary()



# /////////////////////////////////////////////////////////////////////////////



# %% Capture visits from patients with two or more visits in one phase --------
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
# Filter out patients who are not in con_ids or int_ids in each respective phase
# Commented out to produce the Interrupted Time series data set, where two visits in one
# phase are not required.
# data %<>%
#   filter(Intervention.factor == "Control" & Arb_PersonId %in% con_ids |
#          Intervention.factor == "Intervention" & Arb_PersonId %in% int_ids)


# %% Create WP_Visit (weight prioritized visit) -------------------------------
# Identifies visits that were weight-prioritized meaning they were flagged for
# delivering some type of care for weight
data %<>%
  mutate(WP_Visit = ifelse(WPV > 0, 1, 0))

# Tabulate the number of WP_visits, check to make sure there are no NAs
table(data$WP_Visit, useNA ="ifany")


# %% Create PW_Visit ----------------------------------------------------------
# Identifies anyone who had a pathweigh visit
data %<>% 
  mutate(PW_Visit = if_else(WPV_WMQ == 1 | WPV_IP == 1 | WPV_TH == 1 | WPV_smart == 1, 1, 0))

# %% Create subsequent WPV variable -------------------------------------------
# Create a binary variable to indicate if it is a subsequent WPV
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

# %% Make cohort a time invariant variable
# Check if the cohort variable is time invariant
# Some will need to be assigned to a cohort if cohort is to be used
# *** go back and remove Cohort from Visits, then fillna()
# *** No one had more than 1 unique cohort assigned
data %>%
  group_by(Arb_PersonId) %>%
  summarise(n_cohorts = n_distinct(Cohort)) %>%
  filter(n_cohorts > 1)


# %% Create eligible and enrolled ---------------------------------------------
# *** This would operater at the encounter level, we may need something at the
# patient level.


# Enrolled is created in prep_ee_ene and may already be at the patient level
data %>%
  filter(Eligible == 1, Enrolled == 1) %>%
  select(WP_Visit) %>%
  pull(WP_Visit) %>%
  table()


# There should not be any visits before the index date
data %>%
  filter(EncounterDate < IndexDate) %>%
  nrow()

table(data$EE, useNA = "ifany")

# Create N_days_since_id and N_days_since_wpv----------------------------------
# This gets two enrollment dates for each phase
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

# This version would get one enrollment date per person
# # enrollment_dates <-
# data %>%
#   filter(Enrolled == 1, WP_Visit == 1) %>%
#   arrange(Arb_PersonId, EncounterDate) %>%
#   group_by(Arb_PersonId) %>%
#   slice_head() %>%
#   ungroup() %>%
#   select(Arb_PersonId, EncounterDate) %>%
#   mutate(EnrollmentDate = EncounterDate) %>%
#   select(-EncounterDate)

names(data)

# Create three key variables. Number of days after the index, 
# number of days after the first the first WPV for those in the EE group
# if the number of days after the first WPV is missing because the encounter
# is in the ENE group, then set it to zero.
data %<>%
  mutate(N_days_post_id = as.numeric(EncounterDate - IndexDate)) %>%
  mutate(N_days_post_wpv = as.numeric(EncounterDate - EnrollmentDate)) %>%
  mutate(N_days_post_wpv = if_else(is.na(N_days_post_wpv), 0, N_days_post_wpv))


# %% -----------------------------------------------------------------------------
# Age_cat + Sex + Race_Ethnicity + Year_at_ind + Weight_bl + Control

# Create the Age categorical variable
data %<>%
  mutate(Age_cat = ifelse(Age <= 45, "<=45", NA),
         Age_cat = ifelse(Age > 45 & Age <= 60, "45-60", Age_cat),
         Age_cat = ifelse(Age > 60, ">60", Age_cat))


# Create the year at index visit
data %<>%
  mutate(Year_at_ind = ifelse(IndexVisit == 1, Year, NA)) %>%
  group_by(Arb_PersonId, Intervention) %>%
  fill(Year_at_ind, .direction = "updown") %>%
  ungroup()

# Make Weight_bl
data %<>%
  mutate(Weight_bl = ifelse(IndexVisit == 1, Weight_kgs, NA)) %>%
  group_by(Arb_PersonId, Intervention) %>%
  fill(Weight_bl, .direction = "updown") %>%
  ungroup()
    
# Make weight dv
data %<>%
  mutate(Weight_dv = Weight_kgs)

# Additional data steps to take
data %<>%
  # Factor Arb Person Id (for repeated measures)
  mutate(Arb_PersonId = factor(Arb_PersonId)) %>%

  # Create a squared N_days_post_id variable for quadratic trends

  # Set the reference categories
  mutate(Sex = relevel(factor(Sex), ref = "Female"),
          Age_cat = relevel(factor(Age_cat), ref = "45-60"),
          Race_Ethnicity = relevel(Race_Ethnicity, 
                                  ref = "Non-Hispanic White"))


# Create binary indicator variables for the age, sex, race, and year at index
# categorical variables to be used in modeling.
data %<>%
  mutate(age_lt_45 = ifelse(Age_cat == "<=45", 1, 0),
         age_45_to_60 = ifelse(Age_cat == "45-60", 1, 0),
         age_gt_60 = ifelse(Age_cat == ">60", 1, 0),
         sex_m = ifelse(Sex == "Male", 1, 0),
         sex_f = ifelse(Sex == "Female", 1, 0),
         reth_nhw = ifelse(Race_Ethnicity == "Non-Hispanic White", 1, 0),
         reth_his = ifelse(Race_Ethnicity == "Hispanic or Latino", 1, 0),
         reth_blk = ifelse(Race_Ethnicity == "Black or African American", 1, 0),
         reth_asn = ifelse(Race_Ethnicity == "Asian", 1, 0),
         reth_oth = ifelse(Race_Ethnicity == "Other", 1, 0),
         reth_ukn = ifelse(Race_Ethnicity == "Unknown", 1, 0),
         year_at_ind0 = ifelse(Year_at_ind == "Year0", 1, 0),
         year_at_ind1 = ifelse(Year_at_ind == "Year1", 1, 0),
         year_at_ind2 = ifelse(Year_at_ind == "Year2", 1, 0),
         year_at_ind3 = ifelse(Year_at_ind == "Year3", 1, 0),
         year_at_ind4 = ifelse(Year_at_ind == "Year4", 1, 0))

# %% Save data to file --------------------------------------------------------
write_csv(data, file = "D:\\PATHWEIGH\\delivery_20240917\\data\\aim3_data_20240917.csv")


