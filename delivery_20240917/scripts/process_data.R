# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Authors:
# Carlos Rodriguez
# Krithika Suresh
# Emileigh Willems

# Description: _________________________________________________________________
# This script is used to generate a series of datasets used for statistical
# analyses, generating tables, figures, safety officer table, and output for NIH
# research progress performance report (RPPR).

# This script calls several functions and sub scripts to process and produce
# deliverables All of these functions and scripts are located in a centralized
# emr_data_processing library at the following path:
# ("D:/PATHWEIGH/emr_data_processing").

# The purpose of establishing a centralized library containing the scripts is to
# facilitate re-usability across multiple COMPASS data deliveries. This modular
# approach was also adopted to allow reusing functions on separate subsets of
# the data that may be of interest

# Definitions: _________________________________________________________________
# Eligible - Defined as meeting Age (18+) and BMI (25+) criteria only

# Enrolled - Defined as meeting Eligible and at least one of several weight
# prioritized visit (WPV) criteria or visits where weight/obesity was the reason
# for scheduling the visit. Visits under which PATHWEIGH tools were utilized
# fall under WPV criteria as well.

# Weigh prioritized visit (WPV) - A weight prioritized visit is one in which the
#     1) chief complaint matches at least one of a vector of key-words related
#       to weight loss/obesity
#     2) ICD-10 code at the encounter matches 1 of the icd-10 codes of interest
#     3) Use of specific EPIC flow sheets including PW, OBHP, or WMQ
#     4) Use of Smart Tools in the EPIC EHR

# Index date - For Eligible and Enrolled (EE) patients
#     The index date is initially the first weight prioritized visit in each
#     phase of the study that an eligible patient has. There should be one
#     index date per patient in each phase of the study. One at control and
#     another at intervention. The first index date available per patient marks
#     the date of enrollment.

# Index date - For Eligible and Not Enrolled (ENE) patients
#     Many of the subscripts/functions employed rely on the index date. ENE
#     patients are not enrolled and thus do not have and Index date per se.
#     However, within each function/script ENE patients are assigned a pseudo
#     index date so that the function operates properly. These pseudo index
#     dates are primarily assigned to capture a patient's labs, screeners,
#     referrals, and comorbidities.For ENE patients, they are first assigned to
#     a Cohort where the majority of their visits occur. Contrasted with EE
#     patients where they are assigned to the cohort pertaining to the index
#     visit. The pseudo index or reference date is assigned to the first visit
#     in their assigned Cohort. Ties, if any, are broken by assigning the
#     index date to the date of the earliest encounter.

# Outputs: _____________________________________________________________________
# visits - a data frame of patient interactions within the UC health system
# clinics participating in PATHWEIGH within a specified date range. Includes
# encounters from EE and ENE patients.

# visits_post_id - contains visits where the EncounterDate is on or after
#   the index date and is includes visits from EE patients only. Requires an
#   index date visit to qualify for a WPV AND to meet the eligibility criteria.
#   This data set set does not restrict visits after the index date to meet
#   WPV criteria.

# mod_data - contains visits from EE patients that had 2 or more visits in each
#   phase.

# pp_data - contains visits from EE patients that had 2 or more visits in both
#   the control and intervention phases.

# Study periods: _______________________________________________________________
# Year 0 March 17, 2020 - March 16, 2021, Baseline
# Year 1 March 17, 2021 - March 16, 2022, Group 1 crosses over to intervention
# Year 2 March 17, 2022 - March 16, 2023, Group 2 crosses over to intervention
# Year 3 March 17, 2023 - March 16, 2024, Group 3 crosses over to intervention
# Year 4 March 17, 2024 - end date of data delivery
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %% Load Packages -------------------------------------------------------------
library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(magrittr, include.only = "%<>%")
library(here)
library(tictoc)
library(gtsummary)
library(openxlsx)
library(furrr)

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

# Source sub-scripts -----------------------------------------------------------
# Loads all subscript functions into the workspace

# Loads the common set of functions for data deliveries 03-22-2023 and beyond
source(str_c(emr_dir, "subscripts/source_subscripts.R"))

# Encounter Table --------------------------------------------------------------
# Encounter table contains information from the visit including weight values
# Clean encounter table by changing -999 values in weight, creates smoking
# variable and removes asterisks from values, among other tasks. See function
# notes for more information.
read_pw_csv("encounter")
encounter <- prep_encounter(encounter)
invisible(gc())

# Prepare Patient Table --------------------------------------------------------
# Patient table contains demographic information.
# Recodes sex, race, ethnicity variables and removes asterisks from values
read_pw_csv("patient")
patient <- prep_patient(patient)
invisible(gc())

# Create visits data frame  ----------------------------------------------------
# The visits data frame represents all non-duplicated encounters that fall
# within the min (study start date) and max date (delivery date), joined with
# patient and clinic information. Represents the first value in the patient
# flow diagram of total number of encounters. Should be used to also determine
# the total number of unique patients before narrowing down to eligible patients
source(str_c(emr_dir, "subscripts/prep_visits.R"))

# Define WPV Variables ---------------------------------------------------------
# Controls identified by A-D in the analysis plan, Intervention identified via
# A and E-H.

## A. Weight must be recorded at the WPV satisfy criteria. A is implemented
# at the end of this section with an is.na(x) statement applied to all WPV_*
# columns instead of modifying each WPV_* column individually.

## B. WPV_CC -------------------------------------------------------------------
# Set flag for weight prioritized visit identified by chief complaint
visits <- wpv_cc(visits)

## C. WPV_ICD ------------------------------------------------------------------
# Set flag for weight prioritized visit identified by ICD-10 code
read_pw_csv("dx")
visits <- wpv_icd(visits)
rm(dx)

## D-E. WPV_Flow Sheets --------------------------------------------------------
# Set a flag for weight prioritized visits identified by the use of flow-sheets.
# Creates WPV_PW_flow, WPV_OBHPI, and WPV_WMQ.
read_pw_csv("flowsheets")
visits <- wpv_flow(visits)
rm(flowsheets)

## Intervention period WPVs
## F. WPV_Visit Type -----------------------------------------------------------
# Set a flag for a weight prioritized visit identified by a Visit Type ID,
# either in-person visit type or virtual visit type
# n.b. only for Intervention == 1
visits <- wpv_vtype(visits)

## G. WPV_Smart Set ------------------------------------------------------------
# Set a flag for visits with evidence of smart tools.
# n.b. only for Intervention == 1
read_pw_csv("smart")
visits <- wpv_smart(visits)
rm(smart)

## A. Weight must be recorded to meet WPV criteria -----------------------------
# Mark any WPV_* variable as a 0 if Weight == NA for all WPV columns
visits <- wpv_naweights(visits)

## Create WPV ------------------------------------------------------------------
# At this stage there shouldn't be any NAs in any of the WPV_* variables. This
# section is to ensure that there aren't any NAs, before creating a variable of
# the sum of WPV_* indicator variables.
if (visits %>%
      reframe(across(starts_with("WPV_"), is.na)) %>%
      reframe(across(starts_with("WPV_"), sum)) %>%
      rowSums() == 0) {

  # Apply rowSums over columns where names start with WPV_
  visits %<>%
    mutate(WPV = rowSums(select(., starts_with("WPV_"))))
} else {
  stop("NAs detected in WPV_ columns. Review creation of WPV variables.")
}

invisible(gc())

# Set IndexDate, IndexVisit, Cohort, and Enrolled Variables -----------------
# Sets an IndexDate for Eligible and Enrolled and Eligible and not Enrolled
# Sets a binary variable indicating which visits in each phase in the index
# visit
# Sets Cohort and whether or not the patient was enrolled
visits <- set_index_date(visits)

# Last visit in control phase --------------------------------------------------
# Set the last visit in control's weight, from the first visit in the
# intervention if and only if the first intervention has a weight, otherwise
# leave as is. Decided on 07/18/2024 to not proceed with this approach. Will
# cause errors in participant flow diagram if
# implemented. visits <- assign_last_visit_con(visits)

# Create separate EE and ENE datasets ------------------------------------------
# EE and ENE are split up into two separate datasets because some patients begin
# as not eligible and/or not enrolled. As the study progresses, some of these
# patients become eligible or enrolled. Non-eligible visits from enrolled
# patients are excluded. Those that occurred before the index date.

# Create the eligible and enrolled ---------------------------------------------
# For enrolled individuals (EE), filter only the visits after the index date
ee <- visits %>%
  filter(
    Enrolled == 1,
    EncounterDate >= IndexDate,
  )

ee_ids <- ee$Arb_PersonId

# Create the eligible not enrolled ---------------------------------------------
# Select the non-eligible and non-enrolled visits excluding patients that are
# in the ee subset
ene <- visits %>%
  filter(
    Eligible == 1,
    Enrolled == 0,
    !Arb_PersonId %in% ee_ids,
    EncounterDate >= IndexDate
  )

# Process EE and ENE for labs and comorbidities --------------------------------
# Process the ee and ene data for labs, procedures, referrals, EOSS, and
# comorbidities
## Stack ee and ene ----
ee_ene <- bind_rows(ee, ene)
rm(ee, ene)
invisible(gc())

## First prep the ee_ene data ----
# Should yield 59 columns
source(str_c(emr_dir, "subscripts/prep_ee_ene.R"))
ee_ene <- prep_ee_ene(ee_ene)

## Then get the labs, procedures, and comorbidities ----
source(str_c(emr_dir, "subscripts/proc_ee_ene.R"))
invisible(gc())

## Temporary code chunk to modify AST & ALT values, will need to be introduced
# into the proc_labs function during the next data delivery
ee_ene %<>%
  mutate(across(AST:ALT, ~ifelse(. > 100, NA, .))) %>%
  mutate(BMI = ifelse(Arb_EncounterId == 170254415001, NA, BMI))

## Create the EE variable
ee_ene %<>%
  mutate(EE = ifelse(Arb_PersonId %in% ee_ids, 1, 0))

# Make a copy of the ee_ene before implementing the cut off date
ee_ene_consort <- ee_ene

# Cutoff index date at 03/16/2024. No one after index date 3/16/2024 should be
# enrolled.
if (data_delivery_date == 20240917) {
  # Only applied to those in the intervention as there are not any patients that
  # have an enrollment/index date beyond 2024-03-17 in the control phase
  pt_ids_con <-
    ee_ene %>%
    filter(Intervention.factor == "Control",
           IndexVisit == 1) %>%
    select(Arb_PersonId) %>%
    distinct()

  # Get the patient ids where index visits on or after 2024-03-17.
  pt_ids_int <-
    ee_ene %>%
    filter(Intervention.factor == "Intervention",
           IndexVisit == 1,
           EncounterDate >= "2024-03-17") %>%
    select(Arb_PersonId) %>%
    distinct()

  # Filter pt_ids_int
  pt_ids_int %<>%
    filter(!Arb_PersonId %in% pt_ids_con$Arb_PersonId)

  # Remove the visits from those enrolled after the cutoff date
  ee_ene <- ee_ene %>%
    filter(!Arb_PersonId %in% pt_ids_int$Arb_PersonId)
}

## Break ee_ene data frame apart into EE (visits_post_id) and ENE ----
# visits_post_id naming maintained to work with legacy and downstream processing
# steps such as creating the enrollment table and the safety officer table.
visits_post_id <- ee_ene %>% filter(Arb_PersonId %in% ee_ids)

# ene, eligible but not enrolled
ene <- ee_ene %>% filter(!Arb_PersonId %in% ee_ids)

# Clear out memory
invisible(gc())

# Create the NIH enrollment table ----------------------------------------------
# Outputs a table and NIH style participant level data .csv file
# Displays the total amount of unique individuals that are enrolled in the
# study. Enrollment table captures the most recent index visit per patient.
# n.b. sometimes this will result in an error, restarting R and re-running
# remedies the error. Error is in the output of the enrollment table.
create_enrollment_table(visits_post_id)

# Create safety officer table --------------------------------------------------
# Captures safety officer table data from the specified date range
# date_2 is set to the max date available from the compass data delivery
# date_1 will be calculated as 1 year prior to date_2 within the function.

# If statement placed here as a crude way to run only if comorbidities are
# present
if (data.frame(grep("O2CPAPBIPAP", (names(visits_post_id)))) %>% nrow() > 0) {
  create_safety_officer_table(visits_post_id, date_2 = date_max)
}

# Make mod_data ----------------------------------------------------------------
# Create a data frame for the primary aim statistical models. Automatically
# outputs mod_data_full which consists of ee and ene data and mod_data_ee, the
# subset of ee patients only to ./data of the current project directory.
# All of these data sets were capped at N_months_post_id < 18
# N.B. mod_data no longer used
mod_data <- ee_ene %>% make_mod_data(., data_delivery_date)

# Make pp_data -----------------------------------------------------------------
# Create a data frame for the per_protocol analysis
# Requires visits_post_id for the index visits of mod_data patients and mod_data
# ee to determine who's had visits in both control and intervention
# The only input is delivery from workspace to write the files
source(str_c(emr_dir, "subscripts/make_pp_data.R"))
invisible(gc())

# Run the participant flow diagram script --------------------------------------
source(str_c(emr_dir, "subscripts/participant_flow_diagram_values_aim1a.R"))


# Save datasets ----------------------------------------------------------------
# can also use a substring on RData to just get the date.
save(visits,
  file = here(proj_root_dir, "data", str_c("all_visits_", RData))
)

# Can also be referred to as just the EE from the ee_ene data
save(
  visits_post_id,
  file = here(proj_root_dir, "data", str_c("visits_post_id_", RData))
)

save(
  ee_ene,
  file = here(proj_root_dir, "data", str_c("ee_ene_", RData))
)

beepr::beep(sound = 2)

# END OF SCRIPT ----------------------------------------------------------------