#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Authors:
# Carlos Rodriguez
# Krithika Suresh
# Emileigh Willems


# Description: _________________________________________________________________
# This script is part 2 in the PATHWEIGH EMR data pipeline. It is used
# to generate a series of datasets used for statistical analyses, generating
# tables, figures, safety officer table, and output for NIH research progress
# performance report (RPPR). The main input is to load an R Image workspace that
# contains all of the COMPASS .csv files were imported in part 1
# (01_create_rdata_image.R script) as a workspace.

# This script calls several functions and sub scripts to process and produce
# deliverables All of these functions and scripts are located in a centralized
# emr_data_processing library at the following path:
# ("S:/FM/PATHWEIGH/Quantitative/Projects/emr_data_processing").

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
#   phase. This is the primary analysis data set

# Study periods: _______________________________________________________________
# Year 0 March 17, 2020 - March 16, 2021, Baseline
# Year 1 March 17, 2021 - March 16, 2022, Group 1 crosses over to intervention
# Year 2 March 17, 2022 - March 16, 2023, Group 2 crosses over to intervention
# Year 3 March 17, 2022 - March 16, 2024, Group 3 crosses over to intervention



# Built with R 4.2.2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load Packages ----------------------------------------------------------------
# pacman::p_load(here,         # For managing directory paths
#                magrittr,     # For the assignment pipe operator %<>%
#                tidyverse,    # For data manipulation tools
#                furrr,        # For parallel processing
#                data.table,   # For reading .csv files
#                openxlsx,     # For reading .xlsx files
#                tictoc,       # For timing and benchmarking functions
#                gtsummary,    # For tables
#                install = FALSE)

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(magrittr, include.only = "%<>%")
library(here)
library(tictoc)
library(gtsummary)
library(openxlsx)
library(furrr)

# Set number of workers for furrr ----------------------------------------------
# Sets the number of workers for parallel processing
cores <- 1 # !!! 8 cores or more causes multisession failure
labels <- seq_along(1:cores) # used for split_by group in nested functions
plan(sequential, workers = cores)

# # Set plan()
# # Algorithms in proc_ee_ene() utilize future_map_dfr() for parallel processing,
# # but it has been unreliable after implementing renv() for package management
# # into the R project. If multisession results in an error, set to sequential for
# processing in serial without having to modify the underlying code.
# if (grepl("Error", result[1])) {
#   plan(sequential) # If plan multisession for parallel fails, set to serial
# }

# Specify parameters -----------------------------------------------------------
RData <- "20240326.RData"

# For the proc_ene script
date_min <- as.Date("2020-03-17")
date_max <- as.Date(lubridate::ymd(substr(RData, 1, 8)))

# print.text <- FALSE
# save_data_sets <- TRUE


# Load Rimage data -------------------------------------------------------------
# Load .Rdata to preserve any formatting from compass delivery import
# load(here("data", RData))


# Source sub-scripts -----------------------------------------------------------
# Loads all subscript functions into the workspace

# Set the path to the emr_data_processing directory to load scripts/functions
# that are shared along all separate data delivery dates
data_delivery_date <- 20240326
proj_root_dir <- str_c("delivery_", data_delivery_date)
proj_parent_dir <- str_remove(here(), proj_root_dir)
emr_dir <- str_c(proj_parent_dir, "/emr_data_processing/")

# Loads the common set of functions for data deliveries from 03-22-2023 and
# onward in the emr_data_processing directory
source(str_c(emr_dir, "subscripts/source_subscripts.R"))


# Prepare Encounter Table ------------------------------------------------------
# Encounter table contains information from the encounter including weight vals
# Clean encounter table by changing -999 values in weight, creates smoking
# variable and removes asterisks from values, among other tasks. See function
# notes for more information.
read_pw_csv("encounter")

encounter <- prep_encounter(encounter)

invisible(gc())

# Prepare Patient Table --------------------------------------------------------
# Patient table contains demographic information.
# recodes sex, race, ethnicity variables and removes asterisks from values
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
# dx needed for capture_comorbidities() in proc_ee_ene()

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
# WPV is a sum of the number of WPV_* variables. Checks for NAs in any of the
# WPV_* columns before creating the WPV variable since the creation of WPV will
# be affected by NAs.
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
tic()
visits <- set_index_date(visits)
toc()
beepr::beep(sound = 2)
invisible(gc())


# Last visit in control phase --------------------------------------------------
# Set the last visit in control's weight, from the first visit in the
# intervention if and only if the first intervention has a weight, otherwise
# leave as is work with the visits data frame. Decided on 07/18/2024 to not
# proceed with this approach. Will cause errors in participant flow diagram if
# implemented. visits <- assign_last_visit_con(visits)

# Create separate EE and ENE datasets ------------------------------------------
# EE and ENE are split up into two separate datasets because some patients begin
# as not eligible and/or not enrolled. As the study progresses, some of these
# patients become eligible or enrolled. Non-eligible visits from enrolled
# patients are excluded. Those that occurred before the index date.

# Create the eligible and enrolled ---------------------------------------------
# For enrolled individuals (EE), filter only the visits after the index date
ee <- visits %>%
  filter(Enrolled == 1,
         EncounterDate >= IndexDate)

ee_ids <- ee$Arb_PersonId

# Create the eligible not enrolled ---------------------------------------------
# Select the non-eligible and non-enrolled visits excluding patients that are
# in the ee subset
ene <- visits %>%
  filter(Eligible == 1,
         Enrolled == 0,
         !Arb_PersonId %in% ee_ids,
         EncounterDate >= IndexDate)

# Process EE and ENE for labs and comorbidities --------------------------------
# Process the ee and ene data for labs, procedures, referrals, EOSS, and
# comorbidities
## Stack ee and ene ----
ee_ene <- bind_rows(ee, ene)
rm(ee, ene)
invisible(gc())

## Apply processing function ----
# Both arguments should be set to TRUE. Prior functionality of setting to FALSE
# was built in to expedite the creation of the modeling data set. However,
# project requirement changes to include meds, labs, procedures, etc. in the
# modeling data rendered this feature obsolete.
tic()
# ee_ene <- proc_ee_ene(ee_ene, proc_labs = TRUE, proc_dx = TRUE)
source(str_c(emr_dir, "subscripts/proc_ee_ene.R"))
rm(ind_con, ind_int, lv_con, lv_int, dx_sub, dx_sub_coi_count)
toc()
beepr::beep(sound = 2)
invisible(gc())

## Create the EE variable
ee_ene %<>%
  mutate(EE = ifelse(Arb_PersonId %in% ee_ids, 1, 0))

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


# Make mod_data ----------------------------------------------------------------
# Create a data frame for the primary aim statistical models. Automatically
# outputs mod_data_full which consists of ee and ene data and mod_data_ee, the
# subset of ee patients only to ./data of the current project directory.
mod_data <- ee_ene %>% make_mod_data(., data_delivery_date)


# Make pp_data -----------------------------------------------------------------
# Create a data frame for the per_protocol analysis
# Requires visits_post_id for the index visits of mod_data patients and mod_data
# ee to determine who's had visits in both control and intervention
# The only input is delivery to write the files, otherwise
source(str_c(emr_dir, "subscripts/make_pp_data.R"))
invisible(gc())


# Save datasets ----------------------------------------------------------------
# can also use a substring on RData to just get the date.
save(visits, file = here(proj_root_dir, "data", paste0("processed_all_visits_", RData)))
save(visits_post_id, file = here(proj_root_dir, "data", paste0("processed_visits_post_id_", RData)))
save(ee_ene, file = here(proj_root_dir, "data", paste0("ee_ene_", RData)))
save(pp_data, file = here(proj_root_dir, "data", paste0("pp_data_", RData)))
saveRDS(comorbidity_names, file = here(proj_root_dir, "data", str_c("comorbidity_names_", data_delivery_date,".RDS")))


# LEFT OFF HERE 
# WORKING ON GETTING TABLES


# Participant flow diagrams ------------------------------------------------
# These scripts will generate a table of values used as input in the consort
# power point file.
# ITT data for all eligible patients with 2 or more visits in any phase
source(str_c(emr_dir, "subscripts/participant_flow_diagram_data.R"))

# PP data for eligible patients that have 2 or more visits in both phases
source(str_c(emr_dir, "subscripts/pp_participant_flow_diagram_data.R"))


# Make tables ------------------------------------------------------------------
# Must be performed after datasets are saved, since there are two dependencies
# on the saved data sets to be used as inputs to generate the lmer model output
# and the clinic engagement histograms.

# The object mod_data is a list which contains the full ee and ene combined data
# set and the ee data by itself which is a subset of the ee only patients with
# 2 or more visits for modeling.
# n.b. mod_data[["ee"]] is filtered to 18 months post id
source(str_c(emr_dir, "subscripts/create_aim1_tables_v2.R"))

# The input data frame to make_tables() is used to d
# specific patients with 2 or more visits. Otherwise visits_post_id can be used
# for all patients, even those with only 1 visit in each phase. Does not work
# with the last visit.
mod_data[["ee"]] %>%
  make_tables(.)

beepr::beep(sound = 2)

invisible(gc())


# Make tables for pp
source(str_c(emr_dir, "subscripts/create_pp_tables_figs.R"))
pp_data %>%
  make_pp_tables(.)

# Create safety officer table --------------------------------------------------
# Captures safety officer table data from the specified date range
# date_2 is set to the max date available from the compass data delivery
# date_1 will be calculated as 1 year prior to date_2 within the function.

# *** requires processed labs, procedures, and comorbidities
# *** Currently uses visits post id, but it may be worth using mod_data
if (data.frame(grep("O2CPAPBIPAP", (names(visits_post_id)))) %>% nrow() > 0) {
  create_safety_officer_table(visits_post_id, date_2 = date_min)
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Check visits vs visits_post_id -----------------------------------------------
# Includes all visits for EE patients after the index visit, regardless of
# whether or not those visits were WPV or if they have a recorded weight.
# n.b. Enrolled is a visit level indicator
# For 20221017 equals 186,676
# For 20230322 equals 246,067
# For 20231010 equals 323,015
# For 20240326 equals 405,190
visits %>%
  filter(Enrolled == 1) %>%
  nrow()

# Visits that meet WPV criteria
# For 20221017 equals 77,968
# For 20230322 equals 99,484
# For 20231010 equals 130,413
# Ended up with 130,467 after mods to set_index for ENE
# For 20240326  159,156
visits %>%
  filter(WPV > 0, Eligible == 1, EncounterDate >= IndexDate) %>%
  nrow()

# Check to see that each patient has a WPV -------------------------------------
# For 2022-10-17 - 46,057 unique patient ids
# For 2023-03-22 - 54,625 unique patient ids
# For 2023-10-10 - 66,067 unique patient ids
# For 2024-03-26 - 75,952 unique patient ids
n_distinct(visits_post_id$Arb_PersonId)

# How many visits where encounter date == index_date, in intervention ----------
# For 2022-10-17, 19,419 for intervention
# For 2023-03-22, 27,301 for intervention
# For 2023-10-10, 44,143 for intervention
# For 2024-03-26, 57,053 for intervention
visits_post_id %>%
  filter(EncounterDate == IndexDate,
         Intervention == 1) %>%
  nrow()

# For 2022-10-17, 32,479 for control
# For 2023-03-23, 35,035 for control
# For 2023-10-10, 35,076 for control
# For 2024-03-26, 35,043 for control
visits_post_id %>%
  filter(EncounterDate == IndexDate,
         Intervention == 0) %>%
  nrow()

# How many visits in the mod_data[[ee]] data frame.
# n.b. data may be further filtered to visits within 18 months of the index
# date in subsequent analysis steps
# For 2024-03-26, 210,124 overall (recorded 07/10/2024)
#  - same number if the first non-wpv intervention visit with a weight is
#   switched to the control phase, could just be that the rows are switched
mod_data[["ee"]] %>%
  filter(N_months_post_id <= 18) %>%
  nrow()

mod_data[["ee"]] %>%
  filter(N_months_post_id <= 18) %>%
  group_by(Phase) %>%
  count()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Count lines of uncommented code
# Declare line counting function -----------------------------------------------
foo <- function(path) {
  rln <- read_lines(path)
  rln <- rln[-grep(x = trimws(rln), pattern = "^#")] # Remove comments
  rln <- rln[trimws(rln) != ""] # Remove those that are blank spaces
  return(length(rln))
}

# Set directory to emr processing subscripts directory
files <- list.files(str_c(emr_dir, "subscripts"), pattern = ".R")

# Set an empty data frame ------------------------------------------------------
count <-  NULL

# Loop through EMR subscripts and count the number of lines
for (i in 1:length(files)) {
  count[i] <- foo(str_c(emr_dir, "subscripts/", files[i]))
}

# Display the lines of uncommented code
message(str_c(sum(count), " lines of uncommented code."))

# Display the number of scripts required to process the emr data
message(length(count), " scripts to process EMR data.")