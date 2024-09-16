# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. Dept. of Fam. Med. CU Anschutz Medical Campus
# 08/17/2023
# Get labs, meds, and create EOSS
#
# Description:
# The purpose of this script is break apart the visits_post_id data frame and
# then process the visits data to utilize index date at control and intervention
# and last visit (excluding index visits) at control and intervention separately
# to capture labs, meds and create the EOSS. Needs four different data frames
# with non-overlapping visits to work properly.
#
# Dependencies:
# labs_procedures.R
# medications.R
# eoss.r
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load functions ---------------------------------------------------------------
source(str_c(emr_dir, "subscripts/labs_procedures.R"))
source(str_c(emr_dir, "subscripts/medications.R"))
source(str_c(emr_dir, "subscripts/eoss.R"))

# Create data subsets ----------------------------------------------------------
# Make a copy of the output when processed in serial
# visits_post_id_backup <- visits_post_id 
# Revert visits_post_id back to its form before processing labs, meds, eoss

## 1. index visits in control phase ----
# Can be processed normally without modifying the index date
ind_con <- bind_rows(
  (visits_post_id %>%
  filter(Censored == 0,
         IndexVisit == 1,
         Intervention == 0,
         LastVisit == 1)),
  (visits_post_id %>%
  filter(Censored == 0,
         IndexVisit == 1,
         Intervention == 0,
         LastVisit == 0))
)

## 2. last visits in control phase ----
# Needs a temporary modification of the index date to trick the algorithm into
# using the encounter date instead of the index date
lv_con <- visits_post_id %>%
  filter(Censored == 0,
         IndexVisit == 0,
         Intervention == 0,
         LastVisit == 1) %>%
  mutate(IndexDate_backup = IndexDate,
         IndexDate = EncounterDate,
         IndexVisit = 1)

## 3. index visits in intervention phase ----
# Can be processed normally without modifying the index date
ind_int <- bind_rows(
  (visits_post_id %>%
  filter(Censored == 0,
         IndexVisit == 1,
         Intervention == 1,
         LastVisit == 1)),
    (visits_post_id %>%
       filter(Censored == 0,
              IndexVisit == 1,
              Intervention == 1,
              LastVisit == 0)))

## 4. last visits in intervention phase ----
# Needs a temporary modification of the index date to trick the algorithm into
# using the encounter date instead of the index date
lv_int <- visits_post_id %>%
  filter(Censored == 0,
         IndexVisit == 0,
         Intervention == 1,
         LastVisit == 1) %>%
  mutate(IndexDate_backup = IndexDate,
         IndexDate = EncounterDate,
         IndexVisit = 1) 

# Put dfs into a list and then process -----------------------------------------
df_list <- list(ind_con, lv_con, ind_int, lv_int)

# Set a function with the labs, meds, and eoss functions inside for use with the
# lapply() function
proc_data <- function(temp){
  temp <- labs_procedures(temp)
  temp <- capture_medications(temp)
  temp <- eoss(temp)
  return(temp)
}

# Process data in Serial -------------------------------------------------------
# parallel with furrr package was much slower due to copying the global
# environment variables for each session
# Tried p_unload(furrr)
# df_list <- map(df_list, proc_data) #purrr stopped working for some reason
df_list <- lapply(df_list, proc_data)


# Clean up data before stitching------------------------------------------------
# Since the labs_procedures(), capture_medications(), and eoss() functions
# require an index date, data subsets that do not have an index visit are set
# to 1 and the index date is set to the encounter date to "trick" the function
# into working correctly. This chunk of code reverts the IndexVisit and
# IndexDate columns back to their original state. Only needed for lv_con and
# lv_int subsets.
clean_data <- function(temp) {
  if (sum(grepl("IndexDate_backup", names(temp))) == 1) {
    temp %<>%
      mutate(IndexDate = IndexDate_backup,
             IndexVisit = 0) %>%
      select(-IndexDate_backup)
  }
  return(temp)
}

# Apply the clean_data function to the list of data frames
df_list <- map(df_list, clean_data)

# Recreate visits_post_id ------------------------------------------------------
# Capture all of the encounter ids used to link labs, procedures, and meds
linked_visit_ids <-
  bind_rows(df_list) %>%
  pull(Arb_EncounterId)

# Filter out visits that were processed from visits_post_id
non_linked_visits <-
  visits_post_id %>%
  filter(!Arb_EncounterId %in% linked_visit_ids)

# Bind the subsets of index and last visits that were processed
linked_visits <- bind_rows(df_list)

# Check to ensure that no new rows were added before replacing visits_post_id
# If no new rows, then bind the subsets together and save as a
# new visits_post_id data frame
if (bind_rows(linked_visits, non_linked_visits) %>% nrow() == dim(visits_post_id)[1]) {
  visits_post_id <- bind_rows(linked_visits, non_linked_visits)
} else {
  stop("The number of modified output visits does not equal the number of input visits!!! Review and revise code.")
}

# Clear out any temporary data frames and memory resources
rm(linked_visits, non_linked_visits, linked_visit_ids,
   lv_int, lv_con, ind_int, ind_con, df_list)

invisible(gc())