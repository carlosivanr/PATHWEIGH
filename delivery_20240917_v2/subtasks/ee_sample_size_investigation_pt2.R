# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# EE Investigation PART 2 - AFTER MODIFYING CUTOFF DATE ALGORITHM

# Background:
# From data delivery 03-26-2024 to 09-17-2024 the number of patients in the
# Eligible and Enrolled drops from 22,419 to 21,109 in the control period 
# respectively. This represented a loss of 1,310 patients. The following code is
# investigate what can be gathered about these patients 

# It is unlikely that the drop is due to modifications of the pipeline. The
# first section shows how the number of unique patient ids, changes across these
# data frames with the different pipelines. Using the modified pipeline and the
# original pipeline in 03-26-2024 yields the same number of patients.

# In the process, it was discovered the the table and figures .qmd file had an
# error for capturing the intervention phase ee patients. Originally found
# 36,901 patients, but it should have been 45,759

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tidyverse)


# Load the data from 2023-10-10
load("D:/PATHWEIGH/delivery_20231010/data/ee_ene_20231010.RData")

# rename the data
ee_1010 <- ee_ene

# Load the original version of the 2024-03-26 data
load("D:/PATHWEIGH/delivery_20240326/data/backup_20240326/ee_ene_20240326.RData")

# rename the data
ee_0326_a <- ee_ene

# Load the second version, with the updated pipeline
load("D:/PATHWEIGH/delivery_20240326/data/ee_ene_20240326.RData")

# rename the data
ee_0326_b <- ee_ene


# Load the data from 2024-09-17
load("D:/PATHWEIGH/delivery_20240917/data/ee_ene_20240917.RData")

ee_0917 <-  ee_ene

rm(ee_ene)


# join all of the individual data frames into a list to undergo common processing
data_list <- list(ee_1010, ee_0326_a, ee_0326_b, ee_0917)

rm(ee_1010, ee_0326_a, ee_0326_b, ee_0917)

# Section 1 --------------------------------------------------------------------
# Count the number of patients in the EE data frames from:
# 1. The 10-10-2023 pipeline
# 2. The 03-26-2024 pipeline version A
# 3. The 03-26-2024 pipeline version B, that has additional modifications
# 4. The 09-17-2024 pipeline same as version B for 03-26-2024

# Filter the list of data frames to rows where sex is known, censored == 0, and
# the months after of index visits is less than 18
data_list <- data_list %>%
  map(.,
      ~.x %>%
        filter(Sex != "Unknown",
               Censored == 0,
               N_months_post_id <= 18,
               Enrolled == 1) %>%
        drop_na(Weight_kgs)
  )

# For each data frame, we want to get the patients that have 2 or more visits 
# in a phase and are enrolled
func <- function(df) {
  c("Control", "Intervention") %>%
    map(.,
        ~ df %>%
          filter(Intervention.factor == .x) %>%
          group_by(Arb_PersonId) %>%
          count() %>%
          filter(n > 1) %>%
          pull(Arb_PersonId)
    )
}

# Gives the number of patients in each phase for each data delivery for the
# eligible and enrolled data frames
ee_ids_list <- data_list %>%
  map(.,
      ~ func(.x))


# Section 2 --------------------------------------------------------------------
# Compare data_list[[3]] 03-26-2024 to data_list[[4]] 09-17-2024

# After modifying the cut off date algorithm, there are 19 patients that are in
# the 03-26 data that are not in the 09-17 data
sum(!ee_ids_list[[3]][[1]] %in% ee_ids_list[[4]][[1]])

# There are 9 patients in the 09-17 data that are not in the 03-26 data
# - suggests 9 new patients were added
sum(!ee_ids_list[[4]][[1]] %in% ee_ids_list[[3]][[1]])

# From 03-26 to 09-17, there was a loss of 19 patients, but at the same time
# there was a gain of 9 patients, which results in a net loss of 10 patients.


# Get the ids_to_check, by indexing the ee_ids_list from 03-26 not in 09-17
ids_to_check <- ee_ids_list[[3]][[1]][!ee_ids_list[[3]][[1]] %in% ee_ids_list[[4]][[1]]]

ids_lost <- ee_ids_list[[3]][[1]][!ee_ids_list[[3]][[1]] %in% ee_ids_list[[4]][[1]]]

ids_gained <- ee_ids_list[[4]][[1]][!ee_ids_list[[4]][[1]] %in% ee_ids_list[[3]][[1]]]

# Run the process_data.R from the 09-17 delivery to see where these patients get
# dropped, if at all

## A.
# Check to see if the lost patients are in the encounter table from compass
# Those patients are in the compass table
# - 12 of the 19 lost patients are still in the deliver. Suggests we lost rows
#   from 7 patients just from the delivery
encounter %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

# The 7 ids lost from the delivery are
kept_from_0326 <- encounter %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  pull(Arb_PersonId)

lost_from_0326 <- ids_to_check[!ids_to_check %in% kept_from_0326]

# verify that the 7 are not in the encounter table
encounter %>%
  filter(Arb_PersonId %in% lost_from_0326) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

ids_to_check <- kept_from_0326

## B.
# Check to see if the 12 lost patients are found after creating visits data frame
visits %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

## C.
# Check to see if the patients are found after creating the WPV variables
visits %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

## D.
# Check to see if the patients are found after setting index dates
visits %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

# There are 1,310 patients that are in visits at this stage of processing
visits %>%
  filter(Intervention.factor == "Control") %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()


## E.
## Check to see if the patients are found in the ee data frame
# Results in a drop of 6 patients
ee %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

# Get the 6 that are lost when creating EE
lost_at_ee <- 
ids_to_check[!ids_to_check %in% (ee %>%
                      filter(Arb_PersonId %in% ids_to_check) %>%
                      select(Arb_PersonId) %>%
                      distinct() %>%
                      pull(Arb_PersonId))]

# Verify the lost_at_ee are not in ee
ee %>%
  filter(Arb_PersonId %in% lost_at_ee) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

# Look at the data for the 6 lost at ee
View(visits %>%
  filter(Arb_PersonId %in% lost_at_ee) %>%
    select(Arb_PersonId, EncounterDate, IndexDate, WPV, Eligible, Enrolled, BMI)
)

# CONCLUSION: for the 6 patients lost after the creation of EE -----------------
# The six lost after the creation of ee data frame seem to have either a missing
# weight value or a missing height value. The missing weight value prevents the
# visit from getting assigned as an Index visit with a wpv that qualified before
# The lack of height prevents the calculation of BMI, which then affects
# eligibility


# Check if there's a drop in patients
# of the 6 that were not lost after ee, three were dropped in control, but all 
# Six remain in intervention phase
remain_at_ee <- ids_to_check[!ids_to_check %in% lost_at_ee]

ee %>%
  filter(Intervention.factor == "Control") %>%
  filter(Arb_PersonId %in% remain_at_ee) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

ee %>%
  filter(Intervention.factor == "Intervention") %>%
  filter(Arb_PersonId %in% remain_at_ee) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

# Remained after creating ee but lost in control phase
remain_at_ee_con <- ee %>%
  filter(Intervention.factor == "Control") %>%
  filter(Arb_PersonId %in% remain_at_ee) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  pull(Arb_PersonId)

lost_at_ee_con <-  remain_at_ee[!remain_at_ee %in% remain_at_ee_con ]

View(ee %>%
  filter(Arb_PersonId %in% lost_at_ee_con) %>%
    arrange(Arb_PersonId, EncounterDate))

ee %>%
  filter(Intervention.factor == "Control") %>%
  filter(Arb_PersonId %in% lost_at_ee_con) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

View(visits %>%
  filter(Arb_PersonId %in% lost_at_ee_con))

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#### STOPPED HERE 11/23/2024. Not worth looking into the remaining 6 patients
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## F. Check if the same individuals can still be found in ee_ene
ee_ene %>%
  filter(Intervention.factor == "Control") %>%
  filter(Enrolled == 1) %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

## G. Check if the same individuals can still be found after prep ee_ene
ee_ene %>%
  filter(Intervention.factor == "Control") %>%
  filter(Enrolled == 1) %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

## H. Check if the same individuals can still be found after proc ee_ene
ee_ene %>%
  filter(Intervention.factor == "Control") %>%
  filter(Enrolled == 1) %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

# ******************************************************************************
## G. Check if the same individuals can still be found after filtering for the
# cut off date
# The drop in patients occurs when index dates are filtered for index dates that
# are before 03-16-204
ee_ene %>%
  filter(Intervention.factor == "Control") %>%
  filter(Enrolled == 1) %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()
# ******************************************************************************

# Check the index dates in control
ee_ene %>%
  filter(Intervention.factor == "Control") %>%
  filter(Enrolled == 1) %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  filter(IndexVisit == 1) %>%
  select(IndexDate, EncounterDate) %>%
  filter(EncounterDate >= "2024-03-17")

# Check the index dates in intervention
# Results in a loss of 1,298 patients
# Means that in the intervention, they have index dates after 2024-03-18
# The original algorithm was written in a way that that if any patient had an
# index date after the cutoff date, they patient id was placed in a vector and
# then the vector was used to filter out patients, which inadvertently filtered
# out their visits in control, due to Index visits in intervention.
ee_ene %>%
  filter(Intervention.factor == "Intervention") %>%
  filter(Enrolled == 1) %>%
  filter(Arb_PersonId %in% ids_to_check) %>%
  filter(IndexVisit == 1) %>%
  select(IndexDate, EncounterDate) %>%
  filter(EncounterDate >= "2024-03-17")