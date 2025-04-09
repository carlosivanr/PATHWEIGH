# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PHD
# 11-08-2024
# Medications investigation
# 1. Investigate why the number of AOMs in the 2024-09-17 data delivery
# decreases in comparison to the number of AOMs in the 2024-03-26 delivery.
#    Conclusion: The algorithm has been set to filter the Meds delivery table
#    for records that where Active == "Y". The Active variable is set on the 
#    compass end where Active is set to "Y" for records where the Medication
#    Start Date is less than the data pull date and where the Medication End
#    Date is after the pull date or if it's null. Otherwise its set to NO
#    It essentially indicates if the medication is active at the time of the
#    data pull only. CR 11-11-2024

# 2. Investigate why the old and the new algorithm differ in the counts of the
#    AOMs. 
#    Conclusion: The two algorithms will catch a different set of medications
#    because the lookup table from Compass is the most strict so it will pick
#    up less medications than the original lookup table originally drafted
#    before I got on the project.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
invisible(library(tidyverse))
library(magrittr, include.only = "%<>%")
library(gtsummary)
library(here)
library(openxlsx)

# File loading function that will allow loading a file and assign it to a
# different name to in the global environment
load_rdata <- function(file_name) {
  #loads an RData file, and returns it
  load(file_name)
  get(ls()[ls() != "file_name"])
}

# meds investigation ----------------------------------------------------------

# Load the AOM lookup table ---------------------------------------------------
# Contains a list of all the medications where Therapeutic class == 
# "anti obesity drugs"
aom_lookup <- read_csv(here("working_files/medications/aoms_lookup.csv"))

# Check the 09-17-2024 data with the 2 different meds tables ------------------
# Load the pp_mod data for the same delivery
pp_data_0917 <- 
  load_rdata("D:/PATHWEIGH/delivery_20240917/data/pp_data_20240917.RData")


# %% -------------------------------------------------------------------------- 
# Look at why the meds tables for the pp_mod data people have a different
# number of anti obesity medications

# load the compass meds 03-26-2024 table
# n.b. the delivery date, proj_root_dir, proj_parent_dir, need to be set for
# the read_pw_csv() to work properly.
data_delivery_date <- 20240326
proj_root_dir <- str_c("delivery_", data_delivery_date)
proj_parent_dir <- str_remove(here(), proj_root_dir)
source("D:/PATHWEIGH/emr_data_processing/functions/read_pw_csv.R")
read_pw_csv('meds')
meds_0326 <- meds

# load the compass meds 09-17-2024 table
data_delivery_date <- 20240917
proj_root_dir <- str_c("delivery_", data_delivery_date)
proj_parent_dir <- str_remove(here(), proj_root_dir)
source("D:/PATHWEIGH/emr_data_processing/functions/read_pw_csv.R")
read_pw_csv('meds')
meds_0917 <- meds

# Place both meds tables in a list so that they can be processed
# with purrr and map()
meds <- list(meds_0917, meds_0326)


# meds <- meds_0917

# Check that the filtering for Arb_PersonId works
meds[[2]] %>%
  filter(Arb_PersonId %in% pp_data_0917$Arb_PersonId) %>%
  nrow()

# Get the unique AOMs for each patient in pp_data_0917 to compare
# the active medications in the two compass data delivery tables
# At this point the 0917 [[1]] data frame has less active medications than the
# the 0326 [[2]] data frame.
meds <- map(meds,
  ~ .x %>% 
    filter(
      # ActiveMed == "Y",
      Arb_PersonId %in% pp_data_0917$Arb_PersonId,
      MedicationName %in% aom_lookup$MedicationName) %>%
    distinct()
)



# Merge in the keyword/generic name
# Some MedicationNames like orlistat (XENICAL), have multiple MedicationEpicIds
# and have multiple keywords, but have a common generic name
meds <-
map(meds,
  ~ .x %>%
    left_join(., 
      (aom_lookup %>% 
        select(MedicationName, MEDICATIONEPICID, GENERICNAME, keyword) %>% 
        group_by(MedicationName) %>% 
        slice_head() %>% 
        ungroup()),
      by = "MedicationName")
    )



# Merge in the cohort into meds -----------------------------------------------
# Convert factor to numeric otherwise left_joins won't work
meds <-
map(meds,
  ~ .x %>%
    mutate(Arb_PersonId = as.factor(Arb_PersonId))
    )

meds <-
map(meds,
  ~ .x %>%
    left_join(., 
      (pp_data_0917 %>%
        filter(IndexVisit == 1,
               Intervention == "Control",
              ) %>%
        select(Arb_PersonId, Cohort)),
      by = "Arb_PersonId")
    )

# Assign a crossover date
meds <-
map(meds,
  ~ .x %>%
    mutate(crossover_date = case_when(
    Cohort == "Cohort1" ~ "2021-03-17",
    Cohort == "Cohort2" ~ "2022-03-17",
    .default = "2023-03-17"))
    )


# If the OrderedDate is before crossover, then the medication is in the control 
# phase, else the medication belongs in the intervention phase
meds <-
map(meds,
  ~ .x %>%
    mutate(Intervention = ifelse(OrderedDate < crossover_date, "Control", "Intervention"))
    )

# Get a count of the number of AOMs per patient,
count_aoms <-
map(meds,
  ~ .x %>%
    group_by(Arb_PersonId, Intervention) %>%
    count() %>%
    ungroup() %>%
    rename(N_AOM = n) %>%
    mutate(IndexVisit = 1)
)



# Merge counts into pp_data and display a table after converting counts to binary
pp_index_visits <-
map(count_aoms,
~ pp_data_0917 %>%
  filter(IndexVisit == 1) %>%
  left_join(., .x, by = c("Arb_PersonId", "IndexVisit", "Intervention")) %>%
  mutate(AOM = ifelse(N_AOM > 0, 1, 0)) %>%
  mutate(AOM = ifelse(is.na(AOM), 0, AOM))
)

# Something broke here between 11/14/2024 and when this chunk was originally
# drafted
# c("Control", "Intervention") %>%
#   map(
#     ~ pp_index_visits %>%
#       select(N_AOM) %>%
#       tbl_summary(by = "Intervention")
#     )

# The number of patients with at least one AOM  in the period using
# the new data delivery 0917
pp_index_visits[[1]] %>%
  select(AOM, Intervention) %>%
  tbl_summary(by = "Intervention")

# The number of patients with at least one AOM  in the period using
# the prior data delivery 0326
pp_index_visits[[2]] %>%
  select(AOM, Intervention) %>%
  tbl_summary(by = "Intervention")


# Which patients in the prior delivery had AOM, but did not show up
# in the new data delivery
ids_to_check <- pp_index_visits[[2]] %>%
  filter(Intervention == "Control",
         N_AOM > 0) %>%
  pull(Arb_PersonId)

df_0917 <- meds_0917 %>%
  filter(Arb_PersonId %in% ids_to_check)

df_0326 <- meds_0326 %>%
  filter(Arb_PersonId %in% ids_to_check)

df_0917 %>%
  filter(Arb_EncounterId %in% df_0326$Arb_EncounterId) %>%
  pull(ActiveMed) %>%
  table()

df_0326 %>%
  pull(ActiveMed) %>%
  table()

# Active Status Changes, which is why we have a decline in the number of active
# medications. For the same encounter id in different data deliveries, the 
# status can go from active to inactive, suggesting there is a medication end
# date that changed the counts.

# %% --------------------------------------------------------------------------
# Compare the string algorithm vs the look up table algorithm -----------------
# count_aoms[[1]] represents the counts of aoms using the new algorithm based
# on the rapeutic class
aom_alg1_tx_class <- count_aoms[[1]]

# Create a data frame with 4 columns, using the older string-based algorithm
# 1. Arb_PersonId
# 2. Intervention
# 3. N_AOM
# 4. IndexVisit

# Set the crossover date when the clinics cross from control to intervention
pp_data_0917 %<>%
    mutate(crossover_date = case_when(
      Cohort == "Cohort1" ~ "2021-03-17",
      Cohort == "Cohort2" ~ "2022-03-17",
      .default = "2023-03-17"))

# Load and process working files ---------------------------------------------
## Load the uniqueMedName sheet as meds_bin_vars ----
# Contains the unique medication names along with a set of binary variables
# to indicate whether a unique medication name is either anti obesity
# medication (aom), causes weight gain, or causes weight loss
meds_bin_vars <-
  read.xlsx(
            here("working_files/medications/medications_20221017.xlsx"),
            sheet = "UniqueMedName")

## Load the Medications sheet as meds_gn_names ----
meds_gen_names <-
  read.xlsx(
            here("working_files/medications/medications_20221017.xlsx"),
            sheet = "Medications")

## Process meds_bin_vars ----
# corresponds to uniqueMedname sheet in medications.xlsx
# converts all Xs to 1 and all NAs to 0
meds_bin_vars %<>%
  mutate(across(AOM:weight.loss, ~ ifelse(is.na(.x), 0, 1))) %>%
  select(-Generic.name, -trade.names) %>%
  distinct()

## Process meds list ----
# corresponds to Medications sheet in medications.xlsx
meds_gen_names %<>%
  mutate(across("GenericName", tolower)) %>% 
  select(uniqueMedName, GenericName) %>%
  drop_na() %>%
  distinct()

# Merge med_gen_names and meds_bin_vars
meds_gen_names %<>%
  left_join(., meds_bin_vars, by = "uniqueMedName") %>%
  as_tibble()

# Subset meds table ----
# Subset the meds table and convert medication and generic names to lower
# case to match meds_gen_names
meds_sub <- meds_0917 %>%
  filter(ActiveMed == "Y",
          Arb_PersonId %in% pp_data_0917$Arb_PersonId) %>%
  mutate(across(MedicationName:GenericName, tolower)) %>%
  distinct()


# Meds data control
meds_data_control <-
  meds_sub %>%
  mutate(Arb_PersonId = factor(Arb_PersonId)) %>%
  left_join(., meds_gen_names, by = "GenericName", relationship = "many-to-many") %>%
  left_join(., (pp_data_0917 %>% select(Arb_PersonId, crossover_date) %>% group_by(Arb_PersonId) %>% slice_head() %>% ungroup()), by = "Arb_PersonId") %>%
  filter(OrderedDate < crossover_date)

n_meds <- meds_data_control %>%
  group_by(Arb_PersonId, uniqueMedName) %>%
  slice_head() %>%
  ungroup() %>%
  group_by(Arb_PersonId) %>%
  summarise(N_Meds_Gain = sum(weight.gain, na.rm = TRUE),
            N_Meds_Loss = sum(weight.loss, na.rm = TRUE),
            N_Meds_AOM = sum(AOM, na.rm = TRUE))

pp_data_0917 %<>%
  left_join(., n_meds, by = "Arb_PersonId")

# Fix the NAs by converting to 0
pp_data_0917 %<>%
  mutate(across(all_of(c("N_Meds_Gain", "N_Meds_Loss", "N_Meds_AOM")), 
                ~ ifelse(is.na(.), 0, .)))


# Create a table of the counts of patients with an active AOM in control phase
# At this point, in control the new algorithm results in 0 out of 8,858 
# patients with active AOM medications, but the older algorithm results in 3
# patients with active AOM medications. Suggests something is different.
pp_data_0917 %>%
  filter(IndexVisit == 1,
         Intervention == "Control") %>%
  mutate(N_Meds_AOM = ifelse(N_Meds_AOM > 0, 1, 0)) %>%
  select(N_Meds_AOM) %>%
  tbl_summary(label = list(N_Meds_AOM ~ "AOM"))


# Find out the Arb_PersonId of the patients that have at least one AOM in the
# control phase using the older algorithm.
old_alg_ids <- pp_data_0917 %>%
  filter(IndexVisit == 1,
         Intervention == "Control",
         N_Meds_AOM > 0) %>%
  pull(Arb_PersonId)


# Results in 0 rows
count_aoms[[1]] %>% filter(Arb_PersonId %in% old_alg_ids)

# Results in 0 rows
meds[[1]] %>% filter(Arb_PersonId %in% old_alg_ids)

# Results in 41 rows
meds_0917 %>% 
  filter(Arb_PersonId %in% old_alg_ids) %>%
  filter(GenericName %in% aom_lookup$GENERICNAME)

# For the 3 patients that have AOMs with the old algorithm
# the medication names are naltrexone and lisdexamfetamine
meds_data_control %>%
  filter(Arb_PersonId %in% old_alg_ids,
         AOM == 1) %>%
  select(Arb_PersonId, GenericName, MedicationName, uniqueMedName)

# The AOM lookup does not classify lisdexamfetamine as an AOM
# Therefore there is a loss of values for that medication

# The other medication is naltrexone
# check Arb_PersonId 5539609339   
View(meds_0917 %>%
  filter(Arb_PersonId == 5539609339))

View(aom_lookup %>% filter(grepl("naltrexone", GENERICNAME)))
# Then pull all of their active medications from meds_sub and see which ones they
# are taking

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Draft of new AOM algorithm with start and end dates

# Load the meds table
# Filter the meds table to the patients of interest
# Filter the meds table to the AOMs of interest
# - Leigh 11-12-2024 wants ordered medications using the new lookup table
# For each patient, merge in their assigned cohort
# Then create the crossover date based on cohort
# For control, Active could be defined as having an ordered date before the 
# crossover date
# For intervention, Active could be defined as having an ordered date after
# the crossover date, OR if the Crossover date is greater than the crossover
# date, but less than the end date. This would capture people who started
# an AOM in control and had it continue into the intervention period.
