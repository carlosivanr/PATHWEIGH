# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# EE-ENE Investigation

# In the delivery from 09-17-2024, the numbers of eligible and enrolled patients
# is lower than that of the previous delivery from 03-26-2024.

# The following code is designed to see why some individuals are missing?
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

# ee_investigation -------------------------------------------------------------------
# Try to figure out why the new data delivery has a reduced number of patients at the
# ee_ene stage. 
# Turns out the issue is with the new code to prevent patients with an index date after
# 03-17-24 from getting included into the study

# Load data
ee_ene_0326 <- load_rdata("D:/PATHWEIGH/delivery_20240326/data/ee_ene_20240326.RData")

ee_ene_0917 <- load_rdata("D:/PATHWEIGH/delivery_20240917/data/ee_ene_20240917.RData")

# Load the dataframes into a list
df_list <- list(ee_ene_0326, ee_ene_0917)

# What is the number of unique patients for each delivery in the control phase
# The number of unique enrolled patients differs by 1,857
unique_pt_ids <- df_list %>%
  purrr::map(
    ~ .x %>%
      filter(Enrolled == 1,
             Intervention.factor == "Control") %>%
      select(Arb_PersonId) %>%
      distinct()
  )

# Get the list of patients that are in 0326 but not 0917
# Yields 1,871 ids. 
# these should be checked in the data processing stream
# see where they get dropped
overlap_ids <- unique_pt_ids[[1]] %>%
  filter(!Arb_PersonId %in% unique_pt_ids[[2]]$Arb_PersonId)

# Yields 14 patients that are in 0917 but not 0326
new_ids <- unique_pt_ids[[2]] %>%
  filter(!Arb_PersonId %in% unique_pt_ids[[1]]$Arb_PersonId)

# Checking each section of the processing stream ---------------------------
# At this point, open process_data.R and go through the processing stream
# line by line stopping to check for the number of patients that are in the
# list of the overlap_ids to see where patients drop out.


# Check after loading the encounter table
# In the new data delivery, there are 1,856 patients out of the 1,857 of the
# overlapping group, which means most patients are still in the data stream
# upon loading the encounter table, and only one has been lost
encounter %>%
  filter(Arb_PersonId %in% overlap_ids$Arb_PersonId) %>%
  distinct(Arb_PersonId) %>%
  nrow()


# Check after prepping encounter
# 1,856, no change detected
encounter %>%
  filter(Arb_PersonId %in% overlap_ids$Arb_PersonId) %>%
  distinct(Arb_PersonId) %>%
  nrow()


# Check after creating visits
# 1,856, no change detected
visits %>%
  filter(Arb_PersonId %in% overlap_ids$Arb_PersonId) %>%
  distinct(Arb_PersonId) %>%
  nrow()


# Check after setting WPV_* columns and setting the index dates
# 1,856 no change detected
visits %>%
  filter(Arb_PersonId %in% overlap_ids$Arb_PersonId) %>%
  distinct(Arb_PersonId) %>%
  nrow()


# Check after creating ee_ene
# 1,851 a loss of 5 detected, because at this point only visits that occur on or after the Index date are retained
ee_ene %>%
  filter(Arb_PersonId %in% overlap_ids$Arb_PersonId) %>%
  distinct(Arb_PersonId) %>%
  nrow()


# Check after prepping ee_ene for processing labs procedures and comorbidities
# 1,851 no change detected
 ee_ene %>%
  filter(Arb_PersonId %in% overlap_ids$Arb_PersonId) %>%
  distinct(Arb_PersonId) %>%
  nrow()


# Check after processing labs procedures and comorbidities
# 1,851 no change detected
ee_ene %>%
  filter(Arb_PersonId %in% overlap_ids$Arb_PersonId) %>%
  distinct(Arb_PersonId) %>%
  nrow()


# Check after the index date cut off
# 8, massive change detected
# 1,843 were dropped because they had index visits after 03-17-2024
ee_ene %>%
  filter(Arb_PersonId %in% overlap_ids$Arb_PersonId) %>%
  distinct(Arb_PersonId) %>%
  nrow()

# Cutoff date accounts for the majority of differences between
# the patients in the ee_ene data from 03-26 to 09-17. Perhaps
# not worth looking into the handful here in there.


# meds investigation --------------------------------------------------------------
# Look at why the meds tables for the pp_mod data people have a different number of 
# anti obesity medications

# load the meds 03-26-2024 table
data_delivery_date <- 20240326
proj_root_dir <- str_c("delivery_", data_delivery_date)
proj_parent_dir <- str_remove(here(), proj_root_dir)
source("D:/PATHWEIGH/emr_data_processing/functions/read_pw_csv.R")
read_pw_csv('meds')
meds_0326 <- meds

# load the meds 09-17-2024 table
data_delivery_date <- 20240917
proj_root_dir <- str_c("delivery_", data_delivery_date)
proj_parent_dir <- str_remove(here(), proj_root_dir)
source("D:/PATHWEIGH/emr_data_processing/functions/read_pw_csv.R")
read_pw_csv('meds')
meds_0917 <- meds

# Remove the un-necessary data frame
rm(meds)

# Load the pp_mod data for the same delivery
pp_data_0326 <- load_rdata("D:/PATHWEIGH/delivery_20240326/data/pp_data_20240326.RData")

# Ensure that the sample size matches the output from tables and figs
# 7,502
pp_data_0326 %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

ids_0326 <- 
  pp_data_0326 %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  pull(Arb_PersonId)

# Load the pp_mod data for the same delivery
pp_data_0917 <- load_rdata("D:/PATHWEIGH/delivery_20240917/data/pp_data_20240917.RData")

# Ensure that the sample size matches the output from tables and figs
# 8,858
pp_data_0917 %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

ids_0917 <- 
  pp_data_0917 %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  pull(Arb_PersonId)


# Filter each respective table by the 
# Process the meds data frames ----------------------------------------
# Create a list of the meds data frames with the patients of interest only
meds_list <- list((meds_0326 %>% filter(Arb_PersonId %in% ids_0326)), 
                  (meds_0917 %>% filter(Arb_PersonId %in% ids_0917)))

# Process meds list to be able to filter out the AOMs
meds_list <- meds_list %>%
  purrr::map(
    ~ .x %>%
      filter(ActiveMed == "Y") %>%
      mutate(across(MedicationName:GenericName, tolower))
  )



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
  left_join(., meds_bin_vars, by = "uniqueMedName")

aom_meds <- meds_gen_names %>%
  filter(AOM == 1) %>%
  select(GenericName)

# The number of patients from each delivery with a documented
# active prescription for an anti obesity medication ever
# 03-26-2024: 665
# 09-17-2024: 709
meds_list %>%
  purrr::map(
    ~ .x %>%
      filter(GenericName %in% aom_meds$GenericName) %>%
      select(Arb_PersonId) %>%
      distinct() %>%
      nrow()
  )

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Count the AOMs in just the control phase

# Check to make sure no one has more than one cohort
pp_data_0917 %>%
  group_by(Arb_PersonId) %>%
  summarise(n = n_distinct(Cohort)) %>%
  filter(n > 1)

# Assign a cohort_end_date to be able to filter the ordered date
pp_data_0917 %<>%
  mutate(crossover_date = ifelse(Cohort == "Cohort1", "2022-03-16", NA),
         crossover_date = ifelse(Cohort == "Cohort2", "2023-03-16", crossover_date),
         crossover_date = ifelse(Cohort == "Cohort3", "2024-03-16", crossover_date))

# Merge the crossover dates into the meds tables
meds_list[[2]] %>%
  mutate(Arb_PersonId = factor(Arb_PersonId)) %>%
  left_join(., 
            pp_data_0917 %>% group_by(Arb_PersonId) %>% slice_head() %>% select(Arb_PersonId, crossover_date),
            by = "Arb_PersonId") %>%
  filter(OrderedDate <= crossover_date) %>%
  filter(GenericName %in% aom_meds$GenericName)

# The index visits for the pp_data patients in the control phase
enc_ids_0917 <- pp_data_0917 %>%
  filter(IndexVisit == 1,
         Intervention == "Control") %>%
  pull(Arb_EncounterId)

visits_post_id_0917 <- load_rdata("D:/PATHWEIGH/delivery_20240917/data/processed_visits_post_id_20240917.RData")


visits_post_id_0917 %>%
  filter(Arb_EncounterId %in% enc_ids_0917) %>%
  select(N_Meds_AOM) %>%
  summarise(n = sum(N_Meds_AOM))


visits_post_id_0917 %>%
  filter(Arb_EncounterId %in% enc_ids_0917) %>%
  filter(N_Meds_AOM > 0)