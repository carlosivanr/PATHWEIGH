# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PHD
# 11-14-2024
# Medications investigation 2
# 1. Investigate the new vs old medications table for the 20240917 data
# delivery to ensure the new table is giving the appropriate results
# Conclusion: Whenever compass delivers a table, it isn't necessarily locked to
# a specific date. If, for example a data delivery is pulled on 9-17, it will
# contain x amount of records. If we ask for an update on say 11-15, then it
# will contain additional records. The additional records aren't always due to
# dates either. The new meds table had additional records compared to the old
# original table, but filtering by date doesn't necessarily get you back to 
# the original. I.E the data is incomplete, or dynamically changing.

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


data_delivery_date <- 20240917
proj_root_dir <- str_c("delivery_", data_delivery_date)
proj_parent_dir <- str_remove(here(), proj_root_dir)

# load the compass meds 09-17-2024 table without the start and end dates
meds_old <-
  data.table::fread(
                    file = str_c(proj_parent_dir,
                                 "/data_raw/",
                                 data_delivery_date,
                                 "/", "C2976_Table11_Med_20240917_2024-09-17.csv"),
                    header = TRUE,
                    integer64 = "numeric",
                    na.strings = c("", "NA", -999, "*Restricted")) %>%
                    # na.strings = c("", "NA", -999)) %>%
  mutate_if(lubridate::is.Date, as.Date) %>%
  as_tibble()

# load the compass meds 09-17-2024 table with start and end dates
# * n.b. the new data is actually a different delivery date and contains
# OrderedDates up to 11-06-2024, whereas the older table
# contains OrderedDates up to 2024-09-13
meds <-
  data.table::fread(
                    file = str_c(proj_parent_dir,
                                 "/data_raw/",
                                 data_delivery_date,
                                 "/", "C2976_Table11_Med_20240917.csv"),
                    header = TRUE,
                    integer64 = "numeric",
                    na.strings = c("", "NA", -999, "*Restricted")) %>%
                    # na.strings = c("", "NA", -999)) %>%
  mutate_if(lubridate::is.Date, as.Date) %>%
  as_tibble() %>%
  filter(OrderedDate <= "2024-09-17")

# Filter the meds table to only those medications for the sample of interest
# and that are anti obesity
meds <-
meds %>% 
    filter(
      Arb_PersonId %in% pp_data_0917$Arb_PersonId,
      MedicationName %in% aom_lookup$MedicationName)

# Merge in the keyword/generic name
# Some MedicationNames like orlistat (XENICAL), have multiple MedicationEpicIds
# and have multiple keywords, but have a common generic name
meds <-
  meds %>%
    left_join(., 
      (aom_lookup %>% 
        select(MedicationName, MEDICATIONEPICID, GENERICNAME, keyword) %>% 
        group_by(MedicationName) %>% 
        slice_head() %>% 
        ungroup()),
      by = "MedicationName")


# Convert Arb_PersonId into factor for merging in the cohort for each patient
meds <-
  meds %>%
    mutate(Arb_PersonId = as.factor(Arb_PersonId))

# Merge in the cohort for each patient, only need to get the index visits from
# one phase either control or intervention for this step
meds <-
  meds %>%
      left_join(., 
        (pp_data_0917 %>%
          filter(IndexVisit == 1,
                  Intervention == "Control",
                ) %>%
          select(Arb_PersonId, Cohort)),
        by = "Arb_PersonId")

# Assign a crossover date
meds <-
  meds %>%
    mutate(crossover_date = case_when(
    Cohort == "Cohort1" ~ "2021-03-17",
    Cohort == "Cohort2" ~ "2022-03-17",
    .default = "2023-03-17"))

# If the OrderedDate is before crossover, then the medication is in the control 
# phase, else the medication belongs in the intervention phase
meds <-
  meds %>%
    mutate(Control = ifelse(OrderedDate < crossover_date, 1, 0))

meds <-
  meds %>%
    mutate(Intervention = ifelse(Control == 1, 0, 1))

# To capture the medications where the medication is active in both the control
# and intervention phases, set Intervention to 1 where the crossover date is 
# between the ordered date and the end date
meds <-
  meds %>%
    mutate(Intervention = ifelse(crossover_date >= OrderedDate & crossover_date <= EndDate,
                                1, Intervention))

# The number by which the intervention group should increase by
# 607 patients
meds %>%
  filter(Control == 1, 
         Intervention == 1)

# Convert to long to get counts
meds <- bind_rows(
  meds %>% 
  filter(Control == 1) %>%
  mutate(Intervention = "Control") %>%
  select(-Control, -Intervention) %>%
  mutate(Intervention = "Control"),
  
  meds %>% 
  filter(Intervention == 1) %>%
  mutate(Intervention = "Intervention") %>%
  select(-Control, -Intervention) %>%
  mutate(Intervention = "Intervention"))

# Get a count of the number of AOMs per patient,
count_aoms <-
  meds %>%
    group_by(Arb_PersonId, Intervention) %>%
    count() %>%
    ungroup() %>%
    rename(N_AOM = n) %>%
    mutate(IndexVisit = 1)

# Get the index visits and merge in the counts of AOMs, then
# create the AOM variable
pp_index_visits <-
    pp_data_0917 %>%
      filter(IndexVisit == 1) %>%
      left_join(., count_aoms, by = c("Arb_PersonId", "IndexVisit", "Intervention")) %>%
      mutate(AOM = ifelse(N_AOM > 0, 1, 0)) %>%
      mutate(AOM = ifelse(is.na(AOM), 0, AOM))

# Display the table
pp_index_visits %>%
  select(AOM, Intervention) %>%
  tbl_summary(by = "Intervention")