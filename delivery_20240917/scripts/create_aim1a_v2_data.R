# *****************************************************************************
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# Create interrupted time series data set for Mark
# 12-02-2024

# Capture the visits from ee and ene patients after eligibility or enrollment
# criteria are met, dropping all visits without a recorded weight value.
# Filters for patients that have a subsequent weight value. However it
# does not consider wether there are subsequent weight values in each phase.

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


# Tabulate EE WPV -------------------------------------------------------------
load("D:/PATHWEIGH/delivery_20240917/data/ee_ene_20240917.RData")

ee <- ee_ene %>% filter(EE == 1)

# Drop visits with missing weight
ee %<>%
  drop_na(Weight_kgs)

# Drop censored visits (control after intervention)
ee %<>% 
  filter(Censored == 0)


# keep patients that have two or more visits in a phase
ids_to_keep <- ee %>%
  group_by(Arb_PersonId, Intervention.factor) %>%
  summarise(n_visits= n()) %>%
  filter(n_visits >= 2) %>%
  pull(Arb_PersonId)


# Remove the ids with 
ee %<>%
  filter(Arb_PersonId %in% ids_to_keep)


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