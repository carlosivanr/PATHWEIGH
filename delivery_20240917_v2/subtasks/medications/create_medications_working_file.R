# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# 11-08-2024
# Create medications working file
#
# This script creates the aol_lookup.csv working file.
# On 11-05-2024 COMPASS delivered a medication look up table that lists all of
# the therapeutic classes as defined by EPIC. For the anti-obesity medications
# the therapeutic class of anti-obesity drugs is used per Leigh Perrault email
# communication on 11-12-2024. The look up table avoids having to use the 
# original algorithm based on string searching for certain medication names
# when processing the COMPASS data.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


invisible(library(tidyverse))
library(gtsummary)
library(magrittr, include.only = "%<>%")

meds_lookup <-read_csv("D:/PATHWEIGH/data_raw/medication_lookup_table_with_epicids_20241105.csv")

aoms <- meds_lookup %>%
  filter(THERAPEUTICCLASS == "ANTI-OBESITY DRUGS") %>%
  distinct()

write_csv(aoms, "D:/PATHWEIGH/working_files/medications/aoms_lookup.csv")
