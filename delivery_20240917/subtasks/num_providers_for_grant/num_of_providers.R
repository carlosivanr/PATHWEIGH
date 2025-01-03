# *****************************************************************************
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# 01-02-2025
# Count the number of providers per clinic
# Count the number of eligible patients per clinic
# Must be executed with proces_data.R up because it relies on ee_ene_consort
# data frame.
#
# *****************************************************************************

pacman::p_load(tidyverse, install = FALSE)
# library(magrittr, include.only = "%<>%")

# Starts with the total # of eligible patients
# load("D:/PATHWEIGH/delivery_20240917/data/ee_ene_20240917.RData")

# Verify that the total number of patients is 385,090
# ee_ene gives 353,468
# ee_ene_consort gives 385,090 which is reported in the consort diagram
ee_ene_consort %>%
  select(Arb_PersonId) %>%
  distinct() %>%
  nrow()

# The averate number of eligible patients per practice
353468/56 # using ee_ene after date cut off

385090/56 # using ee_ene_consort before date cut off

# Number of unique providers with valid NPI
ee_ene_consort %>%
  drop_na(ProviderNpi) %>%
  select(ProviderNpi) %>%
  n_distinct()

# Average number of providers per clinic that treated the eligible patients
1085/56

# Average number of eligible patients treated per provider 
6876.607 / 19.375

# Number of unique Department Epic Ids
ee_ene_consort %>%
  select(DepartmentEpicId) %>%
  n_distinct()

# Number of unique eligible patients that have a visit at 18 months
ee_ene_consort %>%
  filter(N_months_post_id >= 18) %>%
  select(Arb_PersonId) %>%
  n_distinct()

# Number of eligible patients per clinic with a visit at 18 months
# 435.71
24400/56

# Number of eligible patients per clinic with a visits at 18 months or more
105391/56
# *****************************************************************************