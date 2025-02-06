
## ****************************************************************************
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# Prepare a data set for PATHWEIGH Aim 2 - Mediation/Moderation of provider
# characteristics/risk factors
#
# *****************************************************************************


# Load libraries --------------------------------------------------------------
library(magrittr, include = "%<>%")
pacman::p_load(tidyverse,
               gtsummary,
               gt)

# LOAD DATA SETS --------------------------------------------------------------
# Load the pp_data - Contains all visits with a recorded weight that are after
# the index date in each phase from patients with 2 or more visits. However,
# pp_data were constructed for Aim 1 and will need modification for Aim 2. This
# will become the primary data set structured for statistical analyses.
load("D:/PATHWEIGH/delivery_20240917/data/pp_data_20240917.RData")

# Load all_visits - Contains the 3,972,656 visits displayed in the Aim 1A
# consort diagram
load("D:/PATHWEIGH/delivery_20240917/data/all_visits_20240917.RData")

# Load all NPIs from the visits in pp_data. These are the same NPIs delivered
# to M. Gritz for retrieving provider characteristics from national database
provider_npis <-
  read_csv("D:/PATHWEIGH/delivery_20240917/data/aim1a_provider_npis.csv",
           show_col_types = FALSE)

# Load ee_ene data since pp_data do not have any of the labs, procedures,
# referrals, comorbidites, or EOSS values for aim 2 moderators/mediators
load("D:/PATHWEIGH/delivery_20240917/data/ee_ene_20240917.RData")


# CLINIC CHARACTERISTICS ------------------------------------------------------
## Patient volume per year ----
# Create a year variable from the encounter date to group by clinic and then
# count the number of encounters
visits %<>%
  mutate(Year = lubridate::year(EncounterDate))

# Group by clinic and year to count the number of encounters in each year of
# study
vol_per_year <- visits %>%
  group_by(DepartmentExternalName, Year) %>%
  summarise(Pt_vol_per_year = n(), .groups = "drop")

## Proportion of medicaid in each phase ----
# Group by clinic and intervention phase to count the number encounters
# in each phase with the goal of determining the denominator for calculating
# the proportion of medicaid visits in each phase for each clinic
vol_per_phase <- visits %>%
  group_by(DepartmentExternalName, Intervention.factor) %>%
  summarise(Pt_vol_per_phase = n(), .groups = "drop")

# Filter for medicaid encounters, then group by clinic and phase to count the
# number of medicaid encounter with the goal of determining the numerator
# for calculating the proportion of medicaid encounters for each clinic in
# each phase
medicaid_per_phase <- visits %>%
  filter(Insurance == "Medicaid") %>%
  group_by(DepartmentExternalName, Intervention.factor) %>%
  summarise(Medicaid_per_phase = n(), .groups = "drop")

# Calculate the proportion of medicaid encounters for each clinic in each
# phase
prop_medicaid <-
  left_join(vol_per_phase,
            medicaid_per_phase,
            by = c("DepartmentExternalName", "Intervention.factor")) %>%
  mutate(percent_medicaid = Medicaid_per_phase / Pt_vol_per_phase * 100) %>%
  mutate(Intervention = Intervention.factor) %>%
  select(-Intervention.factor)

# Merge the patient volume per year and proportion of medicaid into pp_data
pp_data %<>%
  mutate(Year = lubridate::year(EncounterDate))

pp_data %<>%
  left_join(., vol_per_year, by = c("DepartmentExternalName", "Year"))

pp_data %<>%
  left_join(., prop_medicaid, by = c("DepartmentExternalName", "Intervention"))

## Clinic engagement ----
# Clinic engagement is a count/sum of the number of pathweigh resources that
# each clinic used when they transitioned into the intervention phase.
clinic_engagement <-
  read_csv("D:/PATHWEIGH/working_files/clinic_engagement.csv",
           col_types = cols())

# Clean up step to ensure any rows with empty DeptNameEpicIds are omitted
# because there are empty rows in the imported .csv file
clinic_engagement %<>%
  drop_na(DeptNameEpicId)

# Drop all columns except for DepartmentEpicId and Engagemet
clinic_engagement %<>%
  select(DepartmentEpicId, Engagement)

# Merge back into the pp_data set
pp_data %<>%
  left_join(., clinic_engagement, by = c("DepartmentEpicId"))


# PROVIDER CHARACTERISTICS ----------------------------------------------------
## Clinical FTE (cFTE) ----
# Clinical FTE will be based on counting the number of encounters per provider
# in each month of study and then multiplying that number by an adjustment
# value depending on whether or not that provider is classified as IM or FM.

# The number of distinct clinics each provider practiced in using all visits
n_clinics_per_npi <-
  visits %>%
  filter(ProviderNpi %in% provider_npis$ProviderNpi) %>%
  group_by(ProviderNpi) %>%
  summarise(n_clinics_per_npi = n_distinct(DepartmentEpicId))

# How many providers practice at more than one clinic through out the study
# 85 providers
n_clinics_per_npi %>%
  filter(n_clinics_per_npi > 1) %>%
  nrow()

# Create clinic classificaton file
# Create a file to determine classification of clinics into FM or IM. Some
# clinics have straightforward IM or FM in the name. For those that do not,
# a classification was needed for downstream processing. This step is to
# create a file that can be used to fill in the ambiguous clinic names e.g.
# clinic names that have "primary care" cannot be readilyt determine if they
# are FM or IM. After creation, file was modified manually and then re-read
# with values
# visits %>%
#   select(DepartmentExternalName) %>%
#   distinct() %>%
#   write_csv(., "clinic_classification.csv")

# Read in the modified clinic classification file
# File underwent manual manipulation after reviewing clinics. Clinics that
# treated children were classified as FM, whereas clinics that were adults only
# and had IM in dept. external name were classified as IM
clinic_class <-
  read_csv("D:/PATHWEIGH/working_files/clinic_classification.csv",
           show_col_types = FALSE)

# # Create a variable that designates the clinic as a specific class
# visits %<>%
#   mutate(FM = ifelse(DepartmentExternalName %in% (clinic_class %>% filter(FM == 1) %>% pull(DepartmentExternalName)), 1, 0))

# Filter visits to only NPIs of interest
# subset visits to only those that are from the providers who treated the
# patients in aim 1a to begin counting clinical FTE
visits_full <- visits
visits %<>%
  filter(ProviderNpi %in% provider_npis$ProviderNpi)

# For providers who only practice at one location
# Filter n_clinics_per_npi to only those who practice at one location through
# out the study
clinic_npis_eq_1 <- n_clinics_per_npi %>%
  filter(n_clinics_per_npi == 1)

# Filter visits to those who practice at one location and slice a row with the
# department external name to assign to that provider
part1 <-
  visits %>%
  filter(ProviderNpi %in% clinic_npis_eq_1$ProviderNpi) %>%
  select(ProviderNpi, DepartmentExternalName) %>%
  group_by(ProviderNpi) %>%
  slice_head() %>%
  ungroup()

# Merge provider clinic_class to the DepartmentExternalName for each provider
part1 <- left_join(part1, clinic_class, by = "DepartmentExternalName")

# For providers who practice a multiple clnics
clinic_npis_gt_1 <- n_clinics_per_npi %>%
  filter(n_clinics_per_npi > 1)

# Assign each provider to the the clinic with the highest
# number of visits
clinic_counts <-
  visits %>%
  filter(ProviderNpi %in% clinic_npis_gt_1$ProviderNpi) %>%
  group_by(ProviderNpi, DepartmentExternalName) %>%
  count() %>%
  ungroup()

# Are there any ties? If there are ties, the number of visits will be the same
# in the top two clinics leading to one distinct value. If the number of visits
# in any top two clinics are the same then there's a tie for clnic with the
# highest number of visits.
ties <-
  clinic_counts %>%
  arrange(ProviderNpi, desc(n)) %>%
  group_by(ProviderNpi) %>%
  slice_head(n = 2) %>%
  summarise(n_distinct = n_distinct(n)) %>%
  filter(n_distinct == 1)

if ((ties %>% nrow()) != 0) {
  stop("Throw an error because there are ties that need to be resolved.")
} else {

  part2 <-
    clinic_counts %>%
    arrange(ProviderNpi, desc(n)) %>%
    group_by(ProviderNpi) %>%
    slice_head() %>%
    ungroup() %>%
    select(-n)

}

# Merge  clinic_class to each unique provider that practiced at multiple
# clinics
part2 <- left_join(part2, clinic_class, by = "DepartmentExternalName")

# Classify each provider as IM OR FM
provider_npi_class <- bind_rows(part1, part2) %>%
  mutate(main_provider_clinic = DepartmentExternalName) %>%
  select(-DepartmentExternalName)

# Merge in the provider classifier (FM)
visits %<>%
  left_join(., provider_npi_class, by = "ProviderNpi")

# Strip the day (dd) from the EncounterDate to count the number of encounters
# per month. This operation takes a really long time, possible place to use
# parallel processing
visits %<>%
  mutate(year_mon =  format(as.Date(EncounterDate), "%Y-%m"))

visits_full %<>%
  mutate(year_mon = format(as.Date(EncounterDate), "%Y-%m"))

# There should not be any values with an NA in the FM column
visits %>%
  filter(is.na(FM))

# Count the number of visits per provider
# For each NPI multiple rows one for each month in each year with the number of
# visits in that monthe
n_visits_per_npi <-
  visits %>%
  group_by(ProviderNpi, year_mon, FM) %>%
  count() %>%
  ungroup()

n_visits_per_npi %>%
  filter(year_mon >= "2023-03") %>%
  group_by(ProviderNpi) %>%
  summarise(avg_visits_per_mo = mean(n)) %>%
  ggplot(., aes(x = avg_visits_per_mo)) +
  geom_histogram()

# Check the distribution of providers not in Aim1 to see if they
# are different
n_visits_per_npi_notin <-
  visits_full %>%
  drop_na(ProviderNpi) %>%
  filter(!ProviderNpi %in% provider_npis$ProviderNpi) %>%
  group_by(ProviderNpi, year_mon) %>%
  count() %>%
  ungroup()

n_visits_per_npi_notin %>%
  group_by(ProviderNpi) %>%
  summarise(avg_visits_per_mo = mean(n)) %>%
  ggplot(., aes(x = avg_visits_per_mo)) +
  geom_histogram()

n_distinct(n_visits_per_npi$ProviderNpi)


# Estimate the proportion of FTE in each month
# Estimate the monthly FTE
# For those in FM adjust number of encounters in FM to 1.1 to account for
# encounters that are not in our data set, but were due to seeing patients
# less than 18 years old (since our data set should only be 18+).

# For FM, take total encounters (yes encounters, not unique patients) per
# NPI#/month x 1.10 (to account for 10% volume being patients <18 years)
# x 1.309 (for BMI<25) = total encounters per month.

# For IM, take total encounters x 1 (to account for 0% volume being patients
# BMI < 25 years) x 1.307 (for BMI<25) = total encounters per month.  Total
# encounters per month / 240 (16 patients per day x 4 days/week x 45 weeks per
# year/ 12 mo/year which is 100% clinical FTE) = estimated clinical FTE per
# provider.
n_visits_per_npi %<>%
  mutate(n_adj_encounters = ifelse(FM == 1,  n * 1.1, n * 1)) %>%
  mutate(est_FTE = n_adj_encounters / 240)

# Are there any zero monthly FTE's
n_visits_per_npi %>%
  filter(est_FTE == 0)

min(n_visits_per_npi$est_FTE)

n_visits_per_npi %>%
  arrange(est_FTE)

# Averate %FTE in all months
provider_fte <-
  n_visits_per_npi %>%
  group_by(ProviderNpi) %>%
  summarise(avg_fte = mean(est_FTE))

# Distribution of FTE using this approach
provider_fte %>%
  ggplot(., aes(x = avg_fte)) +
  geom_histogram()

provider_fte %>%
  filter(is.na(avg_fte))

# Cut the ftes in to groups and make a table
file_out <- "D:\\PATHWEIGH\\delivery_20240917\\scripts\\aim2\\fte_grouped.pdf"

# provider_fte %>%
#   mutate(fte = cut(avg_fte, breaks = seq(0, 1.6, .1))) %>%
#   select(fte) %>%
#   tbl_summary() %>%
#   as_gt() %>%
#   gtsave(filename = file_out)

provider_fte %>%
  mutate(fte = cut(avg_fte, breaks = seq(0, 1.6, .1))) %>%
  select(fte) %>%
  ggplot(., aes(x = fte)) +
  geom_bar()

# pp_data %<>%
#   left_join(., provider_fte, by = "ProviderNpi")

## Provider characteristics from NPPES data ----
nppes <-
  haven::read_sas(
    "D:\\PATHWEIGH\\delivery_20240917\\scripts\\aim2\\nppes_data.sas7bdat")

nppes %<>%
  mutate(ProviderNpi = as.numeric(NPI)) %>%
  select(-NPI)

# Check the distribution of the FM providers only
n_visits_per_npi %>%
  filter(ProviderNpi %in% (nppes %>%
                             filter(FMphys == 1) %>%
                             pull(ProviderNpi))
  ) %>%
  group_by(ProviderNpi) %>%
  summarise(avg_visits_per_mo = mean(n)) %>%
  ggplot(., aes(x = avg_visits_per_mo)) +
  geom_histogram()


# Get the provider NPIs for the encounter Ids in pp_data to merge in the
encounter_ids_w_npi <- visits %>%
  filter(Arb_EncounterId %in% pp_data$Arb_EncounterId) %>%
  select(Arb_EncounterId, ProviderNpi)

# Join data to encounter_ids_w_npi
provider_characteristics <-
  left_join(encounter_ids_w_npi, nppes, by  = "ProviderNpi")

# Calculate years in practice
provider_characteristics %<>%
  mutate(Years_in_prac = 2024 - YearReg)

provider_characteristics %>%
  group_by(ProviderNpi) %>%
  slice_head() %>%
  ungroup() %>%
  ggplot(., aes(x = Years_in_prac)) +
  geom_histogram()

provider_characteristics %>%
  group_by(ProviderNpi) %>%
  slice_head() %>%
  ungroup() %>%
  pull(Years_in_prac) %>%
  table(., useNA = "ifany")


# Categorize Years in practice
provider_characteristics %<>%
  mutate(Years_in_prac_cat = cut(Years_in_prac, breaks = seq(0, 20, 4)))

# Merge provider characteristics into pp_data
pp_data %<>%
  left_join(., provider_characteristics, by = c("Arb_EncounterId"))

# Merge cfte now that each row has the ProviderNPI in pp_data
pp_data %<>%
  left_join(., provider_fte, by = "ProviderNpi")

# Check the distribution of years since registering NPI for those with cFTE
# below 20% to see if those are all in fact residents. If they are all
# residents we would expect to see mostly those within 0-4 years since
# registering for their NPI.
# Left join years in prac and n_visits_per_npi
n_visits_per_npi %<>%
  left_join(.,
            (pp_data %>%
               group_by(ProviderNpi) %>%
               slice_head() %>%
               select(ProviderNpi, Years_in_prac_cat)),
            by = "ProviderNpi")

# This histogram will show panel 0 those with fte > 20% and panel 1, those with
# fte <= 20%. Those with fte <= 20% does have a higher representation of
# providers in the 0-4 years of practice range, but it still includes a portion
# of provider that have between 4-20 years of experience. Omitting them from
# analyses will remove them as well. In other words, removing anyone with cFTE
# under .20 will also remove more experienced providers.
n_visits_per_npi %>%
  group_by(ProviderNpi, Years_in_prac_cat) %>%
  summarise(avg_fte = mean(est_FTE)) %>%
  ungroup() %>%
  mutate(`under_.20_fte` = ifelse(avg_fte <= .2, 1, 0)) %>%
  ggplot(., aes(x = Years_in_prac_cat)) +
  geom_bar() +
  facet_grid(~`under_.20_fte`)


# PATIENT CHARACTERISTICS -----------------------------------------------------
# Subset ee_ene to only the records found in pp_data
ee_ene %<>%
  filter(Arb_EncounterId %in% pp_data$Arb_EncounterId) %>%
  mutate(Arb_PersonId = factor(Arb_PersonId))

## Create Former smoker ----
# Smoking status is already in pp_data, but not time invariant, this chunk
# makes it time invariant
pp_data %<>%
  mutate(Former_Smoker =
           ifelse(Smoking_Status == "Former" & IndexVisit == 1, 1, NA)) %>%
  group_by(Arb_PersonId, Intervention) %>%
  fill(Former_Smoker, .direction = c("downup")) %>%
  ungroup() %>%
  mutate(Former_Smoker = ifelse(is.na(Former_Smoker), 0, Former_Smoker))

## Insurance Type ----
# Insurance type is already in pp_data, but not time invariant, this chunk
# makes it time invariant
pp_data %<>%
  mutate(Insurance_Type = ifelse(IndexVisit == 1, Insurance, NA)) %>%
  group_by(Arb_PersonId, Intervention) %>%
  fill(Insurance_Type, .direction = c("downup")) %>%
  ungroup()

## BMI ----
# BMI is already in pp_data, but not time invariant
pp_data %<>%
  mutate(BMI_bl = ifelse(IndexVisit == 1, BMI, NA)) %>%
  group_by(Arb_PersonId, Intervention) %>%
  fill(BMI_bl, .direction = c("downup")) %>%
  ungroup()

## PHQ9/GAD7 ----
# PHQ9 and GAD7 are not in pp_data. Use ee_ene to capture index values
# PHQ9 and GAD7 are often times not captured, result in an NA
phq <- ee_ene %>%
  filter(IndexVisit == 1) %>%
  select(Arb_PersonId, Arb_EncounterId, PHQ9, GAD7) %>%
  mutate(Arb_PersonId = factor(Arb_PersonId))

# Merge in the phq values
pp_data %<>%
  left_join(., phq, by = c("Arb_PersonId", "Arb_EncounterId"))

# Fille the NAs
pp_data %<>%
  group_by(Arb_PersonId, Intervention) %>%
  fill(PHQ9, .direction = c("downup")) %>%
  fill(GAD7, .direction = c("downup")) %>%
  ungroup()


## Calculate any referral ----
# Patient level, apply to each encounter within each phase. Calculate wether or
# not the patient had a referral.
# For each index visit (one for each intervention), sum the number of refferals
# A referral is counted if found in any visit within the intervention phase
refs <-
  ee_ene %>%
  # Subset to the same records in pp_data, to capture all available variables
  # at the index date.
  filter(IndexVisit == 1) %>%
  # Sum of the Ref_* variables row wise to create an intermediary variable
  select(Arb_PersonId, Arb_EncounterId, starts_with("Ref_")) %>%
  rowwise(Arb_EncounterId) %>%
  mutate(Ref_sum = sum(c_across(starts_with("Ref_")))) %>%
  ungroup() %>%
  # Create Any_ref if the intermediary variables is > 0
  mutate(Any_ref = ifelse(Ref_sum > 0, 1, 0)) %>%
  # mutate(Arb_PersonId = factor(Arb_PersonId)) %>%
  select(Arb_PersonId, Arb_EncounterId, Any_ref)

# Merge refs back into pp_data
pp_data %<>%
  left_join(., refs, by = c("Arb_PersonId", "Arb_EncounterId"))

# Seems to suggest that there are Refs linked to non-index visits
# There are the LastVisits and LastVisits_Weight. Both should coincide due to
# making the last visit with weight as the last visit to compare any change
# and to capture labs, procs, refs, meds, etc.
# ee_ene %>%
#   filter(Arb_PersonId %in% pp_data$Arb_PersonId, IndexVisit == 0) %>%
#   filter(Ref_BariatricSurgery == 1) %>%
#   select(starts_with("Ref_")) %>%
#   summarise_all(., sum, na.rm = TRUE)

# Fill in the NAs for each patient within each intervention phase so that
# any_ref is time invariant within the phase.
pp_data %<>%
  group_by(Arb_PersonId, Intervention) %>%
  fill(Any_ref, .direction = c("downup")) %>%
  ungroup()

## Bariatric procedure ----
bariatric <-
  ee_ene %>%
  filter(IndexVisit == 1) %>%
  select(Arb_PersonId, Arb_EncounterId, BariatricSurgery)

# Merge in the bariatric surgery values
pp_data %<>%
  left_join(., bariatric, by = c("Arb_PersonId", "Arb_EncounterId"))

# Fill the NAs
pp_data %<>%
  group_by(Arb_PersonId, Intervention) %>%
  fill(BariatricSurgery, .direction = c("downup")) %>%
  ungroup()


## EOSS ----
eoss <- ee_ene %>%
  filter(IndexVisit == 1) %>%
  select(Arb_PersonId, Arb_EncounterId, EOSS)

# Merge in the eoss values
pp_data %<>%
  left_join(., eoss, by = c("Arb_PersonId", "Arb_EncounterId"))

# Fill the NAs
pp_data %<>%
  group_by(Arb_PersonId, Intervention) %>%
  fill(EOSS, .direction = c("downup")) %>%
  ungroup()

## Medications ----
meds <- ee_ene %>%
  filter(IndexVisit == 1) %>%
  select(Arb_PersonId, Arb_EncounterId, N_Meds_AOM, N_Meds_Gain, N_Meds_Loss)

# Convert the count variables to binary variables
meds %<>%
  mutate(across(N_Meds_AOM:N_Meds_Loss, ~ ifelse(.x > 0, 1, 0)))

# Rename the column names
colnames(meds) <-
  c("Arb_PersonId", "Arb_EncounterId", "Meds_AOM", "Meds_Gain", "Meds_Loss")

# Merge in the meds
pp_data %<>%
  left_join(., meds, by = c("Arb_PersonId", "Arb_EncounterId"))

# Fill in the NAs
pp_data %<>%
  group_by(Arb_PersonId, Intervention) %>%
  fill(Meds_AOM, .direction = c("downup")) %>%
  fill(Meds_Gain, .direction = c("downup")) %>%
  fill(Meds_Loss, .direction = c("downup")) %>%
  ungroup()


# OUTPUT THE ANALYSIS FILE TO .CSV --------------------------------------------
write.csv(pp_data,
          file = "D:/PATHWEIGH/delivery_20240917/data/aim2_data_20240917.csv",
          row.names = FALSE)