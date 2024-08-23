###### Parts for making EE and ENE #############################################
# FROM the ee vs ene R project.

# Visits  ----------------------------------------------------------------------
# Visits is created to perform some additional processing and exclusion of rows
# All visits with provider NPI as NA are dropped, including visits from the 11
# patients that were not captured in the baseline characteristics paper.
visits <- seq_df2

# - BMI_use to BMI - Set names BMI_use was old, BMI is new
# - Enrolled == Eligible (age and bmi criteria) and WPV > 0
# - Drop Npis - Drop rows if NPI is na
# - This will take the 745,074 visits down to 654,781
# - n.b. the Enrolled variable works on the visit level, not at the patient 
#   level. There are some patients who are Eligible and Enrolled in the study
#   but also have non-enrolled visits, perhaps due to lack of WPV
visits %<>%
  drop_na(ProviderNpi) %>%
  select(-BMI, -BMI_comp) %>%
  rename(BMI = BMI_use) %>%
  mutate(Enrolled = ifelse(WPV > 0 & Eligible == 1, 1, 0),
         Enrolled = ifelse(is.na(ProviderNpi), 0, Enrolled))

# Create eligible and enrolled -------------------------------------------------
# Clean up the 11 patients who were not captured in the bl char paper
# Unique person_ids for all that were supposed to be in baseline
all_unique_ee_ids <- 
  visits %>% 
  filter(Enrolled == 1) %>%
  distinct(Arb_PersonId)

# EE defined as meeting eligibility criteria and at least one WPV
# Enrolled == 1 is not enough to satisfy to the patient level because of 
# multiple visits per patients, but EncounterDate == IndexDate does the job bc
# only those that have a WPV are assigned an IndexDate.
ee <- visits %>%
  filter(EncounterDate == IndexDate)


# What are the IDs that are in visits, but not all_unique_ee_ids
# all_unique_ee_ids represents the 20,394 patients that are eligible and 
# enrolled, but the baseline characteristics paper reports on 20,383. The ee 
# dataframe contains all of the 20,383. The 20,411 did not filter for provider
# NPIs with NAs.
ids_to_exclude <- all_unique_ee_ids %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId)

# Exclude the visits of the 11 patients that were excluded from baseline code 
# because NPI NA was dropped after, instead of before, the assignment of an indexDate. i.e.
# some patients were assigned an index date even though it was decided to 
# exclude them.
visits %<>%
  filter(!Arb_PersonId %in% ids_to_exclude$Arb_PersonId)

# Added 12/20/2022, but not tested, to make Enrolled a dichotomous variables
# visits %>%
#   mutate(Enrolled = ifelse(Arb_PersonId %in% ee$Arb_PersonId, 1, 0))

# Check ee --------------------------------------------------------------------- 
# Do all visits in ee have matching encounter and index dates?
nrow(visits %>% filter(EncounterDate == IndexDate)) == dim(ee)[1]

# Are all visits in ee enrolled?
nrow(ee %>% filter(Enrolled == 1)) == dim(ee)[1]

# Is the number of unique enrolled patients equal to the number of patients in ee
# Should match if the 11 patients with provider NPI as na are excluded
visits %>% filter(Enrolled == 1) %>% pull(Arb_PersonId) %>% n_distinct() == dim(ee)[1]

# Check that there are no patients in ee that are coded in intervention
nrow(ee %>% filter(intervention == 1)) == 0

# Check that only 3 Cohorts are available 
length(table(ee$Cohort)) == 3


# Create eligible but not enrolled ---------------------------------------------
# For EE, the cohort is assigned based on their index WPV. But since ENE do not
# have a WPV, then the following represents the approach towards assigning a
# cohort variable. ENE defined as meeting eligibility criteria but no WPV. For 
# ene patients who were seen at multiple clinics, cohort is assigned as the 
# clinic with the most visits, if tied select the first observation (Group). For
# all others, cohort is assigned according to the 

# How many unique ene patients have more than one visit?
nrow(visits %>%
       filter(!Arb_PersonId %in% ee$Arb_PersonId) %>%
       group_by(Arb_PersonId) %>%
       count() %>%
       filter(n > 1))

# How many unique ene patients have more than one clinic?  
nrow(visits %>%
       filter(!Arb_PersonId %in% ee$Arb_PersonId) %>%
       group_by(Arb_PersonId) %>%
       summarise(clinics = n_distinct(GroupID)) %>%
       filter(clinics > 1))
#_______________________________________________________________________________
# Capture the Arb_PersonIds of ENE patients who had visits at clinics assigned 
# to more than one cohort.
gt1_clinic_ids <- visits %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  summarise(clinics = n_distinct(GroupID)) %>%
  filter(clinics > 1)

# Get the number of visits for each patient that is ene in each cohort
n_visits_per_patient_clinic <- visits %>%
  filter(Enrolled == 0,
         Eligible == 1) %>%
  group_by(Arb_PersonId, GroupID) %>%
  count() %>%
  filter(Arb_PersonId %in% gt1_clinic_ids$Arb_PersonId)


# For ene patients who were seen at multiple clinics, the cohort to be assigned
# is the cohort with the most visits. If tied, then select the 1st obs after
# grouping by patient id.
ene_gt1_clinic_cohorts <- 
  n_visits_per_patient_clinic %>% 
  group_by(Arb_PersonId) %>%
  arrange(n) %>% 
  slice_head() %>%
  mutate(Cohort = str_c("Cohort ", GroupID)) %>%
  select(Arb_PersonId, Cohort)

# Assign a cohort to those with out visits at multiple clinics which is the first
# Cohort after arranging visits by date as was done for EE.
# Coalesce takes the first nonmissing element, but should assign Cohort.y which
# is set to the cohort where most visits occurred. Since this slices by person
# id, it should only be one visit per person
ene_gt1 <- visits %>%
  filter(Eligible == 1, 
         Arb_PersonId %in% ene_gt1_clinic_cohorts$Arb_PersonId) %>%
  left_join(ene_gt1_clinic_cohorts, by = "Arb_PersonId") %>%
  mutate(Cohort = coalesce(Cohort.y, Cohort.x)) %>%
  select(-Cohort.y, -Cohort.x) %>%
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  select(Arb_PersonId:WPV_v2, Cohort, everything())

# ______________________________________________________________________________
# For ene patients that did not go to more than one clinic, we take the first 
# visit in the time period to capture outcome variables. Since there is only
# one clinic that they visit, their cohort stays constant.
ene <- 
  visits %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId,
         !Arb_PersonId %in% ene_gt1$Arb_PersonId,
         Eligible == 1,
         Enrolled == 0) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  slice_head() %>%
  mutate(Cohort = str_c("Cohort ", GroupID))

# Merge ene which initially contains only the first visit of ENE patients with
# ene_gt1_clinic_cohorts which contains the same variables but Cohort has been
# assigned to the clinic in which the most frequent visits take place for those
# with visits across multiple clinics
data <- bind_rows(ee, ene, ene_gt1)
rm(ene_gt1, ene_gt1_clinic_cohorts, gt1_clinic_ids, n_visits_per_patient_clinic, tempdat.id)

