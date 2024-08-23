# Prep Visits  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 08/13/2024

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create the visits data frame by filtering encounters within a designated date
# range and joining encounter, patient, and clinic tables
clinic <- readRDS(here("working_files", "clinic.rds"))
visits <- encounter %>%
  filter(EncounterDate >= date_min, 
         EncounterDate <= date_max) %>%
  left_join(., 
            patient, 
            by = "Arb_PersonId") %>%
  left_join(., 
            select(clinic, "DepartmentEpicId", "GroupID"), 
            by = "DepartmentEpicId")

# Switch EpicId from Old GMC to new Greeley IM ---------------------------------
# DepartmentEpicId 10981033 is the old Greeley Medical Center which moved to its
# new location affiliated with DepartmentEpicId 10981004. From the 2024-03-26
# data delivery forward, it was decided to change the the old GMC Id to the new
# Id. Switch epic ids before setting the GroupID variable
if (lubridate::ymd(data_delivery_date) >= lubridate::ymd("20240326")) {
  visits %<>% 
    mutate(DepartmentEpicId = ifelse(DepartmentEpicId == 10981033, 10981004, DepartmentEpicId),
           GroupID = ifelse(DepartmentExternalName == "UCHealth Internal Medicine Clinic - Greeley", 1, GroupID ))
}


# Set Intervention variable ----------------------------------------------------
# Intervention is based on the date when the clinic crosses over from control to 
# intervention phase. Set intervention to zero if EncounterDate is before the 
# crossover date for the specific GroupID, else set to 1.
visits %<>%
  mutate(Intervention = ifelse(
    (GroupID == 1 & EncounterDate < "2021-03-17") |
      (GroupID == 2 & EncounterDate < "2022-03-17") |
      (GroupID == 3 & EncounterDate < "2023-03-17"), 0, 1))

visits %<>%
  mutate(Intervention.factor = as_factor(
    case_match(Intervention, 0 ~ "Control", 1 ~ "Intervention")))

# Set Eligibility for Index Visits and restrict Weight and Height --------------
# At this stage, the Eligible flag is to identify eligible encounters, not
# necessarily eligible patients
# Age, BMI, Height & Weight inclusion criteria 

# 1. Eligible encounter is defined as meeting Age and BMI criteria only
# Calculate age of patient at visit using EncounterDate and BirthDate
# Visits in which the patient is older than 90 are set to 90 to preserve 
# privacy.
visits %<>% 
  mutate(Age = as.numeric((EncounterDate - BirthDate)/365),
         Age = ifelse(Age > 90, 90, Age))

# Adults (age >=18) with BMI>=25 at index visit
# Some visits will not have Age or BMI and will result in an NA value, so those
# must be turned to 0 in the 2nd mutate statement
visits %<>%
  mutate(IndexVisitEligible = ifelse(Age >= 18 & BMI >=25, 1, 0),
         IndexVisitEligible = ifelse(is.na(IndexVisitEligible), 0, IndexVisitEligible)
         )

# Index Visit Eligible -------------------------------------------------------
# Only visits in which a valid NPI is available are eligible for an index
# visit
visits %<>%
  mutate(IndexVisitEligible = ifelse(is.na(ProviderNpi), 0, IndexVisitEligible),
         IndexVisitEligible = ifelse(is.na(Weight_kgs), 0, IndexVisitEligible))

# Repeat Encounters ------------------------------------------------------------
# Remove any duplicated encounters, but uses an algorithmic approach to 
# determine which of the duplicates to keep. Placed here because it uses the 
# smoking variable as a tie breaker.
visits <- repeat_encounters(visits)

# remove data frames that are no loner needed
rm(encounter, patient, clinic)

invisible(gc())
