# Double Check some of the Consort Diagram figures -----------------------------
# One question will be to know whether or not we want to keep only those with a
# valid npi

# Total number of patient encounters within the period: 745074
# Do not use encounter_sub because it contains duplicates
# visits is created from visits and should serve as a check that they are same
nrow(visits)
nrow(visits)

# Total number of patient encounters within the period from providers with NPI
# 654821
nrow(visits %>% drop_na(ProviderNpi))

# Total number of unique patients seen by a provider with a valid NPI: 270605
visits %>%
  drop_na(ProviderNpi) %>%
  select(Arb_PersonId) %>%
  n_distinct()

# Total number of unique patients that are eligible and were seen with a provider
# with a valid NPI
# *** The eligible flag does not equal those with Age and BMI_use
# : 164708
visits %>%
  drop_na(ProviderNpi) %>%
  #filter(Eligible == 1) %>%
  filter(Age >= 18, BMI_use >= 25) %>%
  select(Arb_PersonId) %>%
  n_distinct()

# Total number of unique eligible patients with WPV
# *** will need to find out what the importance of the index date is
visits %>%
  drop_na(ProviderNpi) %>%
  #filter(Eligible == 1) %>%
  filter(Age >= 18, BMI_use >= 25, WPV >=1, EncounterDate >= IndexDate) %>%
  select(Arb_PersonId) %>%
  n_distinct()

# Total number of eligible patients with WPV in each cohort seen by providers
# with valid NPI
# 1 Cohort 1            7001
# 2 Cohort 2            6082
# 3 Cohort 3            7300
visits %>%
  drop_na(ProviderNpi) %>%
  #filter(Eligible == 1) %>%
  filter(Age >= 18, BMI_use >= 25, WPV >=1, EncounterDate >= IndexDate) %>%
  group_by(Cohort) %>%
  summarise(unique_patients = n_distinct(Arb_PersonId))