# Medications %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# medications() function is designed to capture medications that cause weight
# loss or weight gain, and medications that are approved as anti-obesity
# medications at a given reference date either in control (0) or intervention 
# (1) within the specified date range 

# Algorithm Changes:
# - 07/28/2023 capture only those related to the index visits by EncounterId
# - 08/01/2023 Leigh & Qing, capture within a 2 month window, 30 days previous
# and 30 days after the index/reference date
# - 10/24/2024 Stats group, capture the medications that are found at any time
#   within the intervention phase

# 1. Combine the two sheets in the medications.xlsx file located in the working
# files directory.
#   a. Process meds_bin_vars which needs to be uniqueMedName and three binary
#      variables for AOM, weight.gain and weight.loss
#   b. Process meds_gen_names which needs to be uniqueMedname and GenericName
#       columns
# 2. Process the meds table by filtering to patients in the input data frame
#    and filtering by Active meds set to "Y".
# 3. Create meds_full which combines meds_gn_names, meds_bin_vars, and meds_sub
#   a. Merge meds_gen_names with meds_bin_vars
#   b. merge 3.a with meds_sub
#   *** Meds_full is created in 2 steps, with meds_aom sandwiched in between
#   implementing the two steps in sequence leads to the same results as OG,
#   but then leads to a mismatch in meds_AOM.
#   if the creation of meds_full is split, then the results will be the same
#   as OG

# meds_sub - created by subsetting the meds table for active medications and for
# the patients in the input data frame

# meds_bin_vars is imported from the medications.xlsx in the working files
# directory. meds_bin_vars created as a dataframe that lists the
# uniqueMedName and a set of binary variables to indicate if it's anti-obesity
# (AOM), causes weight gain or weight loss

# meds_gen_names is imported from the medication.xlsx in the working files
# directory

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

capture_medications <- function(temp) {
  ## Filter the input data frame to the index visits in the specified phase ----
  temp %<>% 
    filter(Censored == 0,
            IndexVisit == 1)

  # Check which phase the input data frame is in. If all of the values are 0
  # then the input is in the control phase, otherwise it is in the intervention
  # phase
  if ((names(table(temp$Intervention))) == "0") {
    phase <- "Control"
  } else {
    phase <- "Intervention"
  }

  print(phase)
  
  # Set the crossover date when the clinics cross from control to intervention
  # 
  temp %<>%
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
  meds_sub <- meds %>%
    filter(ActiveMed == "Y",
           Arb_PersonId %in% temp$Arb_PersonId) %>%
    mutate(across(MedicationName:GenericName, tolower)) %>%
    distinct()

  # Create meds_data conditionally on phase ----
  # Primary data frame that will be used to create n_meds and aom_wide
  if (phase == "Control") {
    meds_data <- 
    meds_sub %>%
      left_join(., meds_gen_names, by = "GenericName", relationship = "many-to-many") %>%
      left_join(., (temp %>% select(Arb_PersonId, crossover_date) %>% group_by(Arb_PersonId) %>% slice_head() %>% ungroup()), by = "Arb_PersonId") %>%
      filter(OrderedDate < crossover_date)

  } else {

    meds_data <- 
    meds_sub %>%
      left_join(., meds_gen_names, by = "GenericName", relationship = "many-to-many") %>%
      left_join(., (temp %>% select(Arb_PersonId, crossover_date) %>% group_by(Arb_PersonId) %>% slice_head() %>% ungroup()), by = "Arb_PersonId") %>%
      filter(OrderedDate >= crossover_date)
  }

  n_meds <- meds_data %>%
    group_by(Arb_PersonId, uniqueMedName) %>%
    slice_head() %>%
    ungroup() %>%
    group_by(Arb_PersonId) %>%
    summarise(N_Meds_Gain = sum(weight.gain, na.rm = TRUE),
              N_Meds_Loss = sum(weight.loss, na.rm = TRUE),
              N_Meds_AOM = sum(AOM, na.rm = TRUE))
  
  temp %<>%
    left_join(., n_meds, by = "Arb_PersonId")

  # Fix the NAs by converting to 0
  temp %<>%
    mutate(across(all_of(c("N_Meds_Gain", "N_Meds_Loss", "N_Meds_AOM")), ~ifelse(is.na(.), 0, .)))

  temp %<>%
    select(-crossover_date)

  return(temp)
}