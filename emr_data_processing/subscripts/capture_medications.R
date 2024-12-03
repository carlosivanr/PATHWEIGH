# Medications %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# medications() function is designed to capture medications that cause weight
# loss or weight gain, and medications that are approved as anti-obesity
# medications at a given reference date either in control (0) or intervention 
# (1) within the specified date range 

# Algorithm Changes:
# - 07/28/2023 capture only those related to the index visits by EncounterId
# - 08/01/2023 Leigh & Qing, capture within a 2 month window, 30 days previous
#   and 30 days after the index/reference date
# - 10/24/2024 Stats group, capture the medications that are found at any time
#   within the intervention phase
# - 11/15/2024 Stats group, added the capability to count AOMs that were
#   started in control, but continued into the intervention.
#   Commented out the weight gain/loss sections, because they need further
#   development to match the AOM algorithm, and set aside to complete at
#   another time since weight gain and weight loss meds are not reported
#   in the primary aims paper. Set asided to focus on AOMs.

# Layout out of the older algorithm to capture weight gain/loss meds
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

# meds_bin_vars is imported from the medications.xlsx in the working files
# directory. meds_bin_vars created as a dataframe that lists the
# uniqueMedName and a set of binary variables to indicate if it's anti-obesity
# (AOM), causes weight gain or weight loss

# meds_gen_names is imported from the medication.xlsx in the working files
# directory

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# prior version ----------------------------------------------------------------
capture_medications <- function(temp) {
  ## Filter the input data frame to the index visits in the specified phase ----
  temp %<>% 
    filter(Censored == 0,
            IndexVisit == 1)
  
  # Filter the meds table, because an updated meds table delivered on
  # 2024-11-13 had additional rows that were beyond the data delivery date of
  # 2024-09-17
  meds %<>%
    filter(OrderedDate <= date_max)

  # Check which phase the input data frame is in. If all of the values are 0
  # then the input is in the control phase, otherwise it is in the intervention
  # phase
  if ((names(table(temp$Intervention))) == "0") {
    phase <- "Control"
  } else {
    phase <- "Intervention"
  }
  print(phase)
  

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # This section is to capture patients that have a documented use of an anti-
  # obesity medication 

  # Load the AOM lookup table -------------------------------------------------
  # Contains a list of all the medications where Therapeutic class == 
  # "anti obesity drugs"
  aom_lookup <- read_csv(here("working_files/medications/aoms_lookup.csv"))

  # Filter the meds table to only those medications for the sample of interest
  # and that are anti obesity
  meds_sub <-
    meds %>% 
        filter(
          Arb_PersonId %in% temp$Arb_PersonId,
          MedicationName %in% aom_lookup$MedicationName)

  # Merge in the keyword/generic name
  # Some MedicationNames like orlistat (XENICAL), have multiple MedicationEpicIds
  # and have multiple keywords, but have a common generic name
  meds_sub <-
    meds_sub %>%
      left_join(., 
        (aom_lookup %>% 
          select(MedicationName, MEDICATIONEPICID, GENERICNAME, keyword) %>% 
          group_by(MedicationName) %>% 
          slice_head() %>% 
          ungroup()),
        by = "MedicationName")

  # Merge in the cohort for each patient, only need to get the index visits from
  # one phase either control or intervention for this step
  meds_sub <-
    meds_sub %>%
        left_join(., 
                  (temp %>%select(Arb_PersonId, Cohort)),
                  by = "Arb_PersonId")

  # Assign a crossover date
  meds_sub <-
    meds_sub %>%
      mutate(crossover_date = case_when(
      Cohort == "Cohort1" ~ "2021-03-17",
      Cohort == "Cohort2" ~ "2022-03-17",
      .default = "2023-03-17"))

  if (phase == "Control") {
    # If the reference visits are from the Control phase, then keep records
    # where the OrderedDate are before the cross over date. This will capture
    # records of AOMs that were ordered. n.b. OrderDate doesn't always equal
    # the start date
    meds_data <- 
    meds_sub %>%
      filter(OrderedDate < crossover_date)
  } else {
    # If the reference visits are from the Intervention phase, then keep the
    # records where 1) the OrderedDate is on or after the cross over date to
    # get records for those that were ordered an AOM in intervention; 2) the
    # EndDate is on or after the cross over date to get records for those that
    # had an Ordered date before the cross over, but continued medication into
    # the intervention; and 3) the EndDate is missing, to get records for those
    # that are continuing AOM medication
    meds_data <-
    meds_sub %>%
      filter(OrderedDate >= crossover_date |
        (OrderedDate < crossover_date & EndDate >= crossover_date) |
        (OrderedDate < crossover_date & is.na(EndDate)))
  }

  # Get a count of the number of AOMs per patient,
  count_aoms <-
      meds_data %>%
        group_by(Arb_PersonId) %>%
        count() %>%
        ungroup() %>%
        rename(N_Meds_AOM = n)

  temp %<>%
    left_join(., count_aoms, by = "Arb_PersonId")

  temp %<>%
    mutate(across(all_of(c("N_Meds_AOM")), ~ifelse(is.na(.), 0, .)))

  # temp %<>%
  #   select(-crossover_date)

  return(temp)
}

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ***************************************************************************
  # This section was drafted to count the number of patients on medications 
  # that cause weight gain or weight loss. However, it needs further
  # development to capture patients that may have a medication ordered in
  # control and then continued use in the intervention phase. Development is
  # needed in the code chunk titled "Create meds_data conditionally"
  # ***************************************************************************
  
  # # Set the crossover date when the clinics cross from control to intervention 
  # temp %<>%
  #     mutate(crossover_date = case_when(
  #       Cohort == "Cohort1" ~ "2021-03-17",
  #       Cohort == "Cohort2" ~ "2022-03-17",
  #       .default = "2023-03-17"))

  # # Load and process working files ---------------------------------------------
  # ## Load the uniqueMedName sheet as meds_bin_vars ----
  # # Contains the unique medication names along with a set of binary variables
  # # to indicate whether a unique medication name is either anti obesity
  # # medication (aom), causes weight gain, or causes weight loss
  # meds_bin_vars <-
  #   read.xlsx(
  #             here("working_files/medications/medications_20221017.xlsx"),
  #             sheet = "UniqueMedName")

  # ## Load the Medications sheet as meds_gn_names ----
  # meds_gen_names <-
  #   read.xlsx(
  #             here("working_files/medications/medications_20221017.xlsx"),
  #             sheet = "Medications")

  # ## Process meds_bin_vars ----
  # # corresponds to uniqueMedname sheet in medications.xlsx
  # # converts all Xs to 1 and all NAs to 0
  # meds_bin_vars %<>%
  #   mutate(across(AOM:weight.loss, ~ ifelse(is.na(.x), 0, 1))) %>%
  #   select(-Generic.name, -trade.names) %>%
  #   distinct()

  # ## Process meds list ----
  # # corresponds to Medications sheet in medications.xlsx
  # meds_gen_names %<>%
  #   mutate(across("GenericName", tolower)) %>% 
  #   select(uniqueMedName, GenericName) %>%
  #   drop_na() %>%
  #   distinct()

  # # Merge med_gen_names and meds_bin_vars
  # meds_gen_names %<>%
  #   left_join(., meds_bin_vars, by = "uniqueMedName") %>%
  #   as_tibble()

  # # Subset meds table ----
  # # Subset the meds table and convert medication and generic names to lower
  # # case to match meds_gen_names
  # meds_sub <- meds %>%
  #   filter(Arb_PersonId %in% temp$Arb_PersonId) %>%
  #   mutate(across(MedicationName:GenericName, tolower)) %>%
  #   distinct()

  # # Create meds_data conditionally on phase ----
  # # Primary data frame that will be used to create n_meds and aom_wide
  # if (phase == "Control") {
  #   meds_data <- 
  #   meds_sub %>%
  #     left_join(., meds_gen_names, by = "GenericName", relationship = "many-to-many") %>%
  #     left_join(., (temp %>% select(Arb_PersonId, crossover_date) %>% group_by(Arb_PersonId) %>% slice_head() %>% ungroup()), by = "Arb_PersonId") %>%
  #     filter(OrderedDate < crossover_date)

  # } else {
  #   # %% Fix the way the intervention medications are capture because this will
  #   # not count the medications that were started in control, but then
  #   # continued into the intervention phase
  #   meds_data <- 
  #   meds_sub %>%
  #     left_join(., meds_gen_names, by = "GenericName", relationship = "many-to-many") %>%
  #     left_join(., (temp %>% select(Arb_PersonId, crossover_date) %>% group_by(Arb_PersonId) %>% slice_head() %>% ungroup()), by = "Arb_PersonId") %>%
  #     filter(OrderedDate >= crossover_date)
  # }

  # n_meds <- meds_data %>%
  #   group_by(Arb_PersonId, uniqueMedName) %>%
  #   slice_head() %>%
  #   ungroup() %>%
  #   group_by(Arb_PersonId) %>%
  #   summarise(N_Meds_Gain = sum(weight.gain, na.rm = TRUE),
  #             N_Meds_Loss = sum(weight.loss, na.rm = TRUE))
  
  # temp %<>%
  #   left_join(., n_meds, by = "Arb_PersonId")

  # Fix the NAs by converting to 0 when medications that cause weight gain or
  # or loss are also counted

  # temp %<>%
  #   mutate(across(all_of(c("N_Meds_Gain", "N_Meds_Loss", "N_Meds_AOM")), ~ifelse(is.na(.), 0, .)))  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%