# Medications %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# medications() function is designed to capture medications that cause weight
# loss or weight gain, and medications that are approved as anti-obesity
# medications at a given reference date either in control (0) or intervention 
# (1) within the specified date range 

# Algorithm Changes:
# 07/28/2023: capture only those related to the index visits by EncounterId
# 08/01/2023: Leigh & Qing, capture within a 2 month window, 30 days previous
#             and 30 days after the index/reference date
# 10/24/2024: Stats group, capture the medications that are found at any time
#             within the intervention phase
# 11/15/2024: Stats group, added the capability to count AOMs that were
#             started in control, but continued into the intervention.
#             Commented out the weight gain/loss sections, because they need
#             further development to match the AOM algorithm, and set aside to
#             complete at another time since weight gain and weight loss meds
#             are not reported in the primary aims paper. Set asided to focus
#             on AOMs.
# 01/24/2025: Returned functionality to get N_meds_weight gain and
#             N_meds_weight loss for Aim 2 since these were placed on the back
#             burner to focus on just getting N_meds_AOM for the Aim 1 paper
#             during previous development efforts. Algorithm now functions to
#             to count the number of medications ordered in control, and the
#             number of medications ordered and continued into intervention
#
# Dependencies:
# meds_bin_vars is imported from the medications.xlsx in the working files
# directory. meds_bin_vars created as a dataframe that lists the
# uniqueMedName and a set of binary variables to indicate if it's anti-obesity
# (AOM), causes weight gain or weight loss
#
# meds_gen_names is imported from the medication.xlsx in the working files
# directory
#
# aom_lookup.csv contains a list of medications from EPIC classified as anti-
# obesity medications

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
capture_medications <- function(temp) {
  # Filter the input data frame to the index visits in the specified phase
  temp %<>% 
    filter(Censored == 0,
            IndexVisit == 1)
  
  # Filter the meds table, because an updated meds table delivered on 2024-11-13
  # had additional rows that were beyond the data delivery date of 2024-09-17.
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
  

  # ANTI OBESITY MEDICATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Load the AOM lookup table -------------------------------------------------
  # Contains a list of all the medications where Therapeutic class == 
  # "anti obesity drugs"
  aom_lookup <- read_csv(here("working_files/medications/aoms_lookup.csv"))

  # Filter the meds table to only those medications for the sample of interest
  # and that are anti obesity
  meds_sub <-
    meds %>% 
        filter(Arb_PersonId %in% temp$Arb_PersonId,
               MedicationName %in% aom_lookup$MedicationName)

  # Merge in the keyword/generic name
  # Some MedicationNames like orlistat (XENICAL), have multiple MedicationEpicIds
  # and have multiple keywords, but have a common generic name, so the generic
  # names are merged in.
  meds_sub <-
    meds_sub %>%
      left_join(., 
                (aom_lookup %>% 
                  select(MedicationName, MEDICATIONEPICID,
                         GENERICNAME, keyword) %>%
                  group_by(MedicationName) %>% 
                  slice_head() %>% 
                  ungroup()),
                by = "MedicationName")

  # Merge in the cohort for each patient. Function is designed to only need to
  # get the index visits from one phase at a time either control or
  # intervention for this step, therefore no need to specify any group_by
  # phase manipulations as a phase specific conditional statement is used below
  meds_sub <-
    meds_sub %>%
        left_join(., 
                  (temp %>% select(Arb_PersonId, Cohort)),
                  by = "Arb_PersonId")

  # Assign a crossover date that indicates when the cohort crossed over from
  # control to intervention phase
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


  # MEDICATIONS THAT CAUSE WEIGHT GAIN OR LOSS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Load and process working files --------------------------------------------
  # Load the uniqueMedName sheet as meds_bin_vars ----
  # Contains the unique medication names along with a set of binary variables
  # to indicate whether a unique medication name is either anti obesity
  # medication (aom), causes weight gain, or causes weight loss
  meds_bin_vars <-
    read.xlsx(
              here("working_files/medications/medications_20221017.xlsx"),
              sheet = "UniqueMedName")

  # Load the Medications sheet as meds_gn_names ----
  meds_gen_names <-
    read.xlsx(
              here("working_files/medications/medications_20221017.xlsx"),
              sheet = "Medications")

  # Process meds_bin_vars ----
  # corresponds to uniqueMedname sheet in medications.xlsx
  # converts all Xs to 1 and all NAs to 0
  meds_bin_vars %<>%
    mutate(across(AOM:weight.loss, ~ ifelse(is.na(.x), 0, 1))) %>%
    select(-Generic.name, -trade.names) %>%
    distinct()

  # Process meds list ----
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
    filter(Arb_PersonId %in% temp$Arb_PersonId) %>%
    mutate(across(MedicationName:GenericName, tolower)) %>%
    distinct()

  # Meds subset where the medication generic names and type are merged in
  meds_sub %<>%
    left_join(., meds_gen_names, by = "GenericName", relationship = "many-to-many")

  # Merge in the cohort from the input data frame to create the cross over date
  meds_sub <-
    meds_sub %>%
        left_join(., 
                  (temp %>% select(Arb_PersonId, Cohort)),
                  by = "Arb_PersonId")

  # Assign a crossover date that indicates when the cohort crossed over from
  # control to intervention phase
  meds_sub <-
    meds_sub %>%
      mutate(crossover_date = case_when(
      Cohort == "Cohort1" ~ "2021-03-17",
      Cohort == "Cohort2" ~ "2022-03-17",
      .default = "2023-03-17"))

  # Create meds_data conditionally on phase ----
  # Primary data frame that will be used to create n_meds and aom_wide
  if (phase == "Control") {
    meds_data <- 
    meds_sub %>%
      filter(weight.gain == 1 | weight.loss == 1) %>%
      filter(OrderedDate < crossover_date)

  } else {
    meds_data <- 
    meds_sub %>%
      filter(weight.gain == 1 | weight.loss == 1) %>%
      filter(OrderedDate >= crossover_date |
             (OrderedDate < crossover_date & EndDate >= crossover_date) |
             (OrderedDate < crossover_date & is.na(EndDate)))
  }

  # At this point weight.gain and weight.loss are two mutually exclusive variables
  n_meds <- meds_data %>%
    group_by(Arb_PersonId, weight.gain) %>%
    summarise(n_unique = n_distinct(GenericName), .groups = "drop") %>%
    mutate(weight.gain = ifelse(weight.gain == 1, "N_Meds_Gain", "N_Meds_Loss")) %>%
    group_by(Arb_PersonId) %>%
    pivot_wider(names_from = weight.gain, values_from = n_unique) %>%
    ungroup() %>%
    mutate(across(N_Meds_Gain:N_Meds_Loss, ~ ifelse(is.na(.x), 0, .x)))
  
  temp %<>%
    left_join(., n_meds, by = "Arb_PersonId")

  # Fix the NAs by converting to 0 when medications that cause weight gain or
  # or loss are also counted
  temp %<>%
    mutate(across(all_of(c("N_Meds_Gain", "N_Meds_Loss")), ~ ifelse(is.na(.), 0, .)))

  return(temp)
}