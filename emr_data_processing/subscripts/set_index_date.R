# Set IndexDate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function creates three new variables to the input data set, Eligible,
# Enrolled, and IndexDate, but uses different approaches as to how to assign
# these variables. In addition to indicating when a patient enters the study, 
# the index date is critical for downstream algorithms to link labs, meds,
# procedures, referrals, and comorbidities. One group of interest is the 
# the group where eligibility criteria is met, but WPV criteria are not. Since
# it is of interest to compare these individuals along the same set of outcomes
# an index date is assigned also assigned to these individuals, but uses a 
# different approach since they never had a WPV

# Assigning index dates are based on the following:

# 1. For Eligible and Enrolled patients (EE)
# The IndexDate in each phase is the date of the first encounter where the 
# patient meets Age, BMI, and WPV criteria. For visits where the encounter date
# is after the index date in each respective phase, Eligible is set to 1 and 
# Enrolled is set to 1.

# 2. For Eligible but not Enrolled (ENE)
# These patients will not have a WPV to mark the initial index date. Instead,
# patients are first assigned to the cohort where the majority of their visits
# occurred. If a tie is detected, then the patient is assigned to the cohort
# where the earliest visit occurred. The index date is then defined as the first 
# Eligible visit in their assigned cohort. For visits where the EncounterDate
# is after the IndexDate, Eligible is set to 1 and Enrolled is set to 0.

# 3. Not Eligible and not Enrolled
# These patients are identified as those that are not in EE nor in ENE groups.
# Eligible is set to 0, Enrolled is set to 0. They do not get an index date 
# because no labs, procedures, or comorbidities will be captured for these 
# patients.

# 203,729 obs for visits_post_id for data delivered 10/17/22, with provider NPI
# 208,799 obs for visits_post_id for data delivered 10/17/22, without provider NPI

# WPV: Count variable, the number of ways that the visit qualified for a WPV, >0 is a WPV, 0 is not a WPV
# IndexDate: Date variable, the date of the first WPV
# IndexVisit: Indicator variable, some patients have multiple visits on the IndexDate, is the visits the index WPV?
# Enrolled: Indicator variable, does the visit meet age, BMI, provider NPI, and WPV?

# Per protocol paper: An index visit will be defined as the patient’s first 
# weight-prioritized visit (either control or intervention). Follow-up visits 
# at which weight is recorded will be associated with the intervention status of
# the patient’s preceding index visit. The intervention condition variable will 
# be a binary indicator that is equal to zero if the patient’s index visit occurs
# at a clinic when it is in the control condition and becomes equal to 1 when 
# the patient has an index visit at a clinic after it has crossed into the 
# intervention condition.


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_index_date <- function(temp){
  # Check to ensure that IndexDate does not already exist ----
  if ("IndexDate" %in% colnames(temp)){
    warning("IndexDate already exists!")
    return(temp)
  } else {

  # ____________________________________________________________________________
  # 1. Create index dates for eligible and enrolled (EE) ----- 
  # For EE index dates are assigned according to the first WPV in each phase
  # i.e. control or intervention, must have a provider NPI and recorded weight
  index_dates <- temp %>%
    # filter(Arb_PersonId == 5826365965) %>%
    drop_na(ProviderNpi) %>%
    filter(WPV > 0,
           IndexVisitEligible == 1) %>% 
    group_by(Arb_PersonId,
             Intervention) %>%
    arrange(Arb_PersonId,
            EncounterDate) %>%
    slice_head() %>% # Takes the first WPV in each phase for each patient
    mutate(IndexDate = EncounterDate) %>%
    ungroup() %>%
    select(Arb_PersonId, Intervention, IndexDate)
  
  ## Merge the index dates to the input data frame and create IndexVisit ----
  # n.b. results in non-wpv visits getting an index date because the left
  # join will merge all visits prior to the index date with a patients index 
  # date.
  temp <- 
  left_join(temp, index_dates,
    by = c("Arb_PersonId","Intervention")) %>%
    mutate(IndexVisit = if_else(EncounterDate == IndexDate & WPV > 0 & IndexVisitEligible == 1, 1, 0)) 
  
  # Some patients may have more than one legitimate Index Visit in any given 
  # intervention phase. For these patients, the first record after arranging by 
  # IndexVisit is set as the index visit. The rest are set to 0.
  temp %<>% 
    # filter(Arb_PersonId == 5826365965) %>%
    group_by(Arb_PersonId, Intervention) %>%
    arrange(desc(IndexVisit)) %>%
    mutate(IndexVisit = if_else(row_number() == 1 & IndexVisitEligible == 1 & WPV > 0 & !is.na(IndexDate), 1, 0)) %>%
    ungroup() #%>%
    # filter(Arb_EncounterId == 116854576707) %>%
    # select(IndexVisit)

  # Check how many patients have a non-WPV intervention visit before the 
  # index WPV visit
  # As of 03/26/2024 28,067 patients have a non-wpv intervention phase visit 
  # before the index wpv intervention visit
  # ids_to_check <- 
  # temp %>% 
  #   filter(EncounterDate < IndexDate, Intervention == 1, WPV == 0) %>% 
  #   distinct(Arb_PersonId) %>%
  #   pull(Arb_PersonId)
  # 
  # 
  # test <- 
  # temp %>%
  #   filter(Arb_PersonId %in% ids_to_check) %>%
  #   group_by(Arb_PersonId) %>%
  #   arrange(EncounterDate) %>%
  #   select(Arb_PersonId, Arb_EncounterId, EncounterDate, Intervention, IndexVisit, WPV)
  # 
  # 
  # temp %>%
  #   filter(Arb_PersonId == 1374816209     ) %>%
  #   arrange(EncounterDate) %>%
  #   select(Arb_PersonId, Arb_EncounterId, EncounterDate, Intervention, IndexVisit, IndexDate, WPV)
  # 
  # 
  # 
  # # Check for the sequence, control, intervention - non WPV, Intervention WPV
  # # Get a list of patients that have visits in both control and intervention
  # ids_to_check <- 
  # temp %>%
  #   group_by(Arb_PersonId) %>%
  #   summarise(n = n_distinct(Intervention)) %>%
  #   filter(n > 1) %>%
  #   pull(Arb_PersonId)
  #   
  #   
  # # ids_to_check <- 
  # # temp %>%
  # #   group_by(Arb_PersonId, Intervention) %>%
  # #   count() %>%
  # #   filter(n > 3) %>%
  # #   pull(Arb_PersonId)
  # 
  # temp %>%
  #   filter(Arb_PersonId %in% ids_to_check) %>%
  #   select(Arb_PersonId, Arb_EncounterId, EncounterDate, Intervention, IndexVisitEligible, IndexVisit) %>%
  #   group_by(Arb_PersonId) %>%
  #   arrange(EncounterDate)
  # 
  # # View an example
  # temp %>% 
  #   filter(Arb_PersonId == 11490976621) %>% 
  #   select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexDate, Intervention, IndexVisitEligible, IndexVisit, WPV) %>%
  #   arrange(EncounterDate)
  #   
  # # Finds people where there is a switch from intervention to contol
  # dates <-
  #   temp %>%
  #   group_by(Arb_PersonId) %>%
  #   arrange(EncounterDate) %>%
  #   mutate(lag = lag(Intervention),
  #          diff_lag_int = lag - Intervention) %>%
  #   filter(diff_lag_int == 1) %>%
  #   slice_head() %>%
  #   ungroup() %>%
  #   select(Arb_PersonId, EncounterDate) %>%
  #   rename(censor_date = EncounterDate)
  # 
  # temp %>%
  #   filter(Arb_PersonId %in% dates$Arb_PersonId) %>%
  #   select(Arb_PersonId, Arb_EncounterId, EncounterDate, Intervention, IndexVisitEligible, IndexVisit) %>%
  #   group_by(Arb_PersonId) %>%
  #   arrange(EncounterDate)
  
  
  
  ## Checks for EE patients ----
  # Double check that each patient has one and only one index visit in each 
  # intervention phase of the study.
  n_index_dates <- temp %>%
    filter(EncounterDate == IndexDate, IndexVisit == 1) %>%
    group_by(Arb_PersonId, Intervention) %>%
    count() %>%
    filter(n > 1) %>%
    nrow()
  
  if (n_index_dates != 0){
    warning("Detected EE patients that have more than one valid index date!!!")
  }
  
  # Double check that the index visit does not occur before the index date
  if (temp %>% filter(EncounterDate < IndexDate, IndexVisit == 1) %>% nrow() != 0){
    warning("Detected EE patients where index visits occur before the index date!!!")
  }
  
  ## Create the cohort variable ----
  # First, capture the GroupID for the very first index visit according to date
  cohorts <- temp %>%
    filter(IndexVisit == 1) %>%
    group_by(Arb_PersonId) %>%
    arrange(EncounterDate) %>%
    slice_head() %>%
    ungroup() %>%
    select(Arb_PersonId, GroupID) %>%
    mutate(Cohort = str_c("Cohort", GroupID)) %>%
    select(-GroupID)
    
  # Merge in the cohort of the first IndexVisit. GroupID preserves the actual
  # Cohort, while the Cohort variable will assign everyone to just one group
  # to facilitate patient flow diagram
  temp %<>%
    left_join(., cohorts, by = "Arb_PersonId") %>%
    group_by(Arb_PersonId) %>%
    fill(Cohort, .direction = "downup") %>%
    ungroup()
  
  # Make Eligible and Enrolled time invariant before and after the index date
  # to simplify data wrangling for EE and ENE groups. The subsets of data will
  # be filtered to visits after index date to prevent any confusion
  
  # Filter ENE to visits after their index dates too. The only group that will
  # not have visits filtered by index dates is the not enrolled not eligible

  
  # ____________________________________________________________________________
  # 2. Create index date for those that are eligible but not enrolled (ENE) ----
  # Start with 4 sub dataframes that comprise the main input of all visits.
  # Only the ENE will need to have the index date set. After processing, 
  # data frames are then stacked before returning as output.
  # In the EE, the index date is set to the first visit where the WPV criteria
  # are met. In the ENE, since there are no WPVs, a different algorithm is used
  # to determine the index date and the cohort.
  
  # A. Create the eligible and enrolled sub set
  # At this point only eligible and enrolled patients have had an index date
  # Therefore, all encounters after the index date are set to Enrolled == 1
  # n.b. These are all of the visits for patients who met the WPV and 
  # eligibility criteria and where the encounter date is on or after the index
  # date.
  ee.post_id <- temp %>%
    filter(EncounterDate >= IndexDate) %>%
    mutate(Eligible = 1, Enrolled = 1)
  
  # B. For eligible and enrolled patients, set Eligible and Enrolled to 0, and
  # set IndexVisit to 0. Patients that are in ee.post_id but encounters that are
  # not in ee.post_id
  ee.pre_id <- temp %>%
    filter(Arb_PersonId %in% ee.post_id$Arb_PersonId, 
           !Arb_EncounterId %in% ee.post_id$Arb_EncounterId) %>%
    mutate(Eligible = 1, Enrolled = 1, IndexVisit = 0)

  # Should be 0 rows
  ee.post_id %>% 
    filter(Enrolled == 1, IndexVisit == 1, IndexVisitEligible == 0) %>%
    nrow()
  
  # Should be 0 rows
  ee.pre_id %>% 
    filter(IndexVisit == 1) %>%
    nrow()
  
  # C. Create the eligible, but not enrolled sub set
  # These encounters are from patients not found in the EE group AND meet
  # eligibility criteria (Age & BMI), just no WPV
  ene <- temp %>%
    filter(!Arb_PersonId %in% ee.post_id$Arb_PersonId,
           IndexVisitEligible == 1) %>%
    mutate(Enrolled = 0)
  
  # *** FROM HERE GET A LIST OF PATIENTS IN ENE. THEN FILTER NENE FOR VISITS 
  # FROM THOSE PATIENTS BECAUSE THEY WILL HAVE AN INDEX ASSIGNED TO THEM
  # THEN NENE CAN BE MADE FROM VISITS FROM PATIENTS NOT IN EE AND NOT IN ENE
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # *** Assign a time invariant Cohort
  # Determine how many index eligible visits per GroupID for each patient
  # Used to determine which Group to place the ENE patients in.
  n_visits_per_cohort <- 
    ene %>%
    drop_na(Weight_kgs) %>% #Ensure that no visits with missing weight are categorized as index visits
    filter(IndexVisitEligible == 1) %>% # Ensure that all visits are eligible, redundancy
    group_by(Arb_PersonId, GroupID) %>%
    count() %>%
    ungroup() %>%
    arrange(GroupID, n) %>%
    rename(n_visits = n)
  
  # Determine number of groups per patient
  # Used to further separate ENE into those who had all their available visits
  # in one GroupID or across several
  n_groups_per_patient <- 
    ene %>%
    drop_na(Weight_kgs) %>%
    filter(IndexVisitEligible == 1) %>%
    group_by(Arb_PersonId) %>%
    summarise(n_group = n_distinct(GroupID)) %>%
    arrange(n_group)
  
  
  # Create a modified version of the ENE data set that contains each IndexVisitEligible
  # visit, its encounter date, the number of visits in each cohort for that patient
  # and the number of cohorts for that patient.
  # Merge in the number of visits in each cohort and number of cohorts 
  ene.mod <- 
    ene %>%
    filter(IndexVisitEligible == 1) %>%
    select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexVisitEligible, Intervention, GroupID) %>%
    left_join(., n_visits_per_cohort, by = c("Arb_PersonId", "GroupID") ) %>%
    left_join(., n_groups_per_patient, by = "Arb_PersonId")
  
  # Determine the proportion of those with one group vs more than one ----------
  # 20240326 - only a small proportion 4.5% have visits in more than one cohort
  # ene.mod %>%
  #   group_by(Arb_PersonId) %>%
  #   slice_head() %>%
  #   ungroup() %>%
  #   mutate(gt1_group = ifelse(n_group > 1, 1, 0)) %>%
  #   select(gt1_group) %>%
  #   tbl_summary()
    

  # Those with one group through out all visits --------------------------------
  # 905,280 for 20231010 data delivery.
  # Just need the index dates for each index visit in each phase
  ene.1.ind <- 
    ene.mod %>%
    filter(n_group == 1) %>%
    # filter(Arb_PersonId ==12288724) %>%
    group_by(Arb_PersonId, Intervention) %>%
    arrange(EncounterDate) %>%
    slice_head() %>%
    ungroup %>%
    select(-Arb_EncounterId, -IndexVisitEligible, -n_visits, -n_group)%>%
    rename(IndexDate = EncounterDate)
  
  # Those with more than one group through out all visits ----------------------
  # First find which cohort to assign them to, based on the cohort that they 
  # have the most visits in.
  # Determine which cohort to place these patients in 
  ene.gt1.cohorts <- 
    ene.mod %>%
    filter(n_group != 1) %>%
    # filter(Arb_PersonId ==8476717) %>% # used for development
    group_by(Arb_PersonId) %>%
    arrange(desc(n_visits), EncounterDate) %>%
    slice_head() %>%
    ungroup() %>%
    select(Arb_PersonId, GroupID)
  
  # Merge in the cohorts and filter for GroupId.x == GroupId.y to eliminate
  # rows where the observation will not equal the index date
  # *** For edge cases, it needs filter IndexVisitEligible == 1 to avoid
  # selecting ineligible visits as the index visits
  
  # For the group with visits in multiple cohorts, merge in each patients' number
  # of cohorts variable, then find the first row where the actual group is equal
  # to the assigned group and where index visit eligible is set to 1, then use
  # the first available visit as the index visit and capture the index date from
  # the encounter date
  ene.gt1.ind <-
    ene %>%
    filter(Arb_PersonId %in% (ene.mod %>%filter(n_group != 1) %>% pull(Arb_PersonId))) %>% 
    left_join(., ene.gt1.cohorts, by = "Arb_PersonId") %>%
    # filter(Arb_PersonId == 5483794710 ) %>%                                           # used for development
    # select(-(ProviderSex:Type), -starts_with("WPV"), -(BirthDate:Race_Ethnicity)) %>% # used for development
    # arrange(Arb_PersonId, Intervention, EncounterDate) %>%                            # used for development
    filter(GroupID.x == GroupID.y, IndexVisitEligible == 1) %>%
    # filter(Arb_PersonId ==8476717) %>%                                                # used for development
    group_by(Arb_PersonId, Intervention) %>%
    arrange(EncounterDate) %>%
    slice_head() %>%
    ungroup() %>%
    select(Arb_PersonId, EncounterDate, Intervention, GroupID.y) %>%
    rename(IndexDate = EncounterDate,
           GroupID = GroupID.y)
  
  
  # Stack the index dates from the two subgroups of ENE patients ---------------
  # Those with only one eligible index date and those with greater than 1
  index_dates <- 
    bind_rows(ene.1.ind, ene.gt1.ind) %>%
    # mutate(Cohort = str_c("Cohort", GroupID)) %>%
    select(-GroupID)
  
  
  # Merge the index dates into the ENE subset
  ene <- 
    ene %>%
    # First remove the columns that will need to be merged in
    select(-IndexDate, -Cohort) %>%
    left_join(., 
              (index_dates %>% select(Arb_PersonId, IndexDate, Intervention)),
              by = c("Arb_PersonId", "Intervention")) 
  
  # Create the IndexVisit binary indicator
  ene %<>%
    mutate(IndexVisit = ifelse(IndexVisitEligible == 1 & EncounterDate == IndexDate, 1, 0))
  
  # Some patients will legitimately have more than one index visit on the same
  # day
  ene %<>% 
    group_by(Arb_PersonId, Intervention) %>%
    arrange(desc(IndexVisit)) %>%
    # mutate(IndexVisit = if_else(row_number() == 1, 1, 0)) %>%
    mutate(IndexVisit = if_else(row_number() == 1 & IndexVisitEligible == 1 & !is.na(IndexDate), 1, 0)) %>%
    ungroup()
  
  # Then merge in the GroupID
  # ene %<>%
  #   # The index_dates df has multiple rows per patient, but only one is needed
  #   # hence the slice_head() verb
  #   left_join(., 
  #             (index_dates %>% select(Arb_PersonId, Cohort) %>% group_by(Arb_PersonId) %>% slice_head()),
  #             by = "Arb_PersonId")
  
  ene %<>%
    mutate(Eligible = 1)
  
  cohorts.ene <- ene %>%
    filter(IndexVisit == 1) %>%
    group_by(Arb_PersonId) %>%
    arrange(EncounterDate) %>%
    slice_head() %>%
    ungroup() %>%
    select(Arb_PersonId, GroupID) %>%
    mutate(Cohort = str_c("Cohort", GroupID)) %>%
    select(-GroupID)
  
  ene %<>%
    left_join(., cohorts.ene, by = "Arb_PersonId") 
  
  
  # Lastly, reorder the columns in ene to match those in ee to prepare for
  # stacking the subsets back again
  ene %<>% select(names(ee.post_id))
  
  
  
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # *** Needs to be fixed to not include any patients in ene too
  # D. Create Not Eligible, Not Enrolled.
  # These encounters are from patients not found in the EE group, but do not 
  # meet eligibility criteria.
  nene <- temp %>%
    filter(!Arb_PersonId %in% ee.post_id$Arb_PersonId,
           IndexVisitEligible == 0) %>%
    mutate(Eligible = 0, Enrolled = 0, IndexVisit = 0)
    
  # In it's current form nene will contain records from patients that ended up
  # in ene. These records will eventually be filtered out when filtering ene to
  # those after their respective index date since they will have NAs for 
  # IndexDate
  nene %<>%
    mutate(Eligible = ifelse(Arb_PersonId %in% ene$Arb_PersonId, 1, Eligible))
  
  nene %<>%
    select(names(ee.post_id))
  
  # Filter the ene group to the problematic person ids to see how their
  # data get captured
  # *** Used only for development for handling a small set of edge cases.
  # ene %<>% filter(Arb_PersonId %in% c(1240955011,
  #                                     3386978716,
  #                                     5704696458,
  #                                     2256817050,
  #                                     5483794710,
  #                                     3878974480,
  #                                     1762842633))
  
  
  # Check that the sub groups were divided appropriately. The sum of encounters
  # in all sub groups should equal the total number of rows in the input data
  # frame.
  # N rows in EE after the index date
  # N rows in EE before the index date
  # N rows in ENE, eligible but not enrolled due to not having a WPV
  # N rows in NENE, not eligible, not enrolled
  if ((dim(ee.post_id)[1] + dim(ene)[1] + dim(nene)[1] + dim(ee.pre_id)[1]) != dim(temp)[1]){
    warning("Subgroups(EE, ENE, NENE) do not equal total number of input observations, consider reviewing how subgroups were created.")
  }

  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Bind all the individual sub data frames to return
  temp <- bind_rows(ee.post_id, ee.pre_id, ene, nene)
    return(temp)
  }
}

