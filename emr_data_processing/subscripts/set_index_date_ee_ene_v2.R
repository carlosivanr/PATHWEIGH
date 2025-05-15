# Set IndexDate EE vs ENE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function creates three new variables to the input data set, Eligible,
# Enrolled, and IndexDate, but uses different approaches as to how to assign
# these variables that are different in comparison to the approach in Aim1. 
# Thes data are intended for the comparison of EE vs ENE in Aim 3a (paper 4).

# Assigning index dates are based on the following:

# 1. For Eligible and Enrolled patients (EE)
# The IndexDate in each phase is the date of the first encounter where the 
# patient meets Age, BMI, and has a recorded weight.

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

# WPV: Count variable, the number of ways that the visit qualified for a WPV, >0 is a WPV, 0 is not a WPV
# IndexDate: Date variable, the date of the first WPV
# IndexVisit: Indicator variable, some patients have multiple visits on the IndexDate, is the visits the index WPV?
# Enrolled: Indicator variable, does the visit meet age, BMI, provider NPI, and WPV?

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_index_date_ee_ene <- function(temp) {
  # Check to ensure that IndexDate does not already exist ----
  if ("IndexDate" %in% colnames(temp)){
    warning("IndexDate already exists!")
    return(temp)
  } else {


    # _________________________________________________________________________
    # 1. Create index dates for eligible and enrolled (EE) ----- 
    # IndexVisitEligible indicates the age is 18+ and at least 25 BMI
    # Load the ee_ene data frame to get the Arb_PersonIds of the ee patients

    # These are patients that received care for their weight at some point in
    # time
    ee_pts <- temp %>%
      filter(WPV > 0) %>%
      distinct(Arb_PersonId)


    # Create the EE variable, if the Arb_PersonId is in the list of ee_pts
    temp %<>%
      mutate(EE = ifelse(Arb_PersonId %in% ee_pts$Arb_PersonId, 1, 0))

    # The first visit with a weight
    index_dates <- temp %>%
      # drop_na(ProviderNpi) %>%
      drop_na(Weight_kgs) %>% 
      arrange(Arb_PersonId,
              EncounterDate) %>%
      group_by(Arb_PersonId) %>%
      slice_head() %>% # Takes the first encounter where the patient is eligible and has a recorded weight
      mutate(IndexDate = EncounterDate) %>%
      ungroup() %>%
      select(Arb_PersonId, IndexDate)


    # Merge in the index dates
    temp %<>%
      left_join(., index_dates, by = "Arb_PersonId")

    # Filter out visits to only those after the index date
    temp %<>%
      filter(EncounterDate >= IndexDate)

    # Create the index visit variable
    temp %<>%
      mutate(IndexVisit = ifelse(EncounterDate == IndexDate & IndexVisitEligible == 1, 1, 0))

    # Number of patients that have more than one legitimate IndexVisit
    temp %>% 
      filter(IndexVisit == 1) %>% 
      group_by(Arb_PersonId) %>% 
      count() %>% 
      filter(n > 1) %>%
      nrow()
      
    # Some patients may have more than one legitimate Index Visit in any given 
    # intervention phase. For these patients, the first record after arranging by 
    # IndexVisit is set as the index visit. The rest are set to 0.
    temp %<>% 
      group_by(Arb_PersonId) %>%
      arrange(desc(IndexVisit)) %>%
      mutate(IndexVisit = if_else(row_number() == 1 & IndexVisitEligible == 1 & !is.na(IndexDate), 1, 0)) %>%
      ungroup()

    ## Checks for EE patients ----
    # Double check that each patient has one and only one index visit in each 
    # intervention phase of the study.
    n_index_dates <- temp %>%
      filter(EncounterDate == IndexDate, IndexVisit == 1) %>%
      group_by(Arb_PersonId) %>%
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
    # Cohort. I.E. GroupID is the time varying variable, whereas Cohort is the
    # time invariant variable
    temp %<>%
      left_join(., cohorts, by = "Arb_PersonId") %>%
      group_by(Arb_PersonId) %>%
      fill(Cohort, .direction = "downup") %>%
      ungroup()

    # Create a data frame for the eligible and enrolled
    # ee <-
    #   temp %>%
    #   filter(Arb_PersonId %in% ee_pts$Arb_PersonId,
    #          EncounterDate >= IndexDate) %>%
    #   mutate(Eligible = 1, Enrolled = 1)

    # column_names <- names(ee)

    # # Create a data frame for the eligible not enrolled. Will use the ee_ene
    # # dataframe to avoid having to process the ene group
    # ene <-
    # ee_ene %>%
    #   filter(!Arb_PersonId %in% ee_pts$Arb_PersonId) %>%
    #   select(all_of(column_names))

    # # Stack the ee and ene data sets
    # output <-
    #   bind_rows(ee, ene) %>%
    #   drop_na(Weight_kgs)

    temp %<>%
      drop_na(Weight_kgs)


    return(temp)
  }
}