# Medications %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function has two primary goals:

# First, creates a data frame that counts the number of medications that cause 
# weight gain, weight loss, and are classified as Anti-Obesity (AOM) for each 
# reference visit within a specified date range.

# The second goal is to create a "wide" data frame of AOMs that can either be
# merged into the main visits_post_id data frame or saved for plotting.

# Algorithm Changes:
# - 07/28/2023 capture only those related to the index visits by EncounterId
# - 08/01/2023 Leigh & Qing, capture within a 2 month window, 30 days previous
# and 30 days after the index/reference date
# - 02/01/2024 

# 1. Combine the two sheets in the medications.xlsx file located in the working 
# files directory.
#   a. Process meds_bin_vars which needs to be uniqueMedName and three binary 
#      variables for AOM, weight.gain and weight.loss
#   b. Process meds_gn_names which needs to be uniqueMedname and GenericName columns
# 2. Process the meds table by filtering to patients in the input data frame
#    and filtering by Active meds set to "Y".
# 3. Create meds_full which combines meds_gn_names, meds_bin_vars, and meds_sub
#   a. Merge meds_gn_names with meds_bin_vars
#   b. merge 3.a with meds_sub
#   *** Meds_full is created in 2 steps, with meds_aom sandwiched in between
#   implementing the two steps in sequence leads to the same results as OG,
#   but then leads to a mismatch in meds_AOM. 
#   if the creation of meds_full is split, then the results will be the same 
#   as OG

# meds_aom vs aom_meds

# meds_sub - created by subsetting the meds table for active medications and for
# the patients in the input data frame

# meds_bin_vars is imported from the medications.xlsx in the working files directory
# meds_bin_vars created as a dataframe that lists the uniqueMedName and a set of 
# binary variables to indicate if it's anti-obesity (AOM), causes weight gain
# or weight loss


# meds_gn_names is imported from the medication.xlsx in the working files directory

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

capture_medications <- function(temp) {
  # tic()
  
  # Set the number of days prior to and after the index/reference visit
  # n.b. days after reference will be negative when subtracting reference
  # from OrderedDate
  n_days_prior <- 30
  n_days_after <- -30
  
  # Load and process working files ---------------------------------------------
  ## Load the uniqueMedName sheet as meds_bin_vars ----
  # Contains the unique medication names along with a set of binary variables 
  # to indicate whether a unique medication name is either anti obesity 
  # medication (aom), causes weight gain, or causes weight loss
  meds_bin_vars <- read.xlsx(
    here("working_files/medications/medications_20221017.xlsx"), 
    sheet = "UniqueMedName")
  
  ## Load the Medications sheet as meds_gn_names ----
  meds_gen_names <- read.xlsx(
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
    left_join(. , meds_bin_vars, by = "uniqueMedName")
    
  
  ## Filter the input data frame to the index visits in the specified phase ----
  temp %<>% 
    filter(Censored == 0, 
           IndexVisit == 1)
  
  # Save the number of rows of the input data frame to ensure merges are conducted correctly
  n_obs_start <- dim(temp)[1]
  
  # Create distinct_pts_x_ind ----
  # distinct_pts_x_ind borrowed from labs & procedures script. Originally 
  # intended to capture labs at different index visits. New purpose is to 
  # capture the unique patient ids and index dates to filter meds table and
  # merge with meds data
  distinct_pts_x_ind <- 
    temp %>% 
    filter(Censored == 0,
           IndexVisit == 1) %>%
    select(Arb_PersonId, IndexDate) %>%
    distinct() %>%
    filter(!is.na(IndexDate))
  
  
  # Check patient Ids with more than one unique distinct index date for trouble
  # shooting
  if ((distinct_pts_x_ind %>%
       group_by(Arb_PersonId) %>%
       count() %>%
       filter(n > 1) %>%
       nrow()) != 0){
    stop("Distinct patients at index are not unique!!")
  }
  
  # Subset meds table ----
  # Subset the meds table and convert medication and generic names to lower
  # case to match meds_gen_names
  meds_sub <- meds %>%
    filter(ActiveMed == "Y", Arb_PersonId %in% distinct_pts_x_ind$Arb_PersonId) %>%
    mutate(across(MedicationName:GenericName, tolower))
  
  # Remove any duplicated rows
  meds_sub <- meds_sub[!duplicated(meds_sub),]
  
  # Check to see that all values are unique
  if (n_distinct(meds_sub) != dim(meds_sub)[1]){
    stop("Duplicated rows in meds_sub, review and revise !!!")
    }
  
  # Create meds_data ----
  # Primary data frame that will be used to create n_meds and aom_wide
  meds_data <-
    left_join(distinct_pts_x_ind,
              select(meds_sub, Arb_PersonId, OrderedDate, MedicationName, GenericName, ActiveMed),
              by=c("Arb_PersonId")) %>%
    filter((IndexDate-OrderedDate) <= n_days_prior,
           (IndexDate-OrderedDate) >= n_days_after) %>%
    filter(GenericName %in% meds_gen_names$GenericName) %>%
    left_join(., meds_gen_names, by = "GenericName", relationship = "many-to-many") %>%
    group_by(Arb_PersonId, GenericName) %>%
    slice_head() %>%
    ungroup()
      

  ## Create N_Meds (gain, loss, AOM) -------------------------------------------
  n_meds <- meds_data %>%
    select(Arb_PersonId, IndexDate, AOM:weight.loss) %>%
    group_by(Arb_PersonId) %>%
    summarise(N_Meds_Gain = sum(weight.gain),
              N_Meds_Loss = sum(weight.loss),
              N_Meds_AOM = sum(AOM))
  
  # Capture column names that will need NAs to be set to 0
  n_meds_names <- names(n_meds %>% select(-Arb_PersonId))
  
  
  ## Create AOMs ---------------------------------------------------------------
  # n.b. only subsets medications classified as AOM
  aom_wide <-   
    meds_data %>%
    filter(uniqueMedName %in% 
             (meds_data %>% 
                filter(AOM == 1) %>% 
                distinct(uniqueMedName) %>% 
                pull(uniqueMedName))) %>%
    select(Arb_PersonId, uniqueMedName) %>%
    group_by(Arb_PersonId, uniqueMedName) %>%
    slice_head() %>%
    ungroup() %>%
    mutate(count = 1) %>%
    pivot_wider(., names_from = uniqueMedName, values_from = count)

  
  # Capture column names that will need NAs to be set to 0
  aom_names <- names(aom_wide %>% select(-Arb_PersonId))
  
  
  # Merge N_Meds and AOM_wide to input data frame ------------------------------
  # *** NOW WE ARE READY TO MERGE WITH INDEX VISITS AT CONTROL
  temp %<>%
    left_join(., n_meds, by = "Arb_PersonId")
  
  temp %<>%
    left_join(., aom_wide, by = "Arb_PersonId")
  
  # Check the original values to ensure rows were not introduced
  n_obs_end <- dim(temp)[1]
  
  if (n_obs_start != n_obs_end){
    stop("n_obs_start != n_obs_end. Artefactual rows possibly introduced during merge.")
    }
  
  
  # THEN FIX NAs
  temp %<>%
    mutate(across(all_of(c(n_meds_names, aom_names)), ~ifelse(is.na(.), 0, .)))
  
  
  # toc()
  return(temp)
}
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# # %%%%%%%%%%%%%%%%%%%%%% Data Review %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Check meds compass delivery table
# # unique number of patients in meds
# # 243,109 for dataset 2023-03-22
# n_distinct(meds$Arb_PersonId)
# 
# # how many patients in meds are in visits_post_id
# # 36,031 for dataset 2023-03-22
# meds %>%
#   filter(Arb_PersonId %in% temp$Arb_PersonId) %>%
#   pull(Arb_PersonId) %>%
#   n_distinct(.)
# 
# # how many unique encounters in meds
# n_distinct(meds$Arb_EncounterId)
# 
# # how many encounters in meds are in encounter compass table
# # 1,042,622 for dataset 2023-03-22 n.b. these data are restricted
# # to visits falling within the study start date and data delivery date which
# # equals 2,495,965 which equals about 40% of our delivered encounters have
# # a linked record
# meds %>%
#   filter(Arb_EncounterId %in% encounter$Arb_EncounterId) %>%
#   pull(Arb_EncounterId) %>%
#   n_distinct(.)
# 
# 
# # how many encounters in meds are in visits_post_id
# # 43,607 for dataset 2023-03-22 which equals about 17% of ee encounters that
# # have a meds record
# meds %>%
#   filter(Arb_EncounterId %in% temp$Arb_EncounterId) %>%
#   pull(Arb_EncounterId) %>%
#   n_distinct(.)
# 
# 
# # Tabulate the number of yes and no active medications according to encounter
# meds %>%
#   filter(Arb_EncounterId %in% temp$Arb_EncounterId) %>%
#   pull(ActiveMed) %>%
#   table(.)
# 
# 
# # Tabulate the number of yes and no active medications according patient
# meds %>%
#   filter(Arb_PersonId %in% temp$Arb_PersonId) %>%
#   pull(ActiveMed) %>%
#   table(.)
# 
# # How many records would we have meds for if we just used the index visits?
# meds %>%
#   filter(Arb_EncounterId %in% (temp %>% filter(IndexVisit == 1, Censored == 0) 
#                                %>% pull(Arb_EncounterId))) %>%
#   nrow()
# 
# # How many patients to these records represent?
# # Enough data for 11,770 patients out of 54,554, about 22%
# meds %>%
#   filter(Arb_EncounterId %in% (temp %>% filter(IndexVisit == 1, Censored == 0) 
#                                %>% pull(Arb_EncounterId))) %>%
#   pull(Arb_PersonId) %>%
#   n_distinct()
# 
# # How many unique patients with index visits?
# # for 2023-03-22 dataset its 54,554 unique patients
# temp %>% 
#   filter(IndexVisit == 1, Censored == 0) %>% 
#   pull(Arb_PersonId) %>%
#   n_distinct()
# 
# 
# # Not all records are active meds, 
# # so we actually end up with active meds records for 2,683 patients at index visits
# # vs the 36,940 
# meds %>%
#   filter(Arb_EncounterId %in% (temp %>% filter(IndexVisit == 1, Censored == 0) 
#                                %>% pull(Arb_EncounterId))) %>%
#   filter(ActiveMed == "Y") %>%
#   pull(Arb_PersonId) %>%
#   n_distinct()
# 
# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%