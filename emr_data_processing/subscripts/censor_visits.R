# Censor Visits %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Description:
# Creates a censored flag at the visit/encounter level, not at the patient 
# level.

# 1. For various reasons, some patients will have an intervention WPV at a date
# before the date of a control WPV. This function was created to censor all 
# visits from these identified patients.
# 2. Additionally, it is possible for an encounter to have a PW_flow or WMQ WPV
# in control when these tools are supposed to only be available for intervention
# phase of the study.

# WPV_OBHPI is defined as a WPV that is associated with OBHPI FlowsheeRowEpic
# Ids in the FlowsheetIDs_Obesity_Brief_HPI_PW.csv files. These encounters
# indicate the OBHPI was used.

# WPV_PW_flow is defined as a WPV that is associated with one of the 9, 
# non-overlapping FlowsheetRowEpicIds in the 
# FlowsheetIds_Obesity_Brief_HPI_PW.csv file.

# WPV_WMQ is defined as a WPV that is associated with one of two 
# FlowsheeTemplateEpicIds.


# Usage:
# data <- censor_visits(data) #where data should be the visits_post_id data 
# frame

# Use Arb_PersonId == 5891823092 to inspect the behavior of the function
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
censor_visits <- function(temp, version = NULL){
  
  if (version == 1){
  #1 . Censor any visits that break a sequence of control encounters followed
  # by intervention encounters. ie. control after intervention
  
  # Identifies any control phase encounters for which the previous encounter
  # was in which the intervention phase by subtracting the Intervention 
  # indicator from the lag variable after arranging all visits by EncounterDate
  # Any encounter for which the diff_lag_int variable is set to 1 indicates the
  # encounter is a control visit, but the previous encounter was an intervention
  # encounter. The date of the control visit is captured, joined into the main
  # data frame and then in the subsequent step any encounter that occurs  on or 
  # after that control visit is censored 
  # temp <- data
  # 
  censored_dates <-
    temp %>%
    group_by(Arb_PersonId) %>%
    arrange(EncounterDate) %>%
    mutate(lag = lag(Intervention),
           diff_lag_int = lag - Intervention) %>%
    filter(diff_lag_int == 1) %>%
    slice_head() %>%
    ungroup() %>%
    # select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexDate, IndexVisit, Intervention, Intervention.factor, lag, diff_lag_int) %>%
    select(Arb_PersonId, EncounterDate) %>%
    rename(censor_date = EncounterDate)
  
  temp %<>%
    left_join(., censored_dates, by = "Arb_PersonId") %>%
    mutate(Censored = if_else(EncounterDate >= censor_date, 1, 0)) %>%
    select(-censor_date)

  }
  
  if (version == 2){
  # Version 2 (06/2024) - per Mark Gritz. Once someone has Intervention index
  # visit, then their subsequent visits are intervention, even if the visit
  # takes place in a clinic that has not crossed over
  
  # This version adapts the previous algorithm to capture the date at which
  # a sequence from intervention to control occurs. However, instead of 
  # setting Censored == 1 for any visit after the censor date, the algorithm is
  # adapted to set Intervention to 1, then changing Intervention.factor to match
  # the numerical variable, finally chaning the Censored variable from 1 to 0
  
  # temp <- data

  # the censor_date indicates where a switch from intervention to control is
  # detected and should serve as the date in which subsequent control phase
  # visits can be converted to from control phase to intervention.
  censored_dates <-
    temp %>%
    group_by(Arb_PersonId) %>%
    arrange(EncounterDate) %>%
    mutate(lag = lag(Intervention),
           diff_lag_int = lag - Intervention) %>%
    filter(diff_lag_int == 1) %>%
    slice_head() %>%
    ungroup() %>%
    select(Arb_PersonId, EncounterDate) %>%
    rename(censor_date = EncounterDate)
    # select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexDate, Intervention, Intervention.factor, lag, diff_lag_int)

  # Join in the censored dates into the main input data frame
  temp %<>%
    left_join(., censored_dates, by = "Arb_PersonId")

  # Results in 1302 rows
  # Capture the rows where the visits are in Control, but are on or after the
  # censor date. These are visits where Intervention needs to be set to 1, where
  # Intervention.factor needs to be set to "Intervention", and where IndexDate
  # has to be set to NA, so that it can be modified to correspond to the new
  # phase.
  encounter_ids_to_modify <-
  temp %>%
    filter(Intervention == 0,
           EncounterDate >= censor_date) %>%
    pull(Arb_EncounterId)
    
  temp %<>% 
    # filter(Arb_PersonId == 163961167) %>%
    # select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexDate, IndexVisit, Intervention.factor, Intervention, censor_date) %>%
    mutate(Intervention.factor = if_else(Arb_EncounterId %in% encounter_ids_to_modify, "Intervention", Intervention.factor)) %>%
    mutate(IndexDate = if_else(Arb_EncounterId %in% encounter_ids_to_modify, NA, IndexDate),
           IndexVisit = if_else(Arb_EncounterId %in% encounter_ids_to_modify, NA, IndexVisit)) %>%
    group_by(Arb_PersonId, Intervention.factor) %>%
    arrange(Arb_PersonId, EncounterDate) %>% 
    fill(IndexDate) %>%
    fill(IndexVisit) %>%
    mutate(Intervention = ifelse(Intervention.factor == "Intervention", 1, 0)) %>%
    ungroup() %>%
    select(-censor_date)
    
    # Check to make sure no one has more than two index visits per phase

  # Since updating the censoring algorithm to convert control phase visits
  # to intervention, and adjust the index date, a censored variable does not get
  # created as was done in the first version. This code section assigns a 0
  # to all visits in a variable called "Censored." This variable will be needed
  # in step 2 below.
  temp %<>%
    mutate(Censored = 0)
  }

  
  # 2. Censor WPV_WMQ and WPV_PW_flow if they occur during control phases ------
  # PW_flow
  
  if ((temp %>% filter(WPV_PW_flow == 1, Intervention == 0) %>% nrow()) != 0){
    message("Detected encounters for which WPV_PW_flow occurred in the control phase! Censoring these patients...")
    
    pt_ids <- temp %>% filter(WPV_PW_flow == 1, Intervention == 0) %>% pull(Arb_PersonId)

    temp <- temp %>%
      mutate(Censored = ifelse((Arb_PersonId %in% pt_ids), 1, Censored))
  }
  
  # WMQ
  if ((temp %>% filter(WPV_WMQ == 1, Intervention == 0) %>% nrow()) != 0){
    message("Detected encounters for which WPV_WMQ occurred in the control phase! Censoring these patients...")
    
    pt_ids <- temp %>% filter(WPV_WMQ == 1, Intervention == 0) %>% pull(Arb_PersonId)

    temp <- temp %>%
      mutate(Censored = ifelse((Arb_PersonId %in% pt_ids), 1, Censored))
  }
  
  # Smart
  if ((temp %>% filter(WPV_smart == 1, Intervention == 0) %>% nrow()) != 0){
    message("Detected encounters for which WPV_Smart occurred in the control phase! Censoring these patients...")
    
    pt_ids <- temp %>% filter(WPV_WMQ == 1, Intervention == 0) %>% pull(Arb_PersonId)

    temp <- temp %>%
      mutate(Censored = ifelse((Arb_PersonId %in% pt_ids), 1, Censored))
  }
  
  # If version 1, Censored variable will contain NAs, which need to be converted
  # to 0s
  if (version == 1) {
  # Fix NAs
  temp %<>%
    mutate(Censored = ifelse(is.na(Censored), 0, Censored))
  }

  return(temp)
  
  }



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# OLD CODE DEPRECATED 3/14/2024
# # 1. Censor visits where intervention occurred before control ----
# ## Create a data frame of patients where encounter date == index date --------
# # Should result in at most 2 visits per patient, so that one is the index
# # for control and one is the index for intervention
# visits_on_id <- temp %>%
#   filter(EncounterDate == IndexDate,
#          IndexVisit == 1,
#          WPV > 0) %>%
#   select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexDate, IndexVisit,
#          Intervention, WPV)
# 
# ## QA Check: no more than 2 visits -------------------------------------------
# # Ensure that at most only two visits per patient are included in visits_on_id.
# # One for the control and one for intervention. An error here could mean that 
# # some patients had more than one WPV in one day.
# if (visits_on_id %>%
#     group_by(Arb_PersonId) %>%
#     count() %>%
#     pull(n) %>%
#     max() > 2){
#   stop("The max number of visits per patient in visits_on_id is greater than 2. Consider revising code!")
# }
# 
# # Counts a separate way that there are no more than 2 records per patient
# visits_on_id %>%
#   group_by(Arb_PersonId) %>%
#   count() %>%
#   filter(n > 2)
# 
# 
# ## QA Check: all visits are WPV ----------------------------------------------
# # Ensure that all encounters in visits_on_id are categorized as WPVs
# if (visits_on_id %>%
#     filter(WPV == 0) %>%
#     nrow() != 0){
#   stop("The data frame contains non WPVs! Consider revising code!")
# }
# 
# ## Create date subtraction function ------------------------------------------
# # Set the minus function which subtracts the 2nd element from the 1st element
# # in a column vector
# minus <- function(x) sum(x[1],na.rm=T) - sum(x[2],na.rm=T)
# 
# ## Count number of patients with intervention visits before control ----------
# # Subtracting intervention index date (position 1) from control index date 
# # (position 2). If value is negative (<0), then it means the intervention 
# # index date occurred before the control index date.
# visits_on_id %>%
#   group_by(Arb_PersonId) %>%
#   arrange(desc(Intervention)) %>% # descending order arranges Intervention == 1  first then Intervention == 0
#   summarise(value = minus(as.numeric(IndexDate))) %>% # subtract the date in position 2 from position 1 (1st row minus 2nd row)
#   filter(value < 0) %>% # if the value is negative, then it means the intervention visit occurred before control visit.
#   count()
# 
# ## Create censored_ids (Arb_PersonIds) ---------------------------------------
# # *** Deprecated Older version, replaced with newer version
# # Save the patient ids where the intervention visit occurred before the 
# # control visit. Arranges the index visits by intervention so that the control
# # index visit is above the intervention visit. Then the control visit date
# # is subtracted from the intervention visit date. If the value is less than
# # 0 then it means that the intervention index visit occurs before the 
# # control index visit. However, this algorithm is limited to index visits
# # only and does not account for the the non-index visits. 
# censored_ids <- visits_on_id %>%
#   group_by(Arb_PersonId) %>%
#   arrange(desc(Intervention)) %>%
#   summarise(value = minus(as.numeric(IndexDate))) %>%
#   filter(value < 0) %>%
#   pull(Arb_PersonId)
# ## Create censored_visits (Arb_EncounterIds) ---------------------------------
# # Use the patient ids to get the encounter ids of the control visits
# # censored_visits <- temp %>%
# #   filter(Arb_PersonId %in% censored_ids,
# #          Intervention == 0) %>%
# #   pull(Arb_EncounterId)
# 
# ## Censor Arb_EncounterIds ---------------------------------------------------
# temp <- temp %>%
#   mutate(Censored = ifelse((Arb_PersonId %in% censored_ids), 1, 0))
# 
# # 2nd iteration that would not have sufficed for the analytic needs ------------
# censored_ids <-
#   temp %>%
#   # filter(Censored == 0) %>% # should be commented out to cast a wider net and replace the censored_ids chunk above
#   # select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexVisitEligible, Weight_lbs, BMI, GroupID, Intervention.factor, Intervention, WPV_CC:IndexVisit) %>%
#   group_by(Arb_PersonId) %>%
#   arrange(EncounterDate) %>%
#   mutate(lag = lag(Intervention),
#          diff_lag_int = lag - Intervention) %>%
#   filter(diff_lag_int == 1) %>%
#   distinct(Arb_PersonId) %>%
#   pull(Arb_PersonId)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%