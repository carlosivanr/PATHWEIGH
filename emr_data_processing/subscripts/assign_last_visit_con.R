# Assign Last Visit in Control %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# For patients that are enrolled in the control phase, if the first visit in the
# intervention phase has a weight and is a non-index visit, then switch the 
# phase of that visit to control by setting Intervention.factor to "Control" and
# Intervention to 0.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assign_last_visit_con <- function(data){
  
  data %>%
    pull(Intervention.factor) %>%
    table()
  
  # Get the patient ids of those enrolled in control only
  # We only need to switch the phase of visits of those enrolled in control
  con_ids <- 
    data %>% 
    filter(EncounterDate > IndexDate,
           Intervention == 0,
           Enrolled == 1) %>%
    distinct(Arb_PersonId) %>%
    pull(Arb_PersonId)
  
  # Capture the encounter ids of patients in con_ids, but that are in the 
  # intervention phase and occur before the intervention index date. Then 
  # arrange their data by Arb_PersonId and EncounterDate. Then slice the first
  # row after grouping by Arb_PersonId. Finally drop any rows where the first
  # visit in the intervention phase has a missing weight value and extract the
  # Arb_EncounterIds. These will be the Arb_EncounterIds where we want to change
  # from intervention to control
  ids_to_change <- 
    data %>%
    filter(Arb_PersonId %in% con_ids,
           EncounterDate < IndexDate,
           Intervention == 1) %>%
    arrange(Arb_PersonId,EncounterDate) %>%
    group_by(Arb_PersonId) %>%
    slice_head() %>%
    ungroup() %>%
    drop_na(Weight_kgs) %>%
    pull(Arb_EncounterId)
  
  # For the the Arb_EncounterIds in ids_to_change, switch the phase from 
  # intervention to control, otherwise do nothing for those Arb_EncounterIds
  # that are not in ids_to_change. Switch both intervention.factor and 
  # intervention variables since both are used in the pipeline.
  data2 <- 
    data %>%
    mutate(Intervention.factor = if_else(Arb_EncounterId %in% ids_to_change, "Control", Intervention.factor),
           Intervention = if_else(Arb_EncounterId %in% ids_to_change, 0, Intervention),
           Enrolled = if_else(Arb_EncounterId %in% ids_to_change, 1, Enrolled),
           IndexDate = if_else(Arb_EncounterId %in% ids_to_change, NA, IndexDate)) %>%
    arrange(Arb_PersonId, Intervention.factor, EncounterDate) %>%
    group_by(Arb_PersonId, Intervention.factor) %>%
    fill(IndexDate) %>%
    ungroup

  
  # # ----------------------------------------------------------------------------
  # df_list <-  list(data, data2)
  # 
  # # Does the number of patients change between data2 and data
  # df_list %>%
  #   purrr::map(~ .x %>%
  #   filter(EncounterDate >= IndexDate,
  #          Enrolled == 1,
  #          IndexVisit == 1) %>%
  #   group_by(Intervention.factor) %>%
  #   count() )
  # 
  # # Does the number of rows in each phase change?
  # df_list %>%
  #   purrr::map(~ .x %>%
  #                filter(EncounterDate >= IndexDate,
  #                       Enrolled == 1) %>%
  #                group_by(Intervention.factor) %>%
  #                count() )
  # 
  # # ----------------------------------------------------------------------------
  # data %>%
  #   filter(Arb_PersonId %in% con_ids) %>%
  #   filter(EncounterDate < IndexDate) %>%
  #   filter(Intervention.factor == "Intervention") %>%
  #   select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexDate, IndexVisit,
  #          Intervention.factor, Enrolled, Weight_kgs) %>%
  #   arrange(Arb_PersonId, EncounterDate) %>%
  #   group_by(Arb_PersonId) %>%
  #   slice_head() %>%
  #   ungroup() %>%
  #   drop_na(Weight_kgs)
  # 
  # # Make a test data frame
  # test <- data %>%
  #   filter(Arb_PersonId == 25240011) %>%
  #   arrange(EncounterDate) %>%
  #   select(Arb_PersonId, Arb_EncounterId, EncounterDate, ProviderNpi,
  #          IndexVisitEligible, IndexVisit, IndexDate, Intervention.factor,
  #          Intervention,
  #          Eligible, WPV, Enrolled, Weight_kgs, BMI, Age) %>%
  #   mutate(Intervention.factor = if_else(Arb_EncounterId %in% ids_to_change, "Control", Intervention.factor),
  #          Intervention = if_else(Arb_EncounterId %in% ids_to_change, 0, Intervention),
  #          Enrolled = if_else(Arb_EncounterId %in% ids_to_change, 1, Enrolled),
  #          IndexDate = if_else(Arb_EncounterId %in% ids_to_change, NA, IndexDate)) %>%
  #   group_by(Arb_PersonId, Intervention.factor) %>%
  #   fill(IndexDate) %>%
  #   ungroup()
  # 
  # # ----------------------------------------------------------------------------
  
  return(data2)
  
}
