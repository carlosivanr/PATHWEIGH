#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# 02/02/2024
# Capture comorbidities at index and last visits as function

# The comorbidities.R script loads a function that was designed to capture 
# the comorbidities for each patient that with the restriction that the comor-
# bidity appeared at least 2 times in the medical record within a span of 2 
# years prior to the baseline index date. 

# Since interest was developed in looking at comorbidities at the last visit
# this script relies on the get_comorbidities() function to capture the 
# comorbidities at the index visit in control, intervention and at the last 
# visit in control or intervention. The last visit is the last non-index visit
# in each phase control or intervention.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


proc_comorbidities <- function(data){
  # Load the capture_comorbidities function ------------------------------------
  source(str_c(emr_dir, "subscripts/capture_comorbidities.R"))

  # Create separate subsets of data to capture comorbidities -------------------
  ## 1. index visits in control phase ----
  # Can be processed normally without modifying the index date
  # For some individuals, the index visit and the last visit are the same visit
  ind_con <- bind_rows(
    (data %>%
       filter(Censored == 0,
              IndexVisit == 1,
              Intervention == 0,
              LastVisit == 1)),
    (data %>%
       filter(Censored == 0,
              IndexVisit == 1,
              Intervention == 0,
              LastVisit == 0))
  )
  
  ## 2. last visits in control phase ----
  # Needs a temporary modification of the index date to trick the algorithm into
  # using the encounter date instead of the index date
  lv_con <- data %>%
    filter(Censored == 0,
           IndexVisit == 0,
           Intervention == 0,
           LastVisit == 1) %>%
    mutate(IndexDate_backup = IndexDate,
           IndexDate = EncounterDate,
           IndexVisit = 1)
  
  ## 3. index visits in intervention phase ----
  # Can be processed normally without modifying the index date
  ind_int <- bind_rows(
    (data %>%
       filter(Censored == 0,
              IndexVisit == 1,
              Intervention == 1,
              LastVisit == 1)),
    (data %>%
       filter(Censored == 0,
              IndexVisit == 1,
              Intervention == 1,
              LastVisit == 0))
  )
  
  ## 4. last visits in intervention phase ----
  # Needs a temporary modification of the index date to trick the algorithm into
  # using the encounter date instead of the index date
  lv_int <- data %>%
    filter(Censored == 0,
           IndexVisit == 0,
           Intervention == 1,
           LastVisit == 1) %>%
    mutate(IndexDate_backup = IndexDate,
           IndexDate = EncounterDate,
           IndexVisit = 1)
  
  # Put dfs into a list and then process ---------------------------------------
  df_list <- list(ind_con, lv_con, ind_int, lv_int)
  
  # Remove variables from workspace.
  rm(ind_con, ind_int, lv_con, lv_int)
  
  invisible(gc())
  
  # Apply the capture_comorbidities function to each data set in df_list
  # Set to find comorbidities that are observed at least 2 times within the last 
  # 2 years
  # df_list <- lapply(df_list, capture_comorbidities, 2, 2)
  # Apply the proc data function
  tic()
  df_list <-
    c(1, 2, 3, 4) %>%
    future_map(~ df_list[[.x]] %>% capture_comorbidities(., 2, 2))
  toc()
  beepr::beep(sound = 2)
  
  # %%%%%%%%%%%%%%%%% REGION FROM PROC COMORBIDITIES %%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Clean up data before stitching ---------------------------------------------
  # Since the labs_procedures(), capture_medications(), and eoss() functions
  # require an index date, data subsets that do not have an index visit are set
  # to 1 and the index date is set to the encounter date to "trick" the function
  # into working correctly. This chunk of code reverts the IndexVisit and
  # IndexDate columns back to their original state. Only needed for lv_con and
  # lv_int subsets.
  clean_data <- function(temp) {
    # If the data frame contains an IndexDate_backup column, then modify
    if (sum(grepl("IndexDate_backup", names(temp))) == 1) {
      temp %<>%
        mutate(IndexDate = IndexDate_backup,
               IndexVisit = 0) %>%
        select(-IndexDate_backup)
    }
    return(temp)
  }
  
  # Apply the clean_data function to the list of data frames
  df_list <- map(df_list, clean_data)
  
  # Recreate visits_post_id ----------------------------------------------------
  # This vector represents the index visits and last visit encounter ids used
  # to capture the variables. Used to create a data frame that does not contain
  # those encounter ids
  linked_visit_ids <-
    bind_rows(df_list) %>%
    pull(Arb_EncounterId)
  
  # Filter out visits that were processed from visits_post_id
  non_linked_visits <-
    data %>%
    filter(!Arb_EncounterId %in% linked_visit_ids)
  
  # Bind the subsets of index and last visits that were processed
  linked_visits <- bind_rows(df_list)
  
  # Check to ensure that no new rows were added before replacing visits_post_id
  # If no new rows, then bind the subsets together and save as a new 
  # data frame
  if (bind_rows(linked_visits, non_linked_visits) %>% nrow() == dim(data)[1]) {
    temp_data <- bind_rows(linked_visits, non_linked_visits)
  } else {
    stop("The number of modified output visits does not equal the number of input visits!!! Review and revise code.")
  }
  
  # temp_data %<>%
  #   group_by(Arb_PersonId, Intervention.factor) %>%
  #   arrange(desc(Intervention)) %>%
  #   fill(names(. %>% select(O2CPAPBIPAP:Ref_WellnessClinic)),
  #        .direction = "down") %>%
  #   ungroup() #%>%
  #   # select(-Cohort_end_date)
  
  rm(df_list)
  
  # %%%%%%%%%%%%%%%%%%%%%%%%% END REGION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
  # Fix the NAs ------------------------------------------------------------------
  # Get the name of the first comorbidity
  start <- names(temp_data[,dim(data)[2]+1])
  stop <- names(temp_data[,dim(temp_data)[2]])
  
  temp_data %<>% 
    mutate(across(sym(start):sym(stop), ~ ifelse(is.na(.), 0,.)))
  
  # Arrange column names by percentage -----------------------------------------
  comorbidity_names <- names(
    temp_data %>% 
      select(sym(start):sym(stop)))
  
  # Create an index of the order of comorbidity column names arranged by the 
  # sum of the column's values. This index is used to order the columns by 
  # frequency for displaying in a table
  index <- 
    temp_data %>% 
    select(all_of(comorbidity_names)) %>%
    colSums() %>%
    order(decreasing = TRUE)
  
  # Reorder comorbidity names
  comorbidity_names <- comorbidity_names[index]
  
  # Save comorbidity names to data directory and assign to global environment
  comorbidity_names <<- comorbidity_names
  saveRDS(comorbidity_names, 
          file = here(proj_root_dir, 
                      "data", 
                      str_c("comorbidity_names_", data_delivery_date, ".RDS")))
  
  
  # Rearrange the columns so that the cormorbidities are ordered from most 
  # frequently observed comorbidities that are observed at least 2x within the 2
  # years from the index/last visit to the least frequently observed comorbidities.
  temp_data %<>%
    select(Arb_PersonId:EOSS,
           all_of(comorbidity_names))
  
  # Remove intermediary data frames and memory resources -----------------------
  invisible(gc())
  
  return(temp_data)
  
}
