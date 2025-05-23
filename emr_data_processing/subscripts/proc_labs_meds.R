# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. Dept. of Fam. Med. CU Anschutz Medical Campus
# 02-02-2024
# Get labs, procedures, and medications

# Description:
# This script was designed to capture labs, procedures, and medications. This
# script takes in the input and splits the data into 4 different sub data frames
# that include the index visits in control and in intervention, and the last
# visits in control and intervention.

# The purpose of breaking apart the data, is to use the same core set of 
# functions on different sub sets of data where only the reference dates to link
# the additional variables to has been changed. This function also procesess the
# 4 subsets in parallel to speed up processing.


# Dependencies:
# labs_procedures.R
# medications.R
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


proc_labs_meds <- function(data) {
  # Load functions -------------------------------------------------------------
  source(str_c(emr_dir, "subscripts/labs_procedures.R"))
  # The older version of meds, captures the frequencies of each AOM, whereas
  # the newer one is just a binary whether or not patient had AOM Rx within
  # phase, and counts of Rx
  
  if (data_delivery_date == 20240326) {
    source(str_c(emr_dir, "subscripts/medications_old.R")) # Older version of meds
    # There is also an intermediary version medications.R, that is somewhat an 
    # in-between the old version and the latest version (capture_medications.R)
  } else {
    source(str_c(emr_dir, "subscripts/capture_medications.R")) # Latest version
  }
  
  # Create data subsets --------------------------------------------------------
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

  # remove the individual data frames to conserve RAM
  rm(ind_con, lv_con, ind_int, lv_int)

  # Define a function to process the data
  proc_data <- function(temp) {
    # Load the compass tables required to perform the following processing
    # steps. Read_pw_csv is a custom function that loads the raw compass table
    # based on the file names
    purrr::walk(.x = c("procedure", "labs", "flowsheets", "referrals", "meds"),
                .f = read_pw_csv)

    temp <- labs_procedures(temp)

    temp <- capture_medications(temp)

    return(temp)
  }

  invisible(gc())

  # Apply the proc data function
  # tic()
  df_list <-
    c(1, 2, 3, 4) %>%
    future_map(~ df_list[[.x]] %>% proc_data(.))
  # toc()
  # beepr::beep(sound = 2)

  # Clean up data before stitching ---------------------------------------------
  # Since the labs_procedures(), capture_medications() functions require an
  # index date, lv data subsets that do not have an index visit are set to 1
  # and the index date is set to the encounter date to "trick" the function into
  # working correctly. This chunk of code reverts the IndexVisit and IndexDate
  # columns back to their original state. Only needed for lv_con and lv_int sets
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
  # visits_post_id data frame
  if (bind_rows(linked_visits, non_linked_visits) %>% nrow() == dim(data)[1]) {
    temp_data <- bind_rows(linked_visits, non_linked_visits)
  } else {
    stop("The number of modified output visits does not equal the number of input visits!!! Review and revise code.") # nolint: line_length_linter.
  }

  temp_data %<>%
    group_by(Arb_PersonId, Intervention.factor) %>%
    arrange(desc(Intervention)) %>%
    fill(names(. %>% select(O2CPAPBIPAP:Ref_WellnessClinic)),
         .direction = "down") %>%
    ungroup() %>%
    select(-Cohort_end_date)

  rm(df_list)

  return(temp_data)
}