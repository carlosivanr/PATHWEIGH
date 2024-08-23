# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, Ph.D. CU Anschutz Dept. of Family Medicine
# 02/02/2024
# 08/09/2024: Major updates to store the created columns resulting from the 
#             proc_ee_ene() function. To avoid having to re-process the data
#             when changes to data handling are made, the script now merges in
#             the necessary columns from the stored data.

# process ee and ene (eligible and enrolled and eligible not enrolled data)
# This script was created to process the ENE and EE data in the same way
# to obtain the same outcome variables for any potential comparisons

# This script will do the following
# 1. Create NewId variable which is a concatenation of the Arb_PersonId and the
#     intervention variable
# 2. Create N_days after the IndexVisit
# 3. Create N_months after the IndexVisit
# 4. Create Year and Quarter variables
# 5. Create Censor variable
# 6. Create the LastVisit indicator variable
# 7. Create N_months after the last visit
# 8. Capture the labs, medications and EOSS at the index and last visits in 
#     each phase
# 9. Capture comorbidities at the index and last visits in each phase
# 10. Create Weight_kgs variable


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%% DECLARE PROC_EE_ENE FUNCTION %%%%%%%%%%%%%%%%%%%%%%%%%%%
# ee_ene <- proc_ee_ene(ee_ene, proc_labs = TRUE, proc_dx = TRUE)
proc_ee_ene <- function(data, proc_labs = TRUE, proc_dx = TRUE){  
  tic()
  # Create a NewID for each Patient*Intervention combination ---------------------
  # NewId is made by concatenating the person id and the intervention variable
  # data$NewId <- str_c(
  #   data$Arb_PersonId, "_", data$Intervention)
  
  # Censor visits ----------------------------------------------------------------
  # Censors visits in which in which the intervention WPV occurred before the
  # control visit or where the PW_flow of WMQ were used in the control phase
  # Censoring was previously downstream, but had to be moved up because the
  # index date of control visits that occurred after the intervention index had
  # to be modified. Placing the algorithm here will ensure that these visits
  # have the appropriate N_days/N_months after the index date set.
  data <- censor_visits(data, version = 1)
  
  # Days since the Index Visit  --------------------------------------------------
  data %<>%
    mutate(N_days_post_id = as.numeric(EncounterDate - IndexDate))
  
  
  # Months since Index Visit --------------------------------------------------
  # test <- data %>%
  #   select(N_days_post_id, EncounterDate, IndexDate, IndexVisit) %>%
  #   filter(N_days_post_id == 16) %>%
  #   slice_sample(n = 100) %>%
  #   mutate(
  #          # diff_btw_dates = EncounterDate - IndexDate,
  #          # round_N_over30 = round(N_days_post_id/30.417, digits = 0),
  #          # cut_N_days = cut(N_days_post_id,
  #          #                  breaks = seq(0, max(data$N_days_post_id), by = 30)),
  #          # monnb = as.POSIXlt(as.Date(EncounterDate, origin = IndexDate)),
  #          interval = lubridate::interval(IndexDate, EncounterDate),
  #          interv_div_by_mon = lubridate::interval(IndexDate, EncounterDate) %/% months(1)
  #          )
  #
  
  # Create a lubridate interval value and then perform integer division by
  # one month as a date value. Integer division %/% is the opposite of the
  # modulo %% which gives the remainder
  data %<>%
    mutate(N_months_post_id = lubridate::interval(IndexDate, EncounterDate) %/% months(1))
  
  
  # Set Year_Quarter relative to study start date --------------------------------
  # Year 0 March 17, 2020 - March 16, 2021, Baseline
  # Year 1 March 17, 2021 - March 16, 2022, Group 1 crosses over to intervention
  # Year 2 March 17, 2022 - March 16, 2023, Group 2 crosses over to intervention
  # Year 3 March 17, 2023 - March 16, 2024, Group 3 crosses over to intervention
  
  # Quarter is set relative to the study start date 3-17-2020
  # Quarter 1 March 17 - June 16
  # Quarter 2 June 17 - Sept 16
  # Quarter 3 Sept 17 - Dec 16
  # Quarter 4 Dec 17 - March 16
  
  data %<>%
    mutate(Year = case_when(
      EncounterDate < "2021-03-17" ~ "Year0",
      EncounterDate >= "2021-03-17" & EncounterDate <"2022-03-17" ~ "Year1",
      EncounterDate >= "2022-03-17" & EncounterDate <"2023-03-17" ~ "Year2",
      EncounterDate >= "2023-03-17" & EncounterDate <"2024-03-17" ~ "Year3"),
      Month_Day = format(EncounterDate, "%m-%d"),
      Quarter = case_when(
        Month_Day >= "03-17" & Month_Day < "06-17" ~ "Q1",
        Month_Day >= "06-17" & Month_Day < "09-17" ~ "Q2",
        Month_Day >= "09-17" & Month_Day < "12-17" ~ "Q3",
        Month_Day >= "12-17" | Month_Day < "03-17" ~ "Q4"),
      Year_Quarter = str_c(Year, "_", Quarter)) %>%
    select(-Month_Day, -Quarter)
  
  # Set Last visit and  Last visit w/ weight -------------------------------------
  # Set the last visit for each patient in each phase with and without weight
  # n.b. Creates two "LastVisit" columns, one for the visit with weight and one
  # for any visit to capture labs, procedures, medications, etc.
  data <- set_last_visit(data)
  
  
  # Categorize N_months_post_lv --------------------------------------------------
  # n.b. will only calculate for observations that are the last visit
  # could try a group_by(Arb_PersonId, Intervention) %>% fill() if those values
  # are needed.
  data %<>%
    mutate(N_months_post_lv = ifelse(LastVisit == 1,
                                     round(as.numeric(EncounterDate - IndexDate)/30,digits = 0),
                                     NA)) %>%
    mutate(N_months_post_lv_cat = case_when(
      N_months_post_lv < 7 ~ "0-6",
      N_months_post_lv >= 7 & N_months_post_lv < 12 ~ "7-12",
      N_months_post_lv >= 13 & N_months_post_lv < 18 ~ "13-18",
      N_months_post_lv > 18 ~ "18+"))   # %>%             # uncomment here to fill
  # group_by(Arb_PersonId, Intervention) %>%            # uncomment here to fill
  # fill(N_months_post_lv_cat, .direction = "up") %>%   # uncomment here to fill
  # ungroup()                                           # uncomment here to fill
  
  
  # Labs, Medications, EOSS ------------------------------------------------------
  # Labs, medications, EOSS are captured at the index visit and last visit for
  # each patient in each phase. Since the same strategy of breaking apart the
  # main data frame and capturing these values applies to the subsets of the
  # data frame, this script applies all three functions to each
  # subset of data before binding them back together instead of breaking apart,
  # function, binding 3x, for labs, then for meds, then for eoss.
  # Replaced with lapply() but it
  # takes a longer to process, may need a re-write.
  if (proc_labs == TRUE){
    
    purrr::map(
        .x  = c("procedure", "labs", "flowsheets", "referrals", "meds", "dxco"),
        .f = read_pw_csv
        ) 
    
    source(str_c(emr_dir, "subscripts/proc_labs_meds_eoss.R"))
    
    # source(str_c(emr_dir, "subscripts/labs_meds_eoss.R"))
    # tic()
    data <- proc_labs_meds_eoss(data)
    # toc()
    # beepr::beep(sound = 1)
  }
  
  # Comorbidities ----------------------------------------------------------------
  if (proc_dx == TRUE){
    
    # Load dxco into the workspace if it's not already loaded
    dfs <- c("dxco")  
    if (sum(map_lgl(dfs, exists)) != length(dfs)){
      purrr::map(
        .x  = c("dxco"),
        .f = read_pw_csv
      )
    }
    
    source(str_c(emr_dir, "subscripts/proc_comorbidities.R"))
    
    # source(str_c(emr_dir, "subscripts/comorbidities.R"))
    data <- proc_comorbidities(data)
  }
  
  # Modify the weight variable ---------------------------------------------------
  # Modifies the weight variable in the input data frame so that the
  # weight value captured at the baseline index visit is set to be used as a
  # covariate and the weight values from non-index visits are set to be used as
  # dependent variables.
  # data %>%
  #   filter(IndexVisit == 1) %>%
  #   filter(is.na(Weight)) %>%
  #   nrow()
  
  data %<>%
    # select(IndexVisit, Weight_kgs) %>%
    mutate(Weight_bl = ifelse(IndexVisit == 1, Weight_kgs, NA),
           Weight_dv = ifelse(IndexVisit != 1, Weight_kgs, NA))
  
  # Fill the NA Weight_bl values with the available value at the index visit
  # in each intervention phase
  data %<>%
    group_by(Arb_PersonId, Intervention) %>%
    arrange(Intervention, desc(IndexVisit), EncounterDate) %>%
    fill(Weight_bl, .direction = "down") %>%
    ungroup()
  
  
  # Create Any_PW_Visit variable ------------------------------------------------
  # ***> Needs additional development CR 07/09/2024
  # Current version only specifies whether or not the visit was a PW visit.
  data %<>% set_any_pw_visit(.)
  
  
  return(data)
  
  invisible(gc())
  
}
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%% DECLARE RDATA-LOADING FUNCTION %%%%%%%%%%%%%%%%%%%%%%%%%%%
# File loading function that will allow loading a file and assign it to a 
# different name to in the global environment
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# %%%%%%%%%%%%%%% TEST TO DETERMINE IF PROCESSED DATA EXISTS %%%%%%%%%%%%%%%%%%%
# If exists, load data
#   Then check if any records need processing, if so process separte into 
#     subsets. For those that do not need processing, simply merge in the labs
#     procedures, and comorbidities. For those that need proceessing, feed into 
#     proc_ee_ene(). Finally, stack the two subsets
#   if not then just merge in labs, procedures, and comorbidities
# If data doesn't exist, then process the input

if(file.exists(here(str_c("delivery_", data_delivery_date),"data", paste0("proc_ee_ene_", RData)))){
  # Load the processed ee_ene, that already has labs, procedures, and
  # comorbidities to avoid capturing them again when the script is run
  processed_ee_ene <- loadRData(here(str_c("delivery_", data_delivery_date), "data", paste0("proc_ee_ene_", RData)))
  
  # If all of the encounters in ee_ene are in processed ee_ene, then just merge
  # in the labs, procedures, comorbidities, etcs
  if(all.equal(sort(ee_ene$Arb_EncounterId), sort(processed_ee_ene$Arb_EncounterId))){
    ee_ene <- left_join(ee_ene, processed_ee_ene, by = c("Arb_PersonId", "Arb_EncounterId", "EncounterDate"))
    rm(processed_ee_ene)
  } else {
    stop("Not all visits in the input data frame match data that was previously processed. Review code.")
  }
    
  # } else {
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Written to attempt to use a previous data deliveries, comorbidities, labs
    # procedures, etc, to merge in, but abandoned, to avoid further time spent
    # on troubleshooting and development.
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # # If not all encounters are represented in both data frames, then
    # # Get the encounters that can be merged
    # 
    # # processed encounters
    # subset_proc <- ee_ene %>%
    #   filter(Arb_EncounterId %in% processed_ee_ene$Arb_EncounterId)
    # 
    # # These just get merged in because they already have processed data
    # subset_proc <- left_join(subset_proc, 
    #                          (processed_ee_ene %>%
    #                             filter(Arb_EncounterId %in% ee_ene$Arb_EncounterId)),
    #                          by = "Arb_EncounterId")
    # 
    # # unprocessed encounters
    # subset_unproc <- ee_ene %>%
    #   filter(!Arb_EncounterId %in% processed_ee_ene$Arb_EncounterId)
    # 
    # 
    # subset_unproc <- proc_ee_ene(subset_unproc, proc_labs = TRUE, proc_dx = TRUE)
    # 
    # # stack
    # ee_ene <-  bind_rows(subset_proc, subset_unproc)
    # 
    # # save processed ee_ene added columns only
    # processed_ee_ene <- 
    #   ee_ene %>%
    #   select(Arb_PersonId, Arb_EncounterId, EncounterDate,
    #          # These are the column names in ee_ene not in visits
    #          all_of(names(ee_ene)[!names(ee_ene) %in% names(visits)]))
    # 
    # # save processed ee_ene
    # save(processed_ee_ene, file = here("data", paste0("proc_ee_ene_", RData)))
    # 
    # # remove processed ee_ene from work space
    # rm(processed_ee_ene, dx, dxco, flowsheets, labs, meds, procedure, referrals)
    # 
  # }
  
  } else {
    ee_ene <- proc_ee_ene(ee_ene, proc_labs = TRUE, proc_dx = TRUE)
    
    # save processed ee_ene added columns only
    processed_ee_ene <- 
      ee_ene %>%
      select(Arb_PersonId, Arb_EncounterId, EncounterDate,
             # These are the column names in ee_ene not in visits
             all_of(names(ee_ene)[!names(ee_ene) %in% names(visits)]))
    
    # save processed ee_ene
    save(processed_ee_ene, file = here(str_c("delivery_", data_delivery_date), "data", paste0("proc_ee_ene_", RData)))
    
    # remove processed ee_ene from work space
    rm(processed_ee_ene, dx, dxco, flowsheets, labs, meds, procedure, referrals)

  }
  





# ------------------------------------------------------------------------------
# processed_ee_ene %<>%
#   filter(Arb_EncounterId %in% ee_ene$Arb_EncounterId) %>%
#   select(Arb_PersonId, Arb_EncounterId, EncounterDate,
#          # These are the column names in processed_ee_ene not in ee_ene
#          all_of(names(processed_ee_ene)[!names(processed_ee_ene) %in% names(ee_ene)]),
#          -NewId)

# save(processed_ee_ene, file = here("data", paste0("proc_ee_ene_", RData)))
# Could also try
# saveRDS() and readRDS()
# ------------------------------------------------------------------------------

