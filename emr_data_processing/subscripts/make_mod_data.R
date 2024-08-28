# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# 03/12/2024
# Make modeling data
# mod_data is the data frame for the modeling of the primary PATHWEIGH aim 

# Inputs:
# input is ee_ene data set to be able to also compare ee vs ene in modeling

# Dependencies:
# requires visits_post_id data set as a dependency because it contains all of
# the labs, procedures, meds, and vitals captured at the index visit in each 
# phase
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



make_mod_data <- function(data, delivery){
  
  # Initial data filtering -----------------------------------------------------
  # Remove any patient where sex is unknown because the model could fail due to
  # sparseness, and filters out anyone who was censored
  data %<>% 
    filter(Sex != "Unknown") %>%
    filter(Censored == 0)
  
  # Create separate intervention and control data frames -----------------------
  # In order to capture change in weight, each patient must have at least 2
  # visits in each of the control and intervention phases
  # Uses two separate vectors of patient ids so that ee_ene can be filtered 
  # in each separate phase, otherwise using just a patient id can pull in visits
  # from any phase where 2 or more visits in a phase
  
  # capture the patient ids for those that have more than 1 visit in the control
  # phase to use in the modelling
  con_ids_gt1_visits <-
    data %>%
    filter(Intervention == 0) %>%
    group_by(Arb_PersonId) %>%
    count() %>%
    filter(n > 1) %>%
    pull(Arb_PersonId)
  
  
  # capture the patient ids for those that have more than 1 visit in the intervention
  # phase to use in the modelling
  int_ids_gt1_visits <-
    data %>%
    filter(Intervention == 1) %>%
    group_by(Arb_PersonId) %>%
    count() %>%
    filter(n > 1) %>%
    pull(Arb_PersonId)
  
  
  # Declare data preparation function ------------------------------------------
  # prep_data is declared so that it can be applied to two separate data sets
  # which are then stacked together
  prep_data <-  function(data){
    
    # Filter rows and select the columns necessary for modeling
    data %<>%
      select(Arb_PersonId,
             Arb_EncounterId,
             EncounterDate,
             DepartmentExternalName,
             DepartmentEpicId,
             IndexVisit,
             WPV,
             Weight_kgs,
             Height_cm,
             BMI,
             Age,
             Sex,
             Race_Ethnicity,
             Insurance,
             Smoking_Status,
             Year,
             Cohort,
             N_months_post_id,
             N_days_post_id,
             Intervention.factor,
             EE,
             starts_with("WPV_"),
             PW_Visit) %>% 
      
      # The WPV variable is initially a sum of the ways in which the encounter
      # met criteria for WPV. Turn WPV from count to a binary indicator.
      mutate(WPV = ifelse(WPV > 0, 1, 0)) %>%
      
      # Calculate a cumulative sum for the WPV binary indicator to capture the
      # number of WPVs per patient
      group_by(Arb_PersonId) %>%
      arrange(EncounterDate) %>%
      mutate(N_prior_WPVs = cumsum(WPV)) %>%
      ungroup() %>%
      
      # For those that have a value of greater than 1, subtract 1 from  to 
      # account for the baseline index visit for the EE folks. At minimum, all 
      # rows have at least 1 prior visit, which should be the requirement since 
      # the data are filtered to those that have more than one visit.
      mutate(N_prior_WPVs = ifelse(N_prior_WPVs > 1, N_prior_WPVs - 1, N_prior_WPVs)) %>%
      
      # Create weight baseline and weight dependent variables and Year at index 
      # visit variable. If the visits is not an index visit for weight baseline
      # the value is set to NA so that it can be filled in later. Similarly, for
      # Year_at_ind, if it's not the index visit, the value will be set to NA 
      # for the same reason. Weight_dv is copied from Weight_kgs from 
      mutate(Weight_bl = ifelse(IndexVisit == 1, Weight_kgs, NA),
             Weight_dv = Weight_kgs,
             Year_at_ind = ifelse(IndexVisit == 1, Year, NA)) %>%
      
      # Convert Weight_bl and Year_at_ind to time invariant variables
      group_by(Arb_PersonId) %>%
      fill(Weight_bl, .direction = "updown") %>%
      fill(Year_at_ind, .direction = "updown") %>%
      ungroup() %>%
      
      # Remove the index visits from the data set 
      # modified 08/28/2024 to include the index visits
      # filter(IndexVisit == 0) %>%
      
      # Remove any rows for which weight data were not collected
      drop_na(Weight_dv) %>%
    
      # Factor Arb Person Id (for repeated measures)
      mutate(Arb_PersonId = factor(Arb_PersonId)) %>%
      
      # Create a squared N_days_post_id variable for quadratic trends
      mutate(N_days_post_id_sq = N_days_post_id^2) %>%
      
      # Categorize Age
      mutate(Age_cat = ifelse(Age <= 45, "<=45", NA),
             Age_cat = ifelse(Age > 45 & Age <= 60, "45-60", Age_cat),
             Age_cat = ifelse(Age > 60, ">60", Age_cat)) %>%
      
      # Set the reference categories
      mutate(Sex = relevel(factor(Sex), ref = "Female"),
             Age_cat = relevel(factor(Age_cat), ref = "45-60"),
             Race_Ethnicity = relevel(Race_Ethnicity, ref = "Non-Hispanic White"),
             Intervention = Intervention.factor) %>%
      
      select(-Intervention.factor) 
      # %>%
      # 
      # # Filter observations to 18 months (540 days)
      # filter(N_days_post_id <= 540)
    
    # Output of the function
    return(data)
  }
  
  
  # Prep the data frames -------------------------------------------------------
  # Create the control data
  mod_data_con <- 
    data %>%
    filter(Intervention == 0,
           Censored == 0,
           Arb_PersonId %in% con_ids_gt1_visits) %>%
    prep_data()
  

  # Create the intervention data
  mod_data_int <- 
    data %>%
    filter(Intervention == 1,
           Censored == 0,
           Arb_PersonId %in% int_ids_gt1_visits) %>%
    prep_data()
  
  
  # Stack the two data frames together
  mod_data <- bind_rows(mod_data_con, mod_data_int)
  
  
  # Set reference for year at index
  # reference set here since no encounter in intervention will have Year0 as year at index
  mod_data %<>%
    mutate(Year_at_ind = relevel(factor(Year_at_ind), ref = "Year0"))
  
  # Create number of days after index and after 6 months
  mod_data %<>%
    # N_days_post_id less than (lt) 180 (6 months)
    mutate(bin_N_days_post_id_gt_180 = ifelse(N_days_post_id > 180, 1, 0),
           N_days_post_180 = bin_N_days_post_id_gt_180 * (N_days_post_id - 180))
  
  # create a binary variable of those greater than 30 bmi
  mod_data %<>%
    mutate(bmi_gt_30 = ifelse(BMI >= 30, 1, 0))
  
  # Capture the age category and bmi category at the index visit to merge in as 
  # time invariant variables
  time_invariant_vars <- 
    data %>%
    filter(Censored == 0,
           IndexVisit == 1,
           Arb_PersonId %in% mod_data$Arb_PersonId) %>%
    select(Arb_PersonId, Age, BMI, Intervention.factor, Cohort) %>% 
    mutate(Age_cat = ifelse(Age <= 45, "<=45", NA),
           Age_cat = ifelse(Age > 45 & Age <= 60, "45-60", Age_cat),
           Age_cat = ifelse(Age > 60, ">60", Age_cat),
           bmi_gt_30 = ifelse(BMI >= 30, 1, 0),
           bmi_gt_35 = ifelse(BMI >= 35, 1, 0),
           bmi_gt_40 = ifelse(BMI >= 40, 1, 0),
           Arb_PersonId = factor(Arb_PersonId),
           Intervention = Intervention.factor) %>%
    select(-Age, -Intervention.factor)
  
  # Replace Age_cat, BMI, and Cohort with time-invariant versions
  mod_data %<>%
    select(-Age_cat, -bmi_gt_30, -BMI, -Cohort) %>%
    left_join(.,
              time_invariant_vars,
              by = c("Arb_PersonId", "Intervention"))
  
  # Set the reference for intervention and reorder Age_cat
  mod_data %<>%
    mutate(Intervention = relevel(factor(Intervention), ref = "Control"),
           Age_cat = factor(Age_cat, levels = c("<=45", "45-60", ">60")),
           Race_Ethnicity = factor(Race_Ethnicity, levels = c("Non-Hispanic White", "Hispanic or Latino", "Black or African American", "Asian", "Other", "Unknown")))
  
  
  # # Load the engagement .csv file
  # engagement <- read_csv(
  #   str_c(proj_parent_dir, "/working_files/", "clinic_engagement.csv"),
  #   col_types = cols()
  #   )
  # 
  # # filter data
  # # join with mod data by dept epic Id
  # mod_data <- left_join(mod_data, 
  #                       (engagement %>% select(DepartmentEpicId, Engagement)), 
  #                       by = "DepartmentEpicId")
           
  # Make modifications to the names and values of the phase variables so that
  # the output of the lm() summary is not so cumbersome to read
  mod_data %<>%
    mutate(Phase = Intervention) %>%
    mutate(Phase = case_match(Phase,
                          "Control" ~ 0,
                          "Intervention" ~ 1))
  mod_data %<>%
    mutate(slope1 = EE*Phase,        # EE group in Intervention
           slope2 = EE*(1-Phase),    # EE group in control
           slope3 = (1-EE)*Phase,    # ENE group in intervention
           slope4 = (1-EE)*(1-Phase))# ENE group in Control

  
  # Capture EE -----------------------------------------------------------------
  # Make a subset of EE only
  ee <- mod_data %>%
    filter(EE == 1)
  
  # Create a vector of the Arb_PersonIds in control phase
  gt_2_per_phase_con <-
    ee %>%
    filter(Intervention == "Control") %>%
    pull(Arb_PersonId)

  # Create a vector of the Arb_PersonId in intervention phase
  gt_2_per_phase_int <-
    ee %>%
    filter(Intervention == "Intervention") %>%
    pull(Arb_PersonId)

  # Create a data frame of the labs, meds, procedures for comparison
  # Values need to be captured by phase, because some patients will have visits
  # in the mod_data frame that are only in control or only in intervention due
  # to mod_data and subsequently ee being restricted to patients with 2 or more
  # visits in each phase, and not including the index visits.
  names_to_select <- 
    visits_post_id %>%
    select(Arb_PersonId, Intervention, 
           Systolic_blood_pressure:Temperature,
           A1C:N_Meds_AOM, 
           EOSS,
           # all_of(comorbidity_names),
           ) %>%
    names()
  
  labs_meds_procs <-
    bind_rows(
      (visits_post_id %>%
         filter(Arb_PersonId %in% gt_2_per_phase_con,
                IndexVisit == 1,
                Intervention.factor == "Control") %>%
         select(all_of(names_to_select))),
      (visits_post_id %>%
         filter(Arb_PersonId %in% gt_2_per_phase_int,
                IndexVisit == 1,
                Intervention.factor == "Intervention") %>%
         select(all_of(names_to_select)))) %>%
    mutate(Arb_PersonId = factor(Arb_PersonId))
  
  mod_data_w_ind <- mod_data
  mod_data <- mod_data %>% filter(IndexVisit == 0)
  ee <- ee %>% filter(IndexVisit == 0)

  # Write out the data set -----------------------------------------------------
  # Save mod_data to the data directory on the network drive
  save(mod_data, file = here("delivery_20240326", "data", str_c("mod_data_full_", delivery, ".RData")))
  
  save(ee, file = here("delivery_20240326", "data", str_c("mod_data_ee_", delivery, ".RData")))
  
  save(mod_data_w_ind, file = here("delivery_20240326", "data", str_c("mod_data_w_ind", delivery, ".RData")))
  
  mod_data <- list(mod_data, 
                   ee, 
                   mod_data_w_ind)

  names(mod_data) <- c("mod_data", "ee", "mod_data_w_ind")
  
  return(mod_data)
  
}
