#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Medical Campus, Dept. of Family Medicine
# Aug 1, 2024
# Create tables for per protocol
# Copied from the aim1 tables script, but adapted to create the pp tables

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_pp_tables <- function(data){
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # # TEMPORARY CHUNK TO MAKE TABLES FROM A PREVIOUSLY GENERATED PIPELINE OUTPUT
  #
  # # Set the project root
  # proj_root <- "S:/FM/PATHWEIGH/Quantitative/Projects/"
  # 
  # # Set the data delivery date to the appropriate data set
  # delivery <- 20240326
  # 
  # # Load pp_mod_data
  # load(str_c(proj_root, "dataset_", delivery, "/data/pp_data_", delivery, ".Rdata"))
  # data <- pp_data
  # 
  # # Load visits_post_id
  # load(str_c(proj_root, "dataset_", delivery, "/data/processed_visits_post_id_", delivery, ".Rdata"))

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Set gtsave () file extension -----------------------------------------------
  # ext will drive the file out put type
  # valid options are .png, .pdf, and .docx
  gt_ext <- ".docx"
  
  # Table 1. Health metrics of analyzed patients at index ----------------------
  # Table of Encounter Level Demographics at Index Visits
  # n.b. unique patients may be counted in both control and intervention groups
  # if they have visits in both phases
  
  # Creates two tables, one for control and another for intervention with the
  # number of missing values for each variable in a separate column. Relies on
  # purrr to apply the same tbl_summary() function to data sets that have been
  # filtered to control or intervention patients, then sliced to one row per
  # patient. The tables are then merged, modified and output to docx. Once
  # tables are in word, they will need some manual modifications:
  
  # Generate a table of health metrics at the last visit -----------------------
  c("Control", "Intervention") %>%
    purrr::map(
      ~visits_post_id %>%
        filter(Arb_PersonId %in% (data %>% filter(Intervention == .x) %>% distinct(Arb_PersonId) %>% pull(Arb_PersonId))) %>%
        filter(IndexVisit == 1,
               Intervention.factor == .x) %>%
        mutate(across(PHQ2:GAD7, ~ as.numeric(.)),
               EOSS = fct_na_value_to_level(factor(EOSS, levels = c("0", "1", "2", "3")), level = "Unknown")) %>%
        select(Age, Sex, Race_Ethnicity, Insurance, BMI,
               Systolic_blood_pressure, Diastolic_blood_pressure,
               A1C:TSH, -`Cystatin C`, EOSS,
               PHQ2, PHQ9, GAD7, Smoking_Status) %>%
        tbl_summary(missing = "no",
                    type = list(c(PHQ2, PHQ9, GAD7) ~ 'continuous'),
                    statistic = list(all_continuous() ~ c("{mean} ({sd})")),
                    label = list(Race_Ethnicity ~ "Race/Ethnicity",
                                 BMI ~ "BMI (kg/m^2)",
                                 Systolic_blood_pressure ~ "Systolic BP (mmHg)",
                                 Diastolic_blood_pressure ~ "Diastolic BP (mmHg)",
                                 Smoking_Status ~ "Smoking Status"),
                    digits = list(all_categorical() ~ c(0,1),
                                  all_continuous() ~ c(1,1))) %>%
        add_n(.,
              statistic = "{N_miss} ({p_miss}%)",
              col_label = "**N Missing**") %>%
        modify_table_body(~.x %>% relocate(n, .after = stat_0))
    ) %>%
    tbl_merge(., tab_spanner = c("**Control**", "**Intervention**")) %>%
    as_gt() %>%
    gt::tab_header(title = "Health metrics of analyzed patients from index visits.") %>%
    gt::gtsave(filename = here("tables", "pp_tables",
                               str_c("pp_table_1a_",
                                     date_max, "_",
                                     Sys.Date(),
                                     gt_ext)))
  
  # Generate a table of health metrics at the last visit -----------------------
  c("Control", "Intervention") %>%
    purrr::map(
      ~visits_post_id %>%
        filter(Arb_PersonId %in% (data %>% filter(Intervention == .x) %>% distinct(Arb_PersonId) %>% pull(Arb_PersonId))) %>%
        filter(LastVisit == 1,
               Intervention.factor == .x) %>%
        mutate(across(PHQ2:GAD7, ~ as.numeric(.)),
               EOSS = fct_na_value_to_level(factor(EOSS, levels = c("0", "1", "2", "3")), level = "Unknown")) %>%
        select(Age, Sex, Race_Ethnicity, Insurance, BMI,
               Systolic_blood_pressure, Diastolic_blood_pressure,
               A1C:TSH, -`Cystatin C`, EOSS,
               PHQ2, PHQ9, GAD7, Smoking_Status) %>%
        tbl_summary(missing = "no",
                    type = list(c(PHQ2, PHQ9, GAD7) ~ 'continuous'),
                    statistic = list(all_continuous() ~ c("{mean} ({sd})")),
                    label = list(Race_Ethnicity ~ "Race/Ethnicity",
                                 BMI ~ "BMI (kg/m^2)",
                                 Systolic_blood_pressure ~ "Systolic BP (mmHg)",
                                 Diastolic_blood_pressure ~ "Diastolic BP (mmHg)",
                                 Smoking_Status ~ "Smoking Status"),
                    digits = list(all_categorical() ~ c(0,1),
                                  all_continuous() ~ c(1,1))) %>%
        add_n(.,
              statistic = "{N_miss} ({p_miss}%)",
              col_label = "**N Missing**") %>%
        modify_table_body(~.x %>% relocate(n, .after = stat_0))
    ) %>%
    tbl_merge(., tab_spanner = c("**Control**", "**Intervention**")) %>%
    as_gt() %>%
    gt::tab_header(title = "Health metrics of analyzed patients from last visits.") %>%
    gt::gtsave(filename = here("tables", "pp_tables",
                               str_c("pp_table_1b_",
                                     date_max, "_",
                                     Sys.Date(),
                                     gt_ext)))

  # Instructions for manual manipulation in word:
  # 1. Move unknown Race/Ethnicity value to N Missing column, delete row
  # 2. Move unknown Insurance value to N Missing column, delete row
  # 3. Move unknown EOSS value to N missing column, then delete row
  # 4. Change BMI kg/m^2 to superscript the 2.


  # Table 2: Referrals, Bariatric Procedures, and Anti-Obesity meds ------------
  # Bariatric Surgery: Window of 365 days prior to the index date in each phase
  c("Control", "Intervention") %>%
    purrr::map_df(
      ~visits_post_id %>%
        filter(Arb_PersonId %in% (data %>% filter(Intervention == .x) %>% distinct(Arb_PersonId) %>% pull(Arb_PersonId))) %>%
        filter(IndexVisit == 1,
               Intervention.factor == .x) %>%
        select(
          Intervention.factor,
          Ref_BariatricSurgery:Ref_WellnessClinic,
          BariatricSurgery,
          N_Meds_AOM) %>%
        mutate(across(N_Meds_AOM, ~ifelse(. >= 1, 1, 0)),
               Intervention.factor = factor(Intervention.factor, levels= c("Control", "Intervention")))
    ) %>%
    tbl_summary(
      by = Intervention.factor,
      missing = "ifany",
      label = list(
        Ref_BariatricSurgery ~ "Referral to bariatric surgery",
        Ref_BehavioralHealth ~ "Referral to behavioral health",
        Ref_Dietician ~ "Referral to dietician",
        Ref_Endo ~ "Referral to endocrinology",
        Ref_WellnessClinic ~ "Referral to wellness clinic",
        BariatricSurgery ~ "Bariatric surgery",
        N_Meds_AOM ~ "Anti-obesity medications"),
      digits = list(all_categorical() ~ c(0,1),
                    all_continuous() ~ c(1,1))) %>%
    as_gt() %>%
    gt::tab_header(title = "Referrals, bariatric surgery, and use of anti-obesity medications.") %>%
    gt::gtsave(
      filename = here("tables", "pp_tables",
                      str_c("pp_table_2_",
                            date_max, "_",
                            Sys.Date(),
                            gt_ext)))


  # Table 3 WPVs ---------------------------------------------------------------
  # Table 3 is created by merging two separate tables that are created in the
  # same way, but have different  data sets. Sub table 3.1 consists of the index
  # visits from those that are in mod_data[["ee"]] while sub table 3.2 consists
  # of all visits in from analyzed patients found in visits_post_id including
  # the index visits.

  ## Create data for sub table 3.1 ----
  # Capture index visits for those in control phase in mod data
  wpv_con_ind <- visits_post_id %>%
    filter(Arb_PersonId %in% (data %>% filter(Intervention == "Control") %>% distinct(Arb_PersonId) %>% pull(Arb_PersonId))) %>%
    filter(Intervention.factor == "Control",
           IndexVisit == 1)

  # Capture index visits for those in intervention phase in mod data
  wpv_int_ind <- visits_post_id %>%
    filter(Arb_PersonId %in% (data %>% filter(Intervention == "Intervention") %>% distinct(Arb_PersonId) %>% pull(Arb_PersonId))) %>%
    filter(Intervention.factor == "Intervention",
           IndexVisit == 1)
  
  ## Create wpv_data by stacking index visits in control and in intervention ----
  # wpv_data <- bind_rows(wpv_con_ind, wpv_int_ind)

  # Commented out because there is no need for pp_data to list analyzed and full
  ## Create data for sub table 3.2 ----
  # WPVs from all of those in mod_data, including the Index Visit
  # Capture index visits for those in control phase in mod data
  wpv_con_an_all <- visits_post_id %>%
    filter(Arb_PersonId %in% (data %>% filter(Intervention == "Control") %>% distinct(Arb_PersonId) %>% pull(Arb_PersonId))) %>%
    filter(Intervention.factor == "Control")

  # Capture index visits for those in intervention phase in mod data
  wpv_int_an_all <- visits_post_id %>%
    filter(Arb_PersonId %in% (data %>% filter(Intervention == "Intervention") %>% distinct(Arb_PersonId) %>% pull(Arb_PersonId))) %>%
    filter(Intervention.factor == "Intervention")

  ## Create a list consisting of the two separate data sets ----
  wpv_data <- list(bind_rows(wpv_con_ind, wpv_int_ind),
                   bind_rows(wpv_con_an_all, wpv_int_an_all))

  # Clear out objects from the workspace
  rm(wpv_con_ind, wpv_int_ind, wpv_con_an_all, wpv_int_an_all)

  # Create output table
  wpv_data %>%
    purrr::map(
      ~ .x %>%
        mutate(#WPV_OBHPI = ifelse(WPV_OBHPI == 1 | WPV_WMQ == 1, 1, 0), # Combine OBHPI and WMQ
               WPV_vtype = ifelse(WPV_IP == 1 | WPV_TH == 1, 1, 0), # Combine WPV_IP and WPV_TH
               Intervention.factor = factor(Intervention.factor, levels = c("Control", "Intervention"))) %>%
        select(Intervention.factor,
               WPV_ICD,
               WPV_CC,
               WPV_OBHPI,
               WPV_WMQ,
               WPV_vtype,
               WPV_smart) %>%
        tbl_summary(by = Intervention.factor,
                    label = list(WPV_CC ~ "Chief Complaint",
                                 WPV_ICD ~ "ICD Codes",
                                 WPV_OBHPI ~ "OBHPI",
                                 WPV_WMQ ~ "Weight management questionnaire",
                                 WPV_vtype ~ "PW Visit Type",
                                 WPV_smart ~ "PATHWEIGH Smart Set"),
                    missing = "no",
                    digits = everything() ~ c(0,1))
      ) %>%
    tbl_merge(., tab_spanner = c("**Index visits of analyzed**", "**All visits of analyzed**")) %>%
    as_gt() %>%
    gt::tab_header(title = "Visits with evidence of weight-related care and/or PATHWEIGH tools.") %>%
    gt::gtsave(filename = here("tables", "pp_tables",
                               str_c("pp_table_3_",
                                     date_max, "_",
                                     Sys.Date(),
                                     gt_ext)))

  # Add caption
  # Caption: Index visits of analyzed represent the method of enrollment into the study of patients in the analyzed subset. All visits of analyzed represent the classification of the non-index visits from patients in the analyzed subset. The analyzed subset refers to visits from enrolled patients with 2 or more visits in a single phase. Visits with evidence of weight-related care and/or PATHWEIGH tools are not mutually exclusive.


  # # Supplement 4: EE Health metrics (not just mod_data) ------------------------
  # # Instructions for modifying table,
  # # 1. move unknown Race/Ethnicity value to N Missing column, delete row
  # # 2. delete unknown Insturance row
  # # 3. move unknown EOSS value to N missing column, then delete row
  # 
  # # Same process as above with the mod_data[["ee"]] data frame
  # c("Control", "Intervention") %>%
  #   purrr::map(
  #     ~visits_post_id %>%
  #       filter(IndexVisit == 1,
  #              Intervention.factor == .x) %>%
  #       mutate(across(PHQ2:GAD7, ~ as.numeric(.)),
  #              EOSS = fct_na_value_to_level(factor(EOSS, levels = c("0", "1", "2", "3")), level = "Unknown")) %>%
  #       mutate(Insurance = factor(Insurance, levels = c("Commercial", "Medicare", "Medicaid", "Self-Pay", "Unknown"))) %>%
  #       mutate(Race_Ethnicity = factor(Race_Ethnicity, levels = c("Non-Hispanic White", "Hispanic or Latino", "Black or African American", "Asian", "Other", "Unknown"))) %>%
  #       mutate(Race_Ethnicity = fct_na_value_to_level(Race_Ethnicity, level = "Unknown")) %>%
  #       mutate(Intervention.factor = factor(Intervention.factor, levels = c("Control", "Intervention"))) %>%
  #       select(Age, Sex, Race_Ethnicity, Insurance, BMI,
  #              Systolic_blood_pressure, Diastolic_blood_pressure, 
  #              A1C:TSH, -`Cystatin C`, EOSS,
  #              PHQ2, PHQ9, GAD7, Smoking_Status) %>%
  #       tbl_summary(
  #         missing = "no",
  #         type = list(c(PHQ2, PHQ9, GAD7) ~ 'continuous'),
  #         statistic = list(all_continuous() ~ c("{mean} ({sd})")),
  #         label = list(Race_Ethnicity ~ "Race/Ethnicity",
  #                      BMI ~ "BMI (kg/m^2)",
  #                      Systolic_blood_pressure ~ "Systolic BP (mmHg)",
  #                      Diastolic_blood_pressure ~ "Diastolic BP (mmHg)",
  #                      Smoking_Status ~ "Smoking Status"),
  #         digits = list(all_categorical() ~ c(0,1),
  #                       all_continuous() ~ c(1,1))) %>%
  #       add_n(., 
  #             statistic = "{N_miss} ({p_miss}%)",
  #             col_label = "**N Missing**") %>%
  #       
  #       # show_header_names()
  #       modify_table_body(~.x %>% dplyr::relocate(n, .after = stat_0))) %>%
  # tbl_merge(., tab_spanner = c("**Control**", "**Intervention**")) %>%
  # as_gt() %>%
  # gt::tab_header(title = "Health metrics of eligible and enrolled patients from index visits") %>%
  # gt::gtsave(
  #   filename = here("tables",
  #                   str_c("supplement_4_",
  #                         date_max, "_",
  #                         Sys.Date(),
  #                         gt_ext)))
  #   
  #   
  # Supplement 5: Comorbidities ------------------------------------------------
  # Table for the comorbidities of interest at the index visit
  # The vector comorbidity_names comes from running the comorbidities script and
  # will be in the workspace if 02_process_rdata_image.R is ran.
  comorbidity_names <-
    names(visits_post_id %>%
            select(Dyslipidemia:`esophageal Cancer`))

  ordered_comorbidity_names <-
  visits_post_id %>%
    filter(Intervention.factor == "Control",
           IndexVisit == 1) %>%
    select(all_of(comorbidity_names)) %>%
    summarise_all(list(sum)) %>%
    pivot_longer(., cols = everything(), names_to = "comorbidity", values_to = "n") %>%
    arrange(desc(n)) %>%
    pull(comorbidity)

  c("Control", "Intervention") %>%
    purrr::map_df(
      ~visits_post_id %>%
        filter(Arb_PersonId %in% (data %>% filter(Intervention == .x) %>% distinct(Arb_PersonId) %>% pull(Arb_PersonId))) %>%
        filter(IndexVisit == 1,
               Intervention.factor == .x) %>%
        select(all_of(ordered_comorbidity_names), Intervention.factor) %>%
        mutate(Intervention.factor = factor(Intervention.factor, levels= c("Control", "Intervention")))
    ) %>%
    tbl_summary(by = "Intervention.factor",
                digits = everything() ~ 1) %>%
    as_gt() %>%
    gt::tab_header(
      title = "Analyzed patient comorbidities at the index visit") %>%
    gt::gtsave(
      filename = here("tables", "pp_tables",
                      str_c("supplement_5_",
                            date_max, "_",
                            Sys.Date(),
                            gt_ext)))


  # Table: Weight in kgs at the index visit
  c("Control", "Intervention") %>%
    purrr::map_df(
      ~visits_post_id %>%
        filter(Arb_PersonId %in% (data %>% filter(Intervention == .x) %>% distinct(Arb_PersonId) %>% pull(Arb_PersonId))) %>%
        filter(IndexVisit == 1,
               Intervention.factor == .x) %>%
        select(Weight_kgs, Intervention.factor) %>%
        mutate(Intervention.factor = factor(Intervention.factor, levels= c("Control", "Intervention")))
    ) %>%
    tbl_summary(by = "Intervention.factor",
                digits = everything() ~ 1,
                statistic = list(all_continuous() ~ c("{mean} ({sd})"))
                ) %>%
    as_gt() %>%
    gt::tab_header(
      title = "Weight (kgs) at the index visit") %>%
    gt::gtsave(
      filename = here("tables", "pp_tables",
                      str_c("pp_baseline_weight_",
                            date_max, "_",
                            Sys.Date(),
                            gt_ext)))
  
  

  # # Figure: Histograms of clinic engagement -------------------------------------
  # # Histogram of clinic engagment is created via a quarto document to for the
  # # following reasons
  # # 1. Outputs to .docx for journal submission
  # # 2. Allows modification of a caption or text to explain the clinic engagement
  # # scores
  # 
  # # The .qmd document is located in
  # # "S:/FM/PATHWEIGH/Quantitative/Projects/emr_data_processing/report_layouts/
  # 
  # # The .qmd document is clinic_engagement.qmd
  # 
  # # Set the final file output name
  # # clinic_engagement, _delivery_date
  # file_name <- "clinic_engagement.docx"
  # 
  # # Set the path to input .qmd file
  # file_in_path  <-  str_c(emr_dir, "report_layouts/clinic_engagement.qmd")
  # 
  # # Set the path to the output of the .qmd file
  # file_out_path <- str_c(emr_dir, "/report_layouts/", file_name)
  # 
  # # Render
  # quarto::quarto_render(
  #   input = file_in_path,
  #   execute_params = list(delivery = data_delivery_date))
  # 
  # # Copy from the default emr dir to the current data delivery's tables directory
  # invisible(
  #   file.copy(
  #     from = file_out_path,
  #     # to = here("tables", file_name),
  #     to = here("tables", 
  #               str_c("clinic_engagement_", date_max, "_", Sys.Date(), ".docx")),
  #     TRUE))
  # 
  # # Remove the output of the .qmd file after a copy has been placed in the 
  # # current project's specified directory
  # invisible(
  #   file.remove(from = file_out_path))
  # 
  # 
  # # Figure: LMER observed & predicted values -----------------------------------
  # The .qmd document is located in
  # "S:/FM/PATHWEIGH/Quantitative/Projects/emr_data_processing/report_layouts/
  
  # The .qmd document is lmer_model_output_and_figure.qmd
  
  # Set the final file output name
  file_name <- "pp_lmer_model_output_and_figure.docx"
  
  # Set the qmd file input path
  file_in_path  <-  str_c(emr_dir, "report_layouts/pp_lmer_model_output_and_figure.qmd")
  
  # Set the file output path
  file_out_path <- str_c(emr_dir, "/report_layouts/", file_name)
  
  # Render
  quarto::quarto_render(
    input = file_in_path,
    execute_params = list(delivery = data_delivery_date))
  
  # Copy from the default emr dir to the current data delivery's tables directory
  invisible(
    file.copy(
      from = file_out_path,
      # to = here("tables", file_name),
      to = here("tables", "pp_tables", 
                str_c("pp_lmer_model_output_and_figure_", date_max, "_", Sys.Date(), ".docx")),
      TRUE))
  
  # Remove the vestigial file from the .qmd file's directory
  invisible(
    file.remove(from = file_out_path))
}



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  