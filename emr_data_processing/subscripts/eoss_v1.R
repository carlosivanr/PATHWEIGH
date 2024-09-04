# EOSS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. UC Anschutz Medical Campus, Dept. of Family Medicine

# This script relies on the comorbidities table to search for string terms
# form the EOSS variables to prevent not capturing the sub diagnoses that could
# arise from using ICD-10 codes.

# The compass comorbidities tables represent a fraction of all available 
# comorbidities. The only comorbidities delivered are those listed in the 
# comorbidities_of_interest.csv file located in the working_files/comorbidities 
# directory of the project. Search terms are used rather than ICD-10 codes bc
# the comorbidities of interest may not be a comprehensive list of all codes
# linked to a particular comorbidity. For example, there may be a different code
# for a condition of the left knee that is similar but not exactly the same as
# the condition for the right knee. In addition, search terms convey more
# human interpretable meaning i.e. Diabetes vs E68.xx 

# This script also only utilizes the labs/procedures and medications already
# available in the data. ie meds captured from within 30 days of the index
# date.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eoss <- function(temp){
  # tic()
  
  # Set the number of days prior to and after the index/reference visit
  # n.b. days after reference will be negative when subtracting reference
  # from OrderedDate
  n_days_prior <- 30
  n_days_after <- -30
  
  # Create a temporary data frame from the input data --------------------------
  #temp <- visits_post_id # for development purposes only
  
  # Filter data to only those who have an index visit
  temp %<>% 
    filter(Censored == 0, 
           IndexVisit == 1)
  
  # Save the number of rows of temp to ensure merges are conducted correctly
  n_obs_start <- dim(temp)[1]
  
  # Pare down the data frame for development purposes
  # *** Probably not needed if the meds and dxs cant be 
  # The labs could be used though
  # temp %<>%
  #   select(Arb_PersonId, 
  #          Intervention,
  #          Systolic_blood_pressure, Diastolic_blood_pressure,
  #          O2CPAPBIPAP:Ref_WellnessClinic)
  
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
  
  
  # Import updated dx tables ---------------------------------------------------
  # n.b. The dx and dx_como tables had errors in the ICD-10 codes that required
  # updated deliveries from compass. Current code was set to work with updated
  # tables for the 2023-03-22 delivery, but will need to be modified to work
  # with any delivery beyond 2023-03-22. No backwards compatibility for previous
  # data sets, unless those tables get updated as well.
  # Originally, Gestational Diabetes not found, had to revise & resubmit PR & 
  # Definitions, O24 instead of Q24. Then prediabetes was added.
  
  # Set delivery to the location/date in the PATHWEIGH_DATA_SECURE folder
  # to load updated tables for 20230322 only
  if (data_delivery_date == "20230322"){
    # Set delivery directory the updated comorbidity delivery
    delivery <- "20230809_DXandDXComo"
  
    # Specify file patterns to search for in the delivery directory
    patterns <- c("Table6_DX", "Table7_DXComo")
    
    # Create a vector of paths to the specified data delivery directory that match
    # the table vector for importing .csv files
    files <- dir(here("../../../PATHWEIGH_DATA_SECURE/", delivery), 
                 pattern = paste(patterns, collapse = "|"))
    
    # Parse the files paths to get the corresponding names to assign to the tables
    # once imported 
    df_names <- tolower(
      sub(".*_", "", 
          substring(substring(files, 1, nchar(files)-13), 7)
      )
    )
    
    # Modify the names to reflect what will be required for subsequent data
    # processing steps
    for (i in 1:length(df_names)){
      if (grepl("referral", df_names[i])){
        df_names[i] <- "referrals"
      } else if (grepl("samrtdata", df_names[i])){
        df_names[i] <- "smart"
      } else if (grepl("dxcomo", df_names[i])){
        df_names[i] <- "dxco"
      } else if (grepl("lab", df_names[i])){
        df_names[i] <- "labs"
      } else if (grepl("flowsheet", df_names[i])){
        df_names[i] <- "flowsheets"
      } else if (grepl("med", df_names[i])){
        df_names[i] <- "meds"
      }
    }
    
    # Set the file paths for importing COMPASS Tables
    file_paths <- str_c(here("../../../PATHWEIGH_DATA_SECURE", delivery, files))
    
    # Import COMPASS Tables in parallel --------------------------------------------
    # Set furrr options
    #plan(multisession, workers = availableCores()) # plan/future/furrr broken
    #plan(sequential)
    
    tables <- file_paths %>% 
      future_map(~ fread(., 
                         header = TRUE, 
                         integer64 = "numeric", 
                         na.strings = c("", "NA")))
    
    # Set the names of the tables
    tables <- setNames(tables, df_names)
    
    # Place all compass table data frames into the global environment
    invisible(list2env(tables, envir = .GlobalEnv))
    
    #rm(tables)
  }    

  # Dyslipidemia - picks up nothing, but is listed in the comorbidities_of_interest.
  # NAFLD - use  "Fatty (Change of) liver, not elsewhere classified", but is listed in the comorbidities_of_interest.
  stage_1_terms <- c(
    "Gestational diabetes",
    "Polycystic ovarian syndrome",
    "Proteinuria",
    "Cellulitis")
  
  stage_2_terms <- c(
    "Type 2 Diabetes", 
    "Hypertension",
    "Anxiety",
    "Obstructive sleep apnea",
    "Low back pain",
    "Gastro-esophageal reflux disease",
    "Depression",
    "Osteoarthritis",
    "Fatty (change of)")
  
  stage_3_terms <- c(
    "Chronic kidney disease",
    "cirrhosis",
    "embolus of pulmonary artery",
    "pulmonary embolism",
    "heart failure",
    "Pulmonary hypertension",
    "pancreatitis",
    "cerebral infarction",
    "atheroscler",                        # These terms are for ICD-10 code I25
    "aneurysm of heart",
    "chronic ischemic heart",
    "chronic total occlusion of coronary artery",
    "coronary artery aneurysm",
    "coronary artery dissection",
    "ischemic cardiomyopathy",
    "old myocardial infarction",
    "other forms of chronic ischemic heart disease",
    "silent myocardial ischemia")
  
  # Collect all of the search terms into a single vector  
  search_terms <- c(
    stage_1_terms,
    stage_2_terms,
    stage_3_terms)
  
  # Filter the dxco data frame to those within the input data set and to records
  # where the diagnosis matches one of the search terms
  dx_sub <- bind_rows(dx, dxco) %>%
    filter(Arb_PersonId %in% temp$Arb_PersonId) %>%
    filter(grepl(str_c(search_terms, collapse = "|"), 
                 ignore.case = TRUE, 
                 DiagnosisDescription)) %>%
    mutate(DiagnosisDate = lubridate::as_date(DiagnosisDate))
  
  # Merge in index dates to impose a window of when the medication was ordered  
  # distinct_pts_
  dx_data <-
    left_join(distinct_pts_x_ind,
              select(dx_sub, Arb_PersonId, DiagnosisDate, DiagnosisDescription),
              by=c("Arb_PersonId")) %>%
    filter((IndexDate-DiagnosisDate) <= n_days_prior,
           (IndexDate-DiagnosisDate) >= n_days_after) %>%
    group_by(Arb_PersonId, DiagnosisDescription) %>%
    slice_head() %>%
    ungroup()
  
  # Patient Ids meeting dx criteria for Stage 1 ----
  stage_1_dx <- 
    dx_data %>%
    filter(grepl(str_c(stage_1_terms, collapse = "|"), ignore.case = TRUE, DiagnosisDescription)) %>%
    pull(Arb_PersonId)
  
  # Patient Ids meeting dx criteria for Stage 2 ----
  stage_2_dx <- 
    dx_data %>%
    filter(grepl(str_c(stage_2_terms, collapse = "|"), ignore.case = TRUE, DiagnosisDescription)) %>%
    pull(Arb_PersonId)
  
  # Patient Ids meeting dx criteria for Stage 3 ----
  stage_3_dx <- 
    dx_data %>%
    filter(grepl(str_c(stage_3_terms, collapse = "|"), ignore.case = TRUE, DiagnosisDescription)) %>%
    pull(Arb_PersonId)
  
  # Check for terms
  # dx_sub %>%
  #   filter(grepl("oma", 
  #                ignore.case = TRUE, 
  #                DiagnosisDescription)) %>%
  #   pull(DiagnosisDescription) %>%
  #   table(.) %>%
  #   names(.)
  
  # Create a table to display the diagnoses
  # *** Will this table be reproduced many times? Will it vary by intervention?
  # If so, then modify the output filename to include a suffix
  # Alternatively, save the data, combine, and then create a table
  # dx_sub %>%
  #   select(DiagnosisDescription) %>%
  #   tbl_summary(sort = everything() ~ "frequency") %>%
  #   modify_caption("**Available EOSS search terms for patients of interest**") %>%
  #   as_gt() %>%
  #   gt::gtsave(
  #     filename = here("tables", 
  #                     str_c("eoss_diagnoses_", 
  #                           date_max, "_", 
  #                           Sys.Date(), ".pdf")))
  
  # Medications ----------------------------------------------------------------
  beta_blockers <- c("propranolol", "metoprolol")
  
  
  diabetes_meds <- c("insulin", 
                     "glargine", 
                     "detemir",
                     "degludec", 
                     "glyburide",
                     "glibenclamide",
                     "glimepiride",
                     "glipizide",
                     "nateglinide",
                     "repaglinide",
                     "rosiglitazone",
                     "pioglitazone",
                     "metformin",
                     "exenatide",
                     "liraglutide",
                     "semaglutide",
                     "pramlintide",
                     "dulaglutide",
                     "canagliflozin",
                     "empagliflozin",
                     "dapagliflozin",
                     "ertugliflozin")
  
  
  psych_meds <- c("Nortriptyline",
                  "Amitriptyline",
                  "Doxepin",
                  "Paroxetine",
                  "Mirtazapine",
                  "Thioridazine",
                  "Haloperidol",
                  "Olanzapine",
                  "Risperadone",
                  "Clozapine",
                  "Quetiapine",
                  "Aripiprazole",
                  "Valproic acid", 
                  "valproate",
                  "Lithium",
                  "Buproprion",
                  "Venlafaxine",
                  "Desvenlafaxine",
                  "Ziprasidone",
                  "Lisdexamfetamine",
                  "Methylphenidate",
                  "Amphetamine", 
                  "dextroamphetamine")
  
  # Combine diabetes and psych meds into a single vector
  stage_2_meds <- c(beta_blockers,
                    diabetes_meds, 
                    psych_meds)  
  
  # Create meds sub to capture the medications of interest for the patients
  # of interest
  meds_sub <-   
    meds %>%
    filter(Arb_PersonId %in% distinct_pts_x_ind$Arb_PersonId,
           ActiveMed == "Y",
           grepl(str_c(c(beta_blockers,
                         diabetes_meds,
                         psych_meds), 
                       collapse = "|"), 
                 ignore.case = TRUE, 
                 GenericName))
  
  # Check to see that all values are unique
  if (n_distinct(meds_sub) != dim(meds_sub)[1]){
    stop("Duplicated rows in meds_sub, review and revise !!!")
  }
  
  # Merge in index dates to impose a window of when the medication was ordered   
  meds_data <-
    left_join(distinct_pts_x_ind,
              select(meds_sub, Arb_PersonId, OrderedDate, GenericName, ActiveMed),
              by=c("Arb_PersonId")) %>%
    filter((IndexDate-OrderedDate) <= n_days_prior,
           (IndexDate-OrderedDate) >= n_days_after) %>%
    group_by(Arb_PersonId, GenericName) %>%
    slice_head() %>%
    ungroup() %>%
    pull(Arb_PersonId)
  
  
  # Create EOSS Stage ----------------------------------------------------------
  # Create an empty column for EOSS -----
  temp %<>%
    mutate(EOSS = NA)
  
  # Criteria for Stage 0 ----
  temp %<>%
    mutate(EOSS = ifelse(
      Systolic_blood_pressure < 130 | 
        Diastolic_blood_pressure < 180 |
        A1C < 5.7 |
        ALT < 36 & AST < 33 |
        Triglycerides < 150 |
        HDL > 60 |
        eGFR > 90 |
        PHQ9 < 5 |
        GAD7 < 5, 0, EOSS))
  
  # Criteria for Stage 1 ----
  temp %<>%
    mutate(EOSS = ifelse(
      Systolic_blood_pressure >= 130 & Systolic_blood_pressure < 140 | 
        Diastolic_blood_pressure >= 80 & Diastolic_blood_pressure < 90 |
        A1C >= 5.7 & A1C < 6.5 |
        ALT >= 36 |
        AST >= 33 |
        Triglycerides >= 150 & Triglycerides <= 200 |
        HDL < 60 |
        eGFR >= 60 & eGFR <= 90 |
        PHQ9 >= 5 & PHQ9 <=9 |
        GAD7 >= 5 & GAD7 <=9 |
        Arb_PersonId %in% stage_1_dx, 1, EOSS))
  
  
  # Criteria for Stage 2 ----
  temp %<>%
    mutate(EOSS = ifelse(
      Systolic_blood_pressure >= 140 | 
        Diastolic_blood_pressure >= 90 |
        # FASTING GLUCOSE VARIABLE NOT AVAILABLE
        A1C >= 6.5 |
        Triglycerides > 200 |
        HDL < 50 |
        eGFR < 60 |
        PHQ9 > 9 |
        GAD7 > 9 |
        O2CPAPBIPAP == 1 |
        Arb_PersonId %in% stage_2_dx |
        Arb_PersonId %in% meds_data, 2, EOSS))
  
  
  # Criteria for Stage 3 ----
  temp %<>%
    mutate(EOSS = ifelse(Arb_PersonId %in% stage_3_dx, 3, EOSS))
  
  # toc()
  return(temp)
}
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# %%%%%%%%%%%%%% VESTIGIAL CODE TO REVIEW THE DX_SUB TABLE DATA %%%%%%%%%%%%%%%%
# Check the dx_sub table -----------------------------------------------------
# Searches over 4 million records
# "cancer" not found in the raw data
# dx_sub %>%
# filter(grepl("cerebral infarction", 
#              ignore.case = TRUE, 
#              DiagnosisDescription)) %>%
# select(DiagnosisDescription) %>%
# tbl_summary()
# 
# 
# dx_sub %>%
#   filter(grepl("I25", 
#                ignore.case = TRUE, 
#                DiagnosisCode)) %>%
#   select(DiagnosisCode) %>%
#   tbl_summary()
# 
# dx_sub %>%
#   filter(grepl("atheroscler", 
#                ignore.case = TRUE, 
#                DiagnosisDescription)) %>%
#   select(DiagnosisCode) %>%
#   tbl_summary()
# 
# dx_sub %>%
#   filter(grepl("K76", 
#                ignore.case = TRUE, 
#                DiagnosisCode)) %>%
#   select(DiagnosisDescription) %>%
#   tbl_summary()
#
# check_dx %>%
#   select(DiagnosisDescription) %>%
#   tbl_summary()
# 
# # Check ICD-10 Codes ----
# # Checking for O24 in ICD-10 Codes comes up with nothing
# dx %>% 
#   filter(grepl("O24", ignore.case = TRUE, DiagnosisCode)) %>%
#   select(DiagnosisCode) %>%
#   tbl_summary()
# 
# # Create a table of all available ICD-10 Codes in the dx table
# # When searching all ICD-10 codes, none start with "O", Other maternal 
# # disorders predominantly related to pregnancy ICD-10-CM Code range O20-O29
# dx %>%
#   select(DiagnosisCode) %>%
#   tbl_summary()
# 
# # Checking the dxco table does not result in any "O" icd_10 codes either,
# # suggesting that it may have not been picked up in the data. dxco searches
# # almost 20 million records
# dxco %>%
#   select(DiagnosisCode) %>%
#   tbl_summary()
# 
# # Check codes
# dxco %>%
#   filter(grepl("E78",
#                ignore.case = TRUE,
#                DiagnosisCode)) %>%
#   select(DiagnosisDescription) %>%
#   tbl_summary()

