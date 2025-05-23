# Process labs, procedures, and referrals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# labs_procedures() function is designed to capture labs, screeners, o2, cpap,
# bipap, bariatric surgery, and referrals for eligible and enrolled patients
# at a given index date either control (0) or intervention (1).

# 1. O2/CPAP/BIPAP - 30days prior to and 30days after index date
# 2. Labs - 30days prior to and 30days after index date
# 3. Screeners - 30days prior to and 30days after index date
#  a. PHQ2/9
#  b. GAD7
# 4. Bariatric Surgery - At any time during the phase
# 5. Referrals - At any time during the phase

# - 08-01-2023: Leigh & Qing, capture within the respective time windows at
#   index date and at last visit.
# - 10-24-2024: Stats group, capture bariatric procedures and referrals at any 
#   time during the phase.

# n.b. 
# - This script is designed to be deployed on control and intervention index
#   visit data separately
# - To capture labs for the last visit, a pseudo index date will be created to
#   serve as the reference date. For the last visits in control and in
#   intervention, the bariatric surgery and referrals values will not be valid
#   because the search windows will be from the encounter date of the last visit
#   to cohort_end_date which may be a small period.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

labs_procedures <- function(temp) {

  # Check that all the required data frames are loaded in the workspace.
  dfs <- c("procedure", "labs", "flowsheets", "referrals")
  if (sum(map_lgl(dfs, exists)) != length(dfs)) {
    stop("Not all required data frames are loaded into the workspace. Check that procedure, labs, flowsheets, and referrals are in properly named.") # nolint: line_length_linter.
  }

  # Filter the input data frame to the index visits in the specified phase
  # Input to labs and procedures should be a subset of either the last visit
  # or index visit in each phase of the study. The index visit variable is to
  # trick the rest of the function into thinking that the reference visit is
  # the index visits. Reference visit meaning last or index visit.
  temp <-
    temp %>%
    filter(Censored == 0,
           IndexVisit == 1)

  # Check which phase the input data frame is in. If all of the values are 0
  # then the input is in the control phase, otherwise it is in the intervention
  # phase
  if ((names(table(temp$Intervention))) == "0") {
    phase <- "control"
  } else {
    phase <- "intervention"
  }

  # Set an algorithm to determine the Cohort_end_date by phase -----------------
  # If the input is in the control phase, then the end date will be assigned by
  # cohort to coincide with the date that each cohort crosses over to
  # intervention.
  # If the input is in the intervention phase, then the end date is set to
  # September 16, 2024 as this is the last date, we will likely use to capture
  # data for the main research article
  if (phase == "control") {
    # If all of the values in the con
    temp %<>%
      mutate(Cohort_end_date = ifelse(Cohort == "Cohort1", "2021-03-16",
                                      ifelse(Cohort == "Cohort2", "2022-03-16",
                                             "2023-03-16")))
  } else {
    temp %<>%
      mutate(Cohort_end_date = "2024-09-16")

  }

  # distinct_pts_x_ind, needed downstream for labs and procedures
  # These are unique combinations of PersonIds and Index Dates at a specified
  # intervention because the original algorithm was never designed to work with
  # two index dates (1 for control and 1 for intervention).
  distinct_pts_x_ind <- temp %>%
    filter(Censored == 0,
           IndexVisit == 1) %>%
    select(Arb_PersonId, IndexDate, Cohort_end_date) %>%
    distinct() %>%
    filter(!is.na(IndexDate))

  # Check patient Ids with more than one unique distinct index date for trouble
  # shooting
  if ((distinct_pts_x_ind %>%
         group_by(Arb_PersonId) %>%
         count() %>%
         filter(n > 1) %>%
         nrow()) != 0) {
    stop("Distinct patients at index are not unique!!")
  }

  # Capture the ids to check
  ids_to_check <-
    distinct_pts_x_ind %>%
    group_by(Arb_PersonId) %>%
    count() %>%
    filter(n > 1) %>%
    pull(Arb_PersonId)

  temp %>%
    filter(Arb_PersonId %in% ids_to_check) %>%
    arrange(Arb_PersonId) %>%
    select(Arb_PersonId, Arb_EncounterId, EncounterDate,      Censored,
           IndexDate,    IndexVisit,      IndexVisitEligible, Intervention)

  # Labs  --------------------------------------------------------------------
  # Use 30 days prior to and after index date in each phase
  # A1c (%),TSH (mIU/L),TG (mg/dl),HDL (mg/dl; * note decrease in value),
  # AST (U/L),ALT (U/L),eGFR (ml/min/1.73 m2; * note decrease in value)

  # Restrict only to labs of interest
  str_labs <- c("A1C", "TSH", "Triglycerides", "HDL",
                "AST", "ALT", "eGFR", "Cystatin C")

  # Create a data frame consisting of Arb_PersonId, IndexDate, and columns of
  # the labs of interest filled with their NumericValue. Select the columns in
  # labs, filter rows that match the labs of interest. Drop any rows that
  # have NA as a NumericValue. Then filter rows for which the date falls within
  # the window of interest(6 months prior to and 14 days after index visit).
  # Identify the labs that are closest to the index date, remove the
  # LabCollectionDate column and then convert to wide.
  labs_data_wide <-
    left_join(distinct_pts_x_ind,
              select(labs,     Arb_PersonId, LabCollectionDate,
                     Category, NumericValue),
              by = "Arb_PersonId") %>%
    filter(Category %in% str_labs) %>%
    drop_na(NumericValue) %>%
    filter((IndexDate - LabCollectionDate) <= 30,
           (IndexDate - LabCollectionDate) >= -30) %>%
    group_by(Arb_PersonId, IndexDate, Category) %>%
    slice(which.min(abs(IndexDate - LabCollectionDate))) %>%
    select(-LabCollectionDate) %>%
    spread(Category, NumericValue) %>%
    select(-Cohort_end_date)

  # Merge with temp
  temp <- left_join(temp, labs_data_wide, by = c("Arb_PersonId", "IndexDate"))


  # Screeners  ---------------------------------------------------------------
  # Prep Screener Function - Captures one value per person to merge into the
  # input visits_post_id data frame. Screeners (PHQ/GAD) were problematic as the
  # data contained for the same patient at the same encounter, multiple values,
  # multiple dates, sometimes multiple values across the same date.
  prep_screener <- function(screener_data) {
    # pull encounter ids for records that have inconsistent values across the
    # same instrument, encounter and patient
    inc_enc_ids <-
      screener_data %>%
      filter(Value != "") %>%
      group_by(Arb_PersonId, Arb_EncounterId, FlowsheetRowDisplayName) %>%
      summarise(n_distinct_pts = n_distinct(Value), .groups = "drop") %>%
      filter(n_distinct_pts > 1) %>%
      pull(Arb_EncounterId)

    # Remove encounter ids with multiple yet different values for the same
    # encounter
    screener_data %<>%
      filter(!Arb_EncounterId %in% inc_enc_ids)

    # consistent encounter ids
    con_enc_ids <-
      screener_data %>%
      filter(!Arb_EncounterId %in% inc_enc_ids,
             Value != "") %>%
      group_by(Arb_PersonId, Arb_EncounterId, FlowsheetRowDisplayName) %>%
      count() %>%
      filter(n > 1) %>%
      pull(Arb_EncounterId)

    # Create a data frame of records with multiple consistent values
    consistent_values <-
      screener_data %>%
      filter(Arb_EncounterId %in% con_enc_ids,
             Value != "") %>%
      arrange(Arb_PersonId, Arb_EncounterId, FlowsheetRowDisplayName)

    # Capture one record per each encounter and instrument
    consistent_values %<>%
      group_by(Arb_PersonId, Arb_EncounterId, FlowsheetRowDisplayName) %>%
      slice_head()

    screener_data %<>%
      filter(!Arb_EncounterId %in% con_enc_ids)

    # Check for those duplicates
    N_mult_vals <- screener_data %>%
      filter(!Arb_EncounterId %in% inc_enc_ids) %>%
      group_by(Arb_PersonId, Arb_EncounterId, FlowsheetRowDisplayName) %>%
      count() %>%
      filter(n > 1) %>%
      nrow()

    if (N_mult_vals == 0) {
      # Bind consistent values with screener data
      screener_data <-
        bind_rows(screener_data, consistent_values)
    } else {
      stop("Multiple values for the same encounter detected. Review code.")
    }

    # Create a wide data frame
    screener_data_wide <-
      screener_data %>%
      mutate(FlowsheetRowDisplayName =
             recode(FlowsheetRowDisplayName,
                    "GAD-7 Total Score"     = "GAD7",
                    "PHQ-2 SCORE"           = "PHQ2",
                    "PHQ-9 Total Score"     = "PHQ9",
                    "PHQ-8 Total Score"     = "PHQ8",
                    "PHQ-9 Completed Today" = "PHQ9_Completed",
                    "PHQ2 Completed Today"  = "PHQ2_Completed")) %>%
      select(Arb_PersonId, Arb_EncounterId, FlowsheetRowDisplayName,
             Value,        FlowsheetDate) %>%
      rename(Category = FlowsheetRowDisplayName) %>%
      group_by(Arb_PersonId, Arb_EncounterId, FlowsheetDate) %>%
      pivot_wider(names_from = Category, values_from = Value) %>%
      ungroup()

    return(screener_data_wide)
  }


  ## PHQ2 --------------------------------------------------------------------
  # Filter the main flowsheets data
  screener_data <-
    flowsheets %>%
    filter(Arb_PersonId %in% temp$Arb_PersonId) %>%
    filter(FlowsheetRowDisplayName == "PHQ-2 SCORE") %>%
    filter(!FlowsheetTemplateName %in% c("T UCHS AMB BH WELLBEING SCREENER",
                                         "T AMB HP LVN BH SCREENING")) %>%
    filter(Value != "")

  # Address multiple inconsistent and consistent values and convert to wide
  screener_data_wide <- prep_screener(screener_data)


  # Filter data for PHQ2_Completed
  phq2_completed <-
    flowsheets %>%
    filter(Arb_EncounterId %in% screener_data_wide$Arb_EncounterId) %>%
    filter(FlowsheetRowDisplayName == "PHQ-2 SCORE") %>%
    mutate(PHQ2_Completed = Value) %>%
    select(Arb_EncounterId, PHQ2_Completed) %>%
    group_by(Arb_EncounterId) %>%
    slice_head()


  # Merge in PHQ-2 Completed Today, moved here to avoid inflating NAs
  screener_data_wide <-
    left_join(screener_data_wide,
              phq2_completed,
              by = "Arb_EncounterId") %>%
    mutate(PHQ2 = ifelse(PHQ2_Completed == "No" & PHQ2 == 0, NA, PHQ2)) %>%
    select(-PHQ2_Completed)


  # Merge in index dates, filter by the specified window, and select the value
  # closest to the index date.
  screener_data_wide %<>%
    mutate(FlowsheetDate = as.Date(FlowsheetDate)) %>%
    left_join(.,
              select(temp, Arb_PersonId, IndexDate),
              by = "Arb_PersonId") %>%
    filter((IndexDate - FlowsheetDate) <= 30,
           (IndexDate - FlowsheetDate) >= -30) %>%
    mutate(diff = abs(IndexDate - FlowsheetDate)) %>%
    group_by(Arb_PersonId, IndexDate) %>%
    arrange(diff) %>%
    slice_head() %>%
    ungroup() %>%
    select(PHQ2, Arb_PersonId, IndexDate)

  temp <- left_join(temp,
                    screener_data_wide,
                    by = c("Arb_PersonId", "IndexDate"))

  ## PHQ9 --------------------------------------------------------------------
  # Filter the main flowsheets data
  screener_data <-
    flowsheets %>%
    filter(Arb_PersonId %in% temp$Arb_PersonId) %>%
    filter(FlowsheetRowDisplayName == "PHQ-9 Total Score") %>%
    filter(!FlowsheetTemplateName %in% c("T UCHS AMB BH WELLBEING SCREENER",
                                         "T AMB HP LVN BH SCREENING")) %>%
    filter(Value != "")

  # Address multiple inconsistent and consistent values and convert to wide
  screener_data_wide <- prep_screener(screener_data)


  # Filter data for PHQ9_Completed
  phq9_completed <-
    flowsheets %>%
    filter(Arb_EncounterId %in% screener_data_wide$Arb_EncounterId) %>%
    filter(FlowsheetRowDisplayName == "PHQ-9 Completed Today") %>%
    mutate(PHQ9_Completed = Value) %>%
    select(Arb_EncounterId, PHQ9_Completed) %>%
    group_by(Arb_EncounterId) %>%
    slice_head()


  # Merge in PHQ-9 Completed Today, moved here to avoid inflating NAs
  screener_data_wide <-
    left_join(screener_data_wide,
              phq9_completed,
              by = "Arb_EncounterId") %>%
    mutate(PHQ9 = ifelse(PHQ9_Completed == "No" & PHQ9 == 0, NA, PHQ9)) %>%
    select(-PHQ9_Completed)


  # Merge in index dates, filter by the specified window, and select the value
  # closest to the index date.
  screener_data_wide %<>%
    mutate(FlowsheetDate = as.Date(FlowsheetDate)) %>%
    left_join(.,
              select(temp, Arb_PersonId, IndexDate),
              by = "Arb_PersonId") %>%
    filter((IndexDate - FlowsheetDate) <= 30,
           (IndexDate - FlowsheetDate) >= -30) %>%
    mutate(diff = abs(IndexDate - FlowsheetDate)) %>%
    group_by(Arb_PersonId, IndexDate) %>%
    arrange(diff) %>%
    slice_head() %>%
    ungroup() %>%
    select(PHQ9, Arb_PersonId, IndexDate)

  temp <- left_join(temp,
                    screener_data_wide,
                    by = c("Arb_PersonId", "IndexDate"))


  ## GAD7 --------------------------------------------------------------------
  screener_data <-
    flowsheets %>%
    filter(Arb_PersonId %in% temp$Arb_PersonId) %>%
    filter(FlowsheetRowDisplayName == "GAD-7 Total Score") %>%
    filter(!FlowsheetTemplateName %in% c("T UCHS AMB BH WELLBEING SCREENER",
                                         "T AMB HP LVN BH SCREENING")) %>%
    filter(Value != "")

  # Address multiple inconsistent and consistent values and convert to wide
  screener_data_wide <- prep_screener(screener_data)


  # Merge in index dates, filter by the specified window, and select the value
  # closest to the index date.
  screener_data_wide %<>%
    mutate(FlowsheetDate = as.Date(FlowsheetDate)) %>%
    left_join(.,
              select(temp, Arb_PersonId, IndexDate),
              by = "Arb_PersonId") %>%
    filter((IndexDate - FlowsheetDate) <= 30,
           (IndexDate - FlowsheetDate) >= -30) %>%
    mutate(diff = abs(IndexDate - FlowsheetDate)) %>%
    group_by(Arb_PersonId, IndexDate) %>%
    arrange(diff) %>%
    slice_head() %>%
    ungroup() %>%
    select(GAD7, Arb_PersonId, IndexDate)

  temp <- left_join(temp,
                    screener_data_wide,
                    by = c("Arb_PersonId", "IndexDate"))

  # Procedures (O2, CPAP, BIPAP) V2 --------------------------------------------
  O2CPAPBIPAP_data <-
    left_join(distinct_pts_x_ind,
              select(procedure, Arb_PersonId, ProcedureDate, Category),
              by = c("Arb_PersonId")) %>%
    filter(Category == "CPAP/BIPAP/O2") %>%
    filter(ProcedureDate <= Cohort_end_date) %>%
    group_by(Arb_PersonId, IndexDate) %>%
    summarise(O2CPAPBIPAP = n(), .groups = "rowwise") %>%
    mutate(O2CPAPBIPAP = 1)

  # Merge temp and O2CPAPBIPAP and recode NAs to 0
  temp <-
    left_join(temp, O2CPAPBIPAP_data, by = c("Arb_PersonId", "IndexDate")) %>%
    mutate(O2CPAPBIPAP = ifelse(is.na(.$O2CPAPBIPAP), 0, O2CPAPBIPAP)) #%>%

  # %%%%%%%%%%%%%%%% WORKING HERE TO MODIFY THE SEARCH PERIOD %%%%%%%%%%%%%%%%%%
  # Bariatric procedure --------------------------------------------------------
  # Search between the index date and the cohort end date for evidence of a
  # procedure. The input data frames to this function are split as crosses
  # between intervention phases and reference visit, i.e. ind_con, lv_con,
  # ind_int, and lv_int. The index visit is set according to the first eligible
  # visit in each phase and the cohort_end_date is set as either the crossover
  # date for control phase or the date_max for intervention. Therefore any visit
  # within the phase can be found where the procedure date is between the index
  # date and the cohort end date.

  # Get the Arb_PersonIds and dates of all available bariatric procedures
  bariatric <-
    procedure %>%
    filter(Category == "bariatric procedure", !is.na(ProcedureDate)) %>%
    select(Arb_PersonId, ProcedureDate) %>%
    distinct()

  bariatric_data <-
    left_join(distinct_pts_x_ind, bariatric, by = "Arb_PersonId") %>%
    filter(!is.na(ProcedureDate)) %>%
    filter(ProcedureDate <= Cohort_end_date,
           ProcedureDate >= IndexDate) %>%
    group_by(Arb_PersonId, IndexDate) %>%
    summarise("BariatricSurgery" = 1 * (any(!is.na(ProcedureDate))),
              .groups = "rowwise")

  temp <-
    left_join(temp, bariatric_data, by = c("Arb_PersonId", "IndexDate")) %>%
    mutate(BariatricSurgery = ifelse(is.na(.$BariatricSurgery), 0, 1))

  # Referrals ------------------------------------------------------------------
  # Search between the index date and the cohort end date for evidence of a
  # referral. See section Bariatric procedure for more details on how the
  # algorithm functions
  
  # Create a subset of data consisting of the unique patient IDs, referral
  # dates, and category of referral
  referrals_sub <-
    referrals %>%
    filter(Arb_PersonId %in% temp$Arb_PersonId) %>%
    select(Arb_PersonId, ReferralDate, Category) %>%
    distinct()

  # Merge distinct_pt_x_in and referrals sub, filter to those referrals that
  # were found between the index date and cohort end date. Next recode the cell
  # values to be used as reasonable column names without spaces. Finally, pivot
  # the data wider
  referrals_data_wide <-
    left_join(distinct_pts_x_ind, referrals_sub, by = "Arb_PersonId") %>%
    filter(ReferralDate <= Cohort_end_date,
           ReferralDate >= IndexDate) %>%
    mutate(Category =
           recode(Category,
                  "Referred to bariatric surgery" = "Ref_BariatricSurgery",
                  "Referred to Behavioral Health" = "Ref_BehavioralHealth",
                  "Referred to dietician"         = "Ref_Dietician",
                  "Referred to endocrinology"     = "Ref_Endo",
                  "Referred to wellness clinic"   = "Ref_WellnessClinic")) %>%
    group_by(Arb_PersonId, IndexDate) %>%
    slice(which.min(abs(IndexDate - ReferralDate))) %>%
    mutate(Value = 1) %>%
    spread(Category, Value) %>%
    select(-Cohort_end_date)

  # Merge the two data frames and fill in any NAs as zeros
  temp <- left_join(temp,
                    referrals_data_wide,
                    by = c("Arb_PersonId", "IndexDate")) %>%
    mutate(across(Ref_BariatricSurgery:Ref_WellnessClinic,
                  ~ifelse(is.na(.x), 0, .x)))

  return(temp)
}
