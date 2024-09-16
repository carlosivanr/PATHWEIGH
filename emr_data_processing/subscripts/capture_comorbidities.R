# Comorbidities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Description:

# This script loads two functions into the workspace. The first extracts the
# comorbidities of interest (cois) for a given set of patients. The second
# generates a histogram of the comorbidities of interest.

# The get_comorbidities() function utilizes either the dx or dx_co data to
# extract all of the patient level diagnoses of interest and arranges the data
# in wide format for merging into the main data frame

# Finally, this function creates a data frame of the most commonly observed
# comorbidities to merge with the input data. Only the top 3% of comorbidities
# of interest are merged. Columns will reflect a disease state and values will
# be binary and also be used for the gtsummary package.

# Requirements:
# - visits or visits_post_id - partially processed encounters for ee patients
# - dx or dx_co - dx reflects diagnoses at the visit and dx_co are diagnoses
#   from anywhere in the medical record
# - cois - data frame from an .RData file containing researcher-defined
#   comorbidities of interest

# n.b. Could use dx or dxco. Dx is for diagnoses at the encounter and dxco are
# for diagnoses anywhere in the EMR.

# *** Parameters to decide on:
# Which provenance values do we use, what about NAs in provenance?
# Calculate separate medians for each cohort.
# Top 3% of comorbidities
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Capture comorbidities function -----------------------------------------------
capture_comorbidities <- function(temp, times, years) {
  
  # Load comorbidities of interest as of 11-29-22 ------------------------------
  # Will load only if it is not already present in the global environment
  if (!exists("cois")) {
    load("D:/PATHWEIGH/working_files/comorbidities/comorbidities_of_interest_221129.Rdata")
  }

  # Load the compass tables via the read_pw_csv() function
  purrr::walk(
    .x  = c("dx", "dxco"),
    .f = read_pw_csv)
  
  # 1. Prep dx_sub -------------------------------------------------------------
  ## 1.1 Merge the dx and dxco data frames ----
  # Merging dx and dxco will result will not only result in duplicates, but
  # also introduce records that contain redundant information. For example,
  # some patients will have the same Encounter ID, Dx Code, etc, but only differ
  # in the Provenance and/or DiagnosisDate value
  dx_sub <- bind_rows(dx, dxco)
  
  # rm(dx, dxco)
  
  dx_sub <- distinct(dx_sub)
  
  ## 1.2 Pare down the data to only the patients in the input visits/temp df
  dx_sub <- 
    dx_sub %>%
    filter(Arb_PersonId %in% temp$Arb_PersonId)

  ## 1.3 Filter by comorbidities of interest for data reduction
  dx_sub <- 
    dx_sub %>%
    filter(DiagnosisCode %in% cois$`ICD-10.code(s)`)

  # 2. Slice dx ----------------------------------------------------------------
  # Slice one diagnosis per each patient and each encounter ID

  # n.b. The dates for each encounter ID are not consistent. ie the dates
  # for the same encounter ID can be vastly different.
  # arrange(DiagnosisDate) is used to make the function's behavior more
  # consistent/predictable.

  ## 2.1 slice the earliest diagnosis found ----
  dx_sub <-
    dx_sub %>%
    group_by(Arb_PersonId, Arb_EncounterId, DiagnosisCode) %>%
    arrange(DiagnosisDate) %>%
    slice_head() %>%
    ungroup()

  ## 2.2 Print an error in case there are any duplicates ----
  if (dim(dx_sub)[1] != n_distinct(dx_sub)) {
    # There should not be any duplicated rows.
    warning("Duplicated rows detected!")
  }

  # 3. Prep Dx_sub II ----------------------------------------------------------
  ## 3.1 merge the Index Date and Cohort into dx_sub by person id ----
  # Tag all rows with an index date and cohort
  # Warning message here bc of many-to-many relationships
  dx_sub <-
    dx_sub %>%
    left_join(.,
              select(temp, all_of(c("Arb_PersonId", "IndexDate", "Cohort"))),
              by = "Arb_PersonId")

  ## 3.2 merge the encounter date into dx sub by encounter id to capture a ----
  # DiagnosisDate from the EncounterDate in the case that there is no
  # DiagnosisDate available in dx_sub
  dx_sub <- 
    dx_sub %>%
      mutate(DiagnosisDate = lubridate::as_date(DiagnosisDate)) %>%
      left_join(.,
                select(temp, all_of(c("EncounterDate", "Arb_EncounterId"))),
                by = "Arb_EncounterId") %>%
      mutate(DiagnosisDate = if_else(is.na(DiagnosisDate),
                                     EncounterDate,
                                     DiagnosisDate)
      ) %>%
      select(-EncounterDate)

  # Filter diagnoses to the look back period
  dx_sub <- 
    dx_sub %>%
      mutate(lookback = date_min - DiagnosisDate) %>%
      filter(lookback < (years * 360),
             lookback >= 0) %>%
      select(-lookback)

  ## 3.3 filter out dx_sub to keep records with comorbidities of interest ----
  invisible(gc())
  dx_sub <- 
    dx_sub %>%
      mutate(ICD_Header = sub("\\..*", "", DiagnosisCode),
             code = sub("\\.", "", DiagnosisCode))

  ## 3.4 merge the unique category and disease.state values from cois into ----
  # dx_sub by ICD_Header. ICD_Header was chosen as the matching variable
  # because some comorbidities of interest are excluded when relying on the full
  # ICD-10 code variable such as F32.A.
  dx_sub <- 
    dx_sub %<>%
      left_join(.,
                distinct(select(cois, all_of(c("category",
                                               "disease.state",
                                               "ICD_Header")))),
                by = "ICD_Header")

  # 4. Prep dx_sub_coi_count ---------------------------------------------------
  ## 4.1 Create dx_sub_coi_count by converting dx_sub to wide ----
  # group by patient and disease.state. Disease.state is a character variable
  # used in place of an ICD_header and displays text in tables rather than ICD
  # header or prefix
  dx_sub_coi_count <- dx_sub %>%
    group_by(Arb_PersonId, disease.state) %>%
    count() %>%
    ungroup() %>%
    filter(n >= times) %>%
    mutate(n = 1) %>%
    pivot_wider(names_from = disease.state, values_from = n)

  ## 4.2 Check which variable to use ICD-10 or disease.state ----
  # "times" is a parameter indicating the number of times a diagnosis must be
  # observed in the data to count as a comorbidity

  # Get the names of the unique disease.states found
  names_disease.state <- dx_sub %>%
    group_by(Arb_PersonId, disease.state) %>%
    count() %>%
    ungroup() %>%
    filter(n >= times) %>%
    mutate(n = 1) %>%
    pivot_wider(names_from = disease.state, values_from = n) %>%
    select(-Arb_PersonId) %>%
    names() %>%
    data.frame(.) %>%
    rename("disease.state" = ".") %>%
    arrange(disease.state)

  # Get the names of the unique ICD-10 headers found
  disease.states_by_icd10 <- dx_sub %>%
    group_by(Arb_PersonId, ICD_Header) %>%
    count() %>%
    ungroup() %>%
    filter(n >= times) %>%
    mutate(n = 1) %>%
    pivot_wider(names_from = ICD_Header, values_from = n) %>%
    select(-Arb_PersonId) %>%
    names()

  # Check if using headers vs disease.state results in the same cois
  # Distinct disease.states extracted using ICD-10 headers
    names_ICD_headers <-
      cois %>%
      filter(ICD_Header %in% disease.states_by_icd10) %>%
      distinct(disease.state) %>%
      arrange(disease.state)

  # Does it matter if we use disease.state or ICD-10 Headers to count the
  # comorbidities?
  if ((invisible(
                 anti_join(names_disease.state, names_ICD_headers) %>%
                   nrow())) != 0) {
    cat("Using ICD10 headers differs from disease.state. Consider revising code.")
    cat("\n")
  } else {
    cat("Using ICD10 headers results in the same disease.states")
    cat("\n")
  }

  ## 4.3. Merge visits and dx_sub_coi_count ---
  # and create a variable that indicates the number of comorbidities per patient

  # Get the column names of dx_sub_coi_count to specify which columns to change
  # NAs to 0
  coi_names <- names(select(dx_sub_coi_count, -Arb_PersonId))
# 
#   output <- list("dx_sub_coi_count" = dx_sub_coi_count,
#                  "coi_names" = coi_names,
#                  "times" = times,
#                  "years" = years)

  temp <- left_join(temp, dx_sub_coi_count, by = "Arb_PersonId")
  
  invisible(gc())

  return(temp)
}
