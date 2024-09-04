# Define WPVs by Chief Complaints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Relevant chief complaints include search terms for "weight", "obesity," or 
# "obese". "Weight Loss Consult" or "Weight loss" in tandem with "Weight 
# Management."

# The key words that could be used in a filter statement are:
# 1. WEIGHT MANAGMENT, will include those with "WEIGHT LOSS"
# 2. WEIGHT PROBLEM
# 3. WEIGHT CHECK
# 4. WEIGHT GAIN, must exclude "INADEQUATE WEIGHT GAIN"
# 5. WEIGHT LOSS CONSULT
# 6. WEIGHT CHANGE
# 7. OBESITY

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wpv_cc <- function(temp) {
  ## 1. Chief Complaints ----
  # Create a list of all possible unique AND relevant Encounter Chief Complaints
  # to use for matching encounter chief complaints

  # Works on the chief complaints column, drops any NAs, sets only the unique
  # combinations of complaints, separate rows using the ";" as a separator, then
  # trims the white spaces. Gets the unique columns again and then matches for
  # anything with WEIGHT or OBES. Recodes WEIGHT LOSS CONSULT to WEIGHT CONSULT
  # to be able to filter out WEIGHT LOSS, then filters out WEIGHT LOSS or
  # INADEQUATE, and then recodes WEIGHT CONSULT back to WEIGHT LOSS CONSULT.
  # n.b. two unique() statements are not needed but having both greatly
  #  improves processing speed.

  chief_complaints <- temp %>%
    select(EncounterChiefComplaints) %>%
    drop_na() %>%
    unique() %>%
    separate_rows(EncounterChiefComplaints, sep = ";") %>%
    mutate_if(is.character, str_trim) %>%
    unique() %>%
    filter(grepl("WEIGHT|OBES", EncounterChiefComplaints)) %>%
    transmute(
      EncounterChiefComplaints =
        recode(EncounterChiefComplaints,
               "WEIGHT LOSS CONSULT" = "WEIGHT CONSULT")) %>%
    filter(!grepl("WEIGHT LOSS|INADEQUATE", EncounterChiefComplaints)) %>%
    transmute(
      EncounterChiefComplaints =
        recode(EncounterChiefComplaints, 
               "WEIGHT CONSULT" = "WEIGHT LOSS CONSULT"))

  ## 2. Create a flag if patients that had a WPV from chief complaints ----
  # First concatenate all relevant chief complaints with the OR pipe
  ccs <- str_c(chief_complaints$EncounterChiefComplaints, collapse = "|")

  temp %<>%
    mutate(
      WPV_CC =
        ifelse(!is.na(EncounterChiefComplaints) &
                 str_detect(temp$EncounterChiefComplaints, ccs) == TRUE, 1, 0),
    )

  ## 3. Clean out the Chief Complaints column since it will no longer be needed
  temp %<>% select(-EncounterChiefComplaints)

  return(temp)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%