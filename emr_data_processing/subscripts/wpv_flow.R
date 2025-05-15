# Define WPVs by Flowsheets %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The flowsheets data frame from the COMPASS delivery and contains the
# FlowsheetRowIds and FlowsheetTemplateEpicIds for all encounters within the
# time window of the data capture.

# FlowsheetRowEpicIds represent the responses to individual questions given to
# patients. WPV_PW_flow and WPV_OBHPI are set to 1 if the encounter id is found
# to match for the FlowsheetRowEpicIds in flowsheet_ids$FlowsheetRowID.

# FlowsheetTemplateEpicIds represent the aggregated questionnaires that consist
# of the individual questions denoted by FlowsheetRowEpicIds above.
# WPV_WMQ is set to 1 if the FlowsheetTemplateEpicId is in
# c(2108002828, 210800316). These are the correct values discovered by Wenxin
# Wu 9/7/2022 and confirmed by Sheryl Green 9/20/2022.

# Sets the following WPV_* binary variables
# n.b. if weight is na set to 0 is handled by a separate function applied
# after all WPV_* variables are created.
# 1. WPV_PW_flow - also known as the extended weight management questionnaire
#                     but is identified by row epic id
# 2. WPV_OBHPI - identified by row epic id
# 3. WPV_WMQ - weight management questionnaire identified by template epic id
#                     and should overlap with PW_flow

# Requirements:
# flowsheets data frame from compass data delivery
# flowsheet_ids data frame imported from .xlsx

# UPDATES:
# *** There are encounters that match a Template id for WMQ, but not PW_flow
# row Id, should we set these to PW_flow too

# Leigh 05/2024 indicated to combine them when updating tables

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wpv_flow <- function(temp) {
  # Check if flowsheet_ids is loaded and load if not ---------------------------
  # FlowsheetIDs_Obesity_Brief_HPI_PW.csv should already be placed in work
  # space from 01_create_rdata_image.R
  if (exists("flowsheet_ids") == FALSE) {
    flowsheet_ids <- data.table::fread(
      here("emr_data_processing", "working_files", "FlowsheetIDs_Obesity_Brief_HPI_PW.csv"),
      integer64 = "numeric",
      na.strings = c("", "NA")) %>%
      as.data.frame()
  }

  # Set PW_flow, OBHPI, and WMQ row and template ids  --------------------------
  # WPV_PW_flow set to 1 if the Arb_EncounterId is in the flowsheets that match
  # a Flowsheet_RowID associated with PATHWEIGH. WPV_OBHPI is set to 1 if the
  # Arb_EncounterId is in the flowsheets that match a Flowsheet_RowID associated
  # with OBHPI and is also in the intervention period. Set any OBHPI that is
  # also PW_flow to 0 because OBHPI is a subset of PW.


  # Get the Flowsheet Row Ids and Flowsheet Template Ids for the pathweigh and
  # the obhpi individual questions and for the entire weight management
  # questionnaire

  # Identified by an x under pathweigh in flowsheet ids
  # Creates a vector of the PATHWEIGH row Ids
  pw <- flowsheet_ids %>%
    filter(PATHWEIGH == "X") %>%
    select(Flowsheet_RowID)

  # Identified by an x under obesity brief hpi in flowsheet ids
  # Creates a vector of the OBHPI row ids
  obhpi <- flowsheet_ids %>%
    filter(obesity_brief_HPI == "X") %>%
    select(Flowsheet_RowID)

  # Identified my TemplateEpic Ids
  wmq <- data.frame(FlowsheetTemplateEpicId = c(2108002828, 21080028316))


  # 20 of the 29 PW flow ids also correspond to the OBHPI, therefore PW_flow
  # can only be certain from the 9 non-overlapping row ids.
  # Filter pw flowsheet ids to only those that do not overlap with obhpi
  pw %<>%
    filter(!Flowsheet_RowID %in% obhpi$Flowsheet_RowID)

  # all of the 20 obhpi questions are also part of the pathweigh questions
  obhpi %>%
    filter(Flowsheet_RowID %in% pw$Flowsheet_RowID) %>%
    nrow()

  # Get the matching encounter ids ---------------------------------------------
  # Get the encounter ids for each set of pw or obhpi RowEpicids and for the
  # wmq TemplateEpicIds
  pw_enc_ids <-
    flowsheets %>%
    filter(FlowsheetRowEpicId %in% pw$Flowsheet_RowID) %>%
    pull(Arb_EncounterId)

  obhpi_enc_ids <-
    flowsheets %>%
    filter(FlowsheetRowEpicId %in% obhpi$Flowsheet_RowID) %>%
    pull(Arb_EncounterId)

  wmq_enc_ids <-
    flowsheets %>%
    filter(FlowsheetTemplateEpicId %in% wmq$FlowsheetTemplateEpicId) %>%
    pull(Arb_EncounterId)

  # Set WPV_PW_flow variable ---------------------------------------------------
  temp %<>% 
    mutate(WPV_PW_flow = ifelse(Arb_EncounterId %in% pw_enc_ids, 1, 0))


  # Set the WPV_OBHPI variable -------------------------------------------------
  # Multiple records will match for both the 20 OBHPI FlowsheetRowIds and the 9
  # PW FlowsheetRowIds. The OBHPI variable for cases that match for both are set
  # to 0, to distinguish between OBHPI and PW_flow

  ## Create OBHPI IN Steps
  temp %>%
    mutate(WPV_OBHPI = ifelse(Arb_EncounterId %in% obhpi_enc_ids, 1, 0)) %>%
    select(WPV_PW_flow, WPV_OBHPI) %>%
    tbl_summary()

  temp %>%
    mutate(WPV_OBHPI = ifelse(Arb_EncounterId %in% obhpi_enc_ids, 1, 0),
           WPV_OBHPI = ifelse(WPV_OBHPI == 1 & WPV_PW_flow == 1, 0, WPV_OBHPI)
           ) %>%
    select(WPV_PW_flow, WPV_OBHPI) %>%
    tbl_summary()

  temp %<>%
    mutate(WPV_OBHPI = ifelse(Arb_EncounterId %in% obhpi_enc_ids, 1, 0),
           WPV_OBHPI = ifelse(WPV_OBHPI == 1 & WPV_PW_flow == 1, 0, WPV_OBHPI))


  # Set WPV_WMQ (Weight Management Questionnaire) ------------------------------
  # 2108002828, 21080028316 in TemplateEpicId for initial and follow up visits
  # respectively
  temp %<>%
    mutate(WPV_WMQ = ifelse(Arb_EncounterId %in% wmq_enc_ids, 1, 0))

  return(temp)

}
