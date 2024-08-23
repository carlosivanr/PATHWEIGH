# Process labs, procedures, and referrals for safety officer table %%%%%%%%%%%%%
# For the safety officer table only procedures and labs are required. Not the 
# referrals, screeners, etc.

# Captures labs and procedures for the visits in the input data.

# Requires the following tables
# - procedures
# - labs
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

labs_procedures_sot <- function(temp){
  tic()
  
  # Check that all the required data frames are loaded in the workspace.
  dfs <- c("procedure", "labs", "flowsheets", "referrals")  
  if (sum(map_lgl(dfs, exists)) != length(dfs)){
    stop("Not all required data frames are loaded into the workspace. Check that procedure, labs, screener, and referrals are in properly named.")
    }
    
  # distinct_pts_x_ind, needed downstream for procedures
  # These are unique combinations of PersonIds and Index Dates used to filter
  # the labs and procedures tables for each intervention.
  distinct_pts_x_ind <- temp %>% 
    select(Arb_PersonId, IndexDate) %>%
    distinct()
  
  # Test to see if the O2CPAPBIPA column is already present. If not present, 
  # proceed to creating it. Prevents creation of column in case the function
  # had already been implemented. The if statement test can be expanded to 
  # include screeners, bariatric, and/or referrals columns. Currently used as 
  # a general test for all of the above columns.
  if (sum(grepl("O2CPAPBIPAP", names(temp))) == 0){
    
    # Procedures (O2, CPAP, BIPAP) ---------------------------------------------
    # Create a data frame where procedures are joined with distinct_pts_x_ind by patient ID,
    # then filtered for those with O2, CPAP, or BIPAP. Then procedures
    # are grouped and counted in a summarise statement.

    O2CPAPBIPAP_data <- 
      left_join(distinct_pts_x_ind, 
                select(procedure, Arb_PersonId, ProcedureDate, Category), 
                by="Arb_PersonId") %>%
      filter(Category=="CPAP/BIPAP/O2") %>%
      group_by(Arb_PersonId) %>%
      arrange(ProcedureDate) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      group_by(Arb_PersonId, IndexDate) %>%
      summarise(O2CPAPBIPAP = n(), .groups = "rowwise") %>% # Could slice_head/max here instead of summarise
      mutate(O2CPAPBIPAP = 1)

    
    # Merge temp and O2CPAPBIPAP and recode NAs to 0
    temp <- 
      left_join(temp, O2CPAPBIPAP_data, by=c("Arb_PersonId", "IndexDate")) %>%
      mutate(O2CPAPBIPAP = ifelse(is.na(.$O2CPAPBIPAP), 0, O2CPAPBIPAP))
  
    
    # Labs  --------------------------------------------------------------------
    #A1c (%),TSH (mIU/L),TG (mg/dl),HDL (mg/dl; * note decrease in value),
    #AST (U/L),ALT (U/L),eGFR (ml/min/1.73 m2; * note decrease in value)
    
    # Restrict only to labs of interest
    str_labs <- c("A1C","TSH","Triglycerides","HDL",
                  "AST","ALT","eGFR","Cystatin C")
    
    # Create a data frame consisting of Arb_PersonId, IndexDate, and columns of 
    # the labs of interest filled with their NumericValue. Select the columns in 
    # labs, filter rows that match the labs of interest. Drop any rows that
    # have NA as a NumericValue.
    # Identify the labs that are closest to the index date, remove the
    # LabCollectionDate column and then convert to wide.
    labs_data_wide <- 
      left_join(distinct_pts_x_ind,
                select(labs, Arb_PersonId, LabCollectionDate, Category, NumericValue),
                by = "Arb_PersonId") %>%
      filter(Category %in% str_labs) %>%
      # filter(Category %in% c("Cystatin C") ) %>%
      drop_na(NumericValue) %>%
      group_by(Arb_PersonId, Category) %>% #added the group by category, previous version did not have category
      arrange(LabCollectionDate) %>%
      slice_tail(n = 1) %>%
      #filter(Category %in% c("Cystatin C") ) %>%
      ungroup() %>%
      group_by(Arb_PersonId, IndexDate, Category) %>%
      slice(which.min(abs(IndexDate - LabCollectionDate))) %>%
      select(-LabCollectionDate) %>%
      spread(Category, NumericValue)
  
    # Merge with temp
    temp <- left_join(temp, labs_data_wide, by = c("Arb_PersonId","IndexDate"))
    
    toc()
    return(temp)
  }
}