# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# old code didn't work either because of change in gt_summary, data delivery, or
# both. Also older code was restricted to wpv encounters only
 # Load Packages ----------------------------------------------------------------
pacman::p_load(here,         # For managing directory paths
  magrittr,     # For the assignment pipe operator %<>%
  tidyverse,    # For data manipulation tools
  furrr,        # For parallel processing
  data.table,   # For reading .csv files
  openxlsx,     # For reading .xlsx files
  tictoc,       # For timing and benchmarking functions
  gtsummary,    # For tables
  flextable,
  install = FALSE)


# Specify parameters -----------------------------------------------------------
RData <- "20240917.RData"

# Set the path to the emr_data_processing directory to load scripts/functions
# that are shared along all separate data delivery dates
data_delivery_date <- str_remove(RData, ".RData")

# Specify delivery date
delivery <- data_delivery_date

# Load visits_post_id data -----------------------------------------------------
load(here(str_c("delivery_", delivery), str_c("data/", "visits_post_id_", delivery, ".RData")))

# Load econ data ---------------------------------------------------------------
# econ data are cu medicine and uc health tables
load(here(str_c("delivery_", delivery), str_c("data/", "econ_", delivery, ".RData")))


# Create a vector of encounterIds that are WPVs --------------------------------
# Only the encounters that were designated a WPV are of interest
wpv_encounters <-
  visits_post_id %>%
  filter(WPV > 0,
         Censored == 0) %>%
  pull(Arb_EncounterId)


# Create a df to use for merging into other dfs for making tables
# Create a df with only the WPV visits
wpv_visits <-
  visits_post_id %>%
  filter(WPV > 0, Censored == 0) %>%
  select(Arb_EncounterId, EncounterDate, Insurance, IndexVisit, IndexDate, WPV_PW_flow, WPV_smart, WPV_IP:WPV_TH, Intervention, Intervention.factor, Censored) %>%
  mutate(WPV_vtype = ifelse(WPV_IP == 1 | WPV_TH == 1, 1, 0),
         any_WPV = ifelse(WPV_PW_flow > 0 | WPV_smart > 0 | WPV_vtype > 0, 1, 0)) %>%
  select(-WPV_IP, -WPV_TH)


# Define the codes of interest -------------------------------------------------
expected_cpt_hcspcs_codes <-
  c(seq(97802, 97804),
    seq(99202, 99215),
    seq(99401, 99404),
    seq(97411, 97412),
    99078,
    "G0270",
    "G0271",
    "G0446",
    "G0447",
    "G0473",
    "G2212")

other_possible_codes <-
  c(seq(99385, 99387),
    seq(99395, 99397),
    seq(96150, 96155),
    seq(98960, 98962),
    "S0315", "S0316", "S0317",
    "S9445", "S9446",
    "S9449",
    "S9452",
    "S9470")

# Combine codes  ---------------------------------------------------------------
# Either code is the concatenated expected or other possible codes of interest
either_code <- c(expected_cpt_hcspcs_codes, other_possible_codes) 

# Filter uc health and cu medicine data only to encounters in visits post id
# uses
uchealth_data <- 
  billprocedure %>%
  filter(Arb_EncounterId %in% visits_post_id$Arb_EncounterId) %>%
  filter(OMOPcode %in% either_code | EpicCode %in% either_code)

# Create Cu medicine data
# Uses procedureCode colume to search for codes of interest
cumedicine_data <- 
  cumedbilling %>%
  filter(Arb_EncounterId %in% visits_post_id$Arb_EncounterId) %>%
  filter(ProcedureCode %in% either_code)


# ----------------------------------------------------------------
# The following code section is designed to understand the source of each
# procedure code. 

# Make columns for each code of interest, and then append a .source to the columns
# Make a matrix of NAs that is the number of rows in wpv_visits by the number
# of codes of interest
mat <- data.frame(matrix(NA, dim(wpv_visits)[1], length(either_code)))

# Set the names of mat according to the codes of interest
names(mat) <- either_code

# Mat a 2nd matrix for the .source columns
mat2 <- data.frame(matrix(NA, dim(wpv_visits)[1], length(either_code)))

names(mat2) <- str_c(either_code, ".source")

# Join mat and mat2 together
codes.source <- bind_cols(mat, mat2)

# Join in the Arb_EncounterId from wpv_visits
# Creates a data frame of all encounter_ids that had a WPV and a set of columns
# that will be used to create a binary indicator for wether or not that code 
# was used and where the code was found

codes.source <- bind_cols((wpv_visits %>% select(Arb_EncounterId)), codes.source)

# For each code in either_code, filter data for those that match that type of 
# code, then pull the Arb Encounter ID.
# If the arb_encounter ID is in the pulled vector, then fill in the cell with 
# a 1 and the .source with the code type if it's empty. If the cell for .source
# is not empty, then concatenate the existing string and the new string for 
# code type
for (i in either_code){
  print(i)
  
  # Set a variable to the corresponding .source column
  src <- str_c(i, ".source")
  
  # Get the encounter ids that match the code in EpicCode for UC health data
  matched_code_ids <- 
    uchealth_data %>%
    filter(EpicCode == i) %>%
    pull(Arb_EncounterId)
  
  # For the ith code, set the cell to 1 if the code is matched, else leave as NA
  codes.source %<>%
    mutate(!!sym(i) := ifelse(Arb_EncounterId %in% matched_code_ids, 1, !!sym(i)))
  
  # Set the corresponding .source column to "EpicCode" if it's empty
  codes.source %<>%
    mutate(!!sym(src) := ifelse(is.na(!!sym(src)) & (Arb_EncounterId %in% matched_code_ids), "EpicCode", !!sym(src)))
  
  # Get the encounter ids that match the code in OMOPCode for UC health data
  # *** n.b. overwrites previous definition of matched_code_ids
  matched_code_ids <- 
    uchealth_data %>%
    filter(OMOPcode == i) %>%
    pull(Arb_EncounterId)
  
  # Set to one if the code is matched, else leave as NA
  codes.source %<>%
    mutate(!!sym(i) := ifelse(Arb_EncounterId %in% matched_code_ids, 1, !!sym(i)))
  
  
  # Set the corresponding .source column to "OMOPcode" if it's empty
  codes.source %<>%
    mutate(!!sym(src) := ifelse(is.na(!!sym(src)) & (Arb_EncounterId %in% matched_code_ids), "OMOPcode", 
                                ifelse(!is.na(!!sym(src)), str_c(!!sym(src), "OMOPcode"), !!sym(src))))


  # Get the ids that match the code in ProcedureCode for CU medicine data
  # *** n.b. overwrites previous definition of matched_code_ids
  matched_code_ids <- 
    cumedicine_data %>%
    filter(ProcedureCode == i) %>%
    pull(Arb_EncounterId)
  
  # Set to one if the code is matched, else leave as NA
  codes.source %<>%
    mutate(!!sym(i) := ifelse(Arb_EncounterId %in% matched_code_ids, 1, !!sym(i)))
  
  # Set the corresponding .source column to "ProcCode" if it's empty
  codes.source %<>%
    mutate(!!sym(src) := ifelse(is.na(!!sym(src)) & (Arb_EncounterId %in% matched_code_ids), "ProcCode", 
                                ifelse(!is.na(!!sym(src)), str_c(!!sym(src), "ProcCode"), !!sym(src))))
  
  
}

# Create tab_data. The main data frame for creating tables ---------------------
# Only the code columns and not the .source columns are converted to binary
# values. The .source columns are left with missing values.
tab_data <- 
  left_join(wpv_visits, codes.source, by = "Arb_EncounterId") %>%
  mutate(IndexVisit = ifelse(IndexVisit == 1, "Index Visits", "Subsequent Visits")) %>%
  # select(-Arb_EncounterId, -Insurance, -IndexDate, -Intervention.factor, -Censored, -EncounterDate) %>%
  select(-Insurance, -IndexDate, -Intervention.factor, -Censored, -EncounterDate) %>%
  mutate(across(all_of(either_code), ~ifelse(is.na(.x), 0, .x)))


# Code combinations per encounter -----------------------------------------
cd_cmb_data <- 
  tab_data %>%
  select(all_of(either_code)) %>%
  mutate(code_count = rowSums(., na.rm=T))

# Modify each column so that the value in each cell is turned into a string
# of the code instead of a binary indicator
# Then all of the strings will be concatenated to get combinations.
for (i in either_code){
  cd_cmb_data %<>%
    mutate(!!sym(i) := ifelse(!!sym(i) == 1, i, NA))
}

# Concatenate all of the cell values, removing NAs, to create the column
# of code combination
cd_cmb_data %<>%
  unite(code_combination, all_of(either_code), na.rm = TRUE) %>%
  mutate(code_combination = ifelse(code_combination == "", NA, code_combination))

# Merge wpv_visits with the code combination data
# This would be the data frame to use if the denominator is to be the number of WPVs
wpv_visits <- bind_cols(wpv_visits, cd_cmb_data)


# This would be the data frame to use if the denominator is to be all of the eligible and enrolled visits
visits_post_id <- left_join(visits_post_id, 
          (wpv_visits %>% select(Arb_EncounterId, code_combination)),
          by = "Arb_EncounterId")


# Will need 6 subtables
# Control Index Visits
tab1 <- wpv_visits %>%
  filter(Intervention == 0,
         IndexVisit == 1) %>%
  select(code_combination) %>%
  mutate(code_combination = fct_na_value_to_level(code_combination, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Index WPV Control; N = {n}") %>%
  as_tibble()

tab2 <- wpv_visits %>%
  filter(Intervention == 0,
         IndexVisit == 0) %>%
  select(code_combination) %>% 
  mutate(code_combination = fct_na_value_to_level(code_combination, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Other WPV Control; N = {n}") %>%
  as_tibble()

tab3 <- wpv_visits %>%
  filter(Intervention == 1,
         IndexVisit == 1) %>%
  select(code_combination) %>% 
  mutate(code_combination = fct_na_value_to_level(code_combination, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Index WPV Intervention; N = {n}") %>%
  as_tibble()

tab4 <- wpv_visits %>%
  filter(Intervention == 1,
         IndexVisit == 0) %>%
  select(code_combination) %>% 
  mutate(code_combination = fct_na_value_to_level(code_combination, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Other WPV Intervention; N = {n}") %>%
  as_tibble()

tab5 <- wpv_visits %>%
  filter(Intervention == 1,
         IndexVisit == 1,
        any_WPV == 1) %>%
  select(code_combination) %>% 
  mutate(code_combination = fct_na_value_to_level(code_combination, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Index WPV w/ PW Intervention; N = {n}") %>%
  as_tibble()

tab6 <- wpv_visits %>%
  filter(Intervention == 1,
         IndexVisit == 0,
        any_WPV == 1) %>%
  select(code_combination) %>% 
  mutate(code_combination = fct_na_value_to_level(code_combination, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Other WPV w/ PW Intervention; N = {n}") %>%
  as_tibble()

out_path <- here(str_c("delivery_", delivery), "tables")

left_join(tab1, tab2, by = "**Characteristic**") %>%
  left_join(tab3, by = "**Characteristic**") %>%
  left_join(tab4, by = "**Characteristic**") %>%
  left_join(tab5, by = "**Characteristic**") %>%
  left_join(tab6, by = "**Characteristic**") %>%
  writexl::write_xlsx(., str_c(out_path, "/cpt_code_combinations.xlsx"))




# 2/14/2025 -------------------------------------------------------------------
# Will need 6 subtables
# Control Index Visits
tab1 <- wpv_visits %>%
  filter(Intervention == 0,
         IndexVisit == 1) %>%
  select(Insurance) %>%
  mutate(Insurance = fct_na_value_to_level(Insurance, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Index WPV Control; N = {n}") %>%
  as_tibble()

tab2 <- wpv_visits %>%
  filter(Intervention == 0,
         IndexVisit == 0) %>%
  select(Insurance) %>% 
  mutate(Insurance = fct_na_value_to_level(Insurance, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Other WPV Control; N = {n}") %>%
  as_tibble()

tab3 <- wpv_visits %>%
  filter(Intervention == 1,
         IndexVisit == 1) %>%
  select(Insurance) %>% 
  mutate(Insurance = fct_na_value_to_level(Insurance, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Index WPV Intervention; N = {n}") %>%
  as_tibble()

tab4 <- wpv_visits %>%
  filter(Intervention == 1,
         IndexVisit == 0) %>%
  select(Insurance) %>% 
  mutate(Insurance = fct_na_value_to_level(Insurance, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Other WPV Intervention; N = {n}") %>%
  as_tibble()

tab5 <- wpv_visits %>%
  filter(Intervention == 1,
         IndexVisit == 1,
        any_WPV == 1) %>%
  select(Insurance) %>% 
  mutate(Insurance = fct_na_value_to_level(Insurance, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Index WPV w/ PW Intervention; N = {n}") %>%
  as_tibble()

tab6 <- wpv_visits %>%
  filter(Intervention == 1,
         IndexVisit == 0,
        any_WPV == 1) %>%
  select(Insurance) %>% 
  mutate(Insurance = fct_na_value_to_level(Insurance, level = "Unknown")) %>%
  tbl_summary() %>%
  modify_header(., stat_0 = "Other WPV w/ PW Intervention; N = {n}") %>%
  as_tibble()

left_join(tab1, tab2, by = "**Characteristic**") %>%
  left_join(tab3, by = "**Characteristic**") %>%
  left_join(tab4, by = "**Characteristic**") %>%
  left_join(tab5, by = "**Characteristic**") %>%
  left_join(tab6, by = "**Characteristic**") %>%
  writexl::write_xlsx(., str_c(out_path, "/insurance_classifications.xlsx"))