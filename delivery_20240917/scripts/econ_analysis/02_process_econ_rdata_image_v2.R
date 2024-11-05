#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. Cu Dept. of Family Medicine

# Process econ rdata image

# Inputs are processed visits
# Econ rdata image

# Capture CPT and HCPCS codes from Compass tables with billing information



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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
# Filter for Arb_EncounterId that is a WPV
# 20231010: There are 130,416 encounters where WPV > 0
# 20240917: There are 175,283 encounters where WPV > 0
wpv_encounters <- 
  visits_post_id %>%
  filter(WPV > 0,
         Censored == 0) %>%
  pull(Arb_EncounterId)


# Create a df to use for merging into other dfs for making tables
wpv_visits <-
  visits_post_id %>%
  filter(WPV > 0, Censored == 0) %>%
  select(Arb_EncounterId, EncounterDate, Insurance, IndexVisit, IndexDate, WPV_PW_flow, WPV_smart, WPV_IP:WPV_TH, Intervention, Intervention.factor, Censored) %>%
  mutate(WPV_vtype = ifelse(WPV_IP == 1 | WPV_TH == 1, 1, 0),
         any_WPV = ifelse(WPV_PW_flow > 0 | WPV_smart > 0 | WPV_vtype > 0, 1, 0)) %>%
  select(-WPV_IP, -WPV_TH)
  
# Define the codes of interest -------------------------------------------------
expected_cpt_hcspcs_codes <- 
  c(seq(97802,97804),
    seq(99202,99215),
    seq(99401,99404),
    seq(97411,97412),
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
either_code <- c(expected_cpt_hcspcs_codes, other_possible_codes) 

# Filter uc health data by the wpvs, omop and epic codes
uchealth_data <- 
  billprocedure %>%
  filter(Arb_EncounterId %in% wpv_encounters) %>%
  filter(OMOPcode %in% either_code | EpicCode %in% either_code)

# How many unique encounter Ids have more than one unique Epic code?
# 20231010: 11,571
# 20240917: 17,385
uchealth_data %>% 
  group_by(Arb_EncounterId) %>% 
  summarise(n_distinct_EpicCode = n_distinct(EpicCode)) %>% 
  filter(n_distinct_EpicCode > 1)

# 4 is the max number of unique codes associated with one encounter
uchealth_data %>% 
  group_by(Arb_EncounterId) %>% 
  summarise(n_distinct_EpicCode = n_distinct(EpicCode)) %>% 
  pull(n_distinct_EpicCode) %>%
  max(.)

# 20231010: 25 Arb_EncounterIds have 4 distinct codes, so it's a very small fraction
# 20240917: 32
uchealth_data %>% 
  group_by(Arb_EncounterId) %>% 
  summarise(n_distinct_EpicCode = n_distinct(EpicCode)) %>% 
  filter(n_distinct_EpicCode == 4) 


# How many unique encounter Ids have more than one unique OMOP code?
# 20231010: 11,097
# 20240917: 16,743
uchealth_data %>% 
  group_by(Arb_EncounterId) %>% 
  summarise(n_distinct_OMOPcode = n_distinct(OMOPcode)) %>% 
  filter(n_distinct_OMOPcode > 1)

# 3 is the max number of unique OMOP codes associated with one encounter
uchealth_data %>% 
  group_by(Arb_EncounterId) %>% 
  summarise(n_distinct_OMOPcode = n_distinct(OMOPcode)) %>% 
  pull(n_distinct_OMOPcode) %>%
  max(.)

# Create Cu medicine data
cumedicine_data <- 
  cumedbilling %>%
  filter(Arb_EncounterId %in% wpv_encounters) %>%
  filter(ProcedureCode %in% either_code)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  
  # Get the ids that match the code in Epic
  matched_code_ids <- 
    uchealth_data %>%
    filter(EpicCode == i) %>%
    pull(Arb_EncounterId)
  
  # Set to one if the code is matched, else leave as NA
  codes.source %<>%
    mutate(!!sym(i) := ifelse(Arb_EncounterId %in% matched_code_ids, 1, !!sym(i)))
  
  # Set the corresponding .source column to "EpicCode" if it's empty
  codes.source %<>%
    mutate(!!sym(src) := ifelse(is.na(!!sym(src)) & (Arb_EncounterId %in% matched_code_ids), "EpicCode", !!sym(src)))
  
  # Get the ids that match the code in OMOP
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


  # Get the ids that match the code in ProcedureCode
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
  select(-Arb_EncounterId, -Insurance, -IndexDate, -Intervention.factor, -Censored, -EncounterDate) %>%
  mutate(across(all_of(either_code), ~ifelse(is.na(.x), 0, .x))) #%>%
  # mutate(across(all_of(str_c(either_code, ".source")), ~ifelse(is.na(.x), 0, .x)))




# Create a table for control -----------------------------------
out_path <- here(str_c("delivery_", delivery), "tables")

control <- 
  tab_data %>% 
  filter(Intervention == 0) %>%
  select(-Intervention, -WPV_PW_flow, -WPV_smart, -WPV_vtype, -any_WPV) %>%
  tbl_summary(by = IndexVisit,
              missing = "no") %>%
  modify_header(label = "**Control**") %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., str_c(out_path, "/cpt_control.xlsx"))

# Create tables for intervention -----------------------------------
# error on G0446.source, perhaps because of all NAs, convert to binary
intervention <- 
  tab_data %>% 
  filter(Intervention == 1) %>%
  select(-Intervention, -WPV_PW_flow, -WPV_smart, -WPV_vtype, -any_WPV,
         -`G0446.source`) %>%
  tbl_summary(by = IndexVisit,
              missing = "no") %>%
  modify_header(label = "**Intervention**") %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., str_c(out_path, "/cpt_intervention.xlsx"))

# *** Does not work
# int_flow <- 
#   tab_data %>% 
#   filter(Intervention == 1,
#          WPV_PW_flow == 1) %>%
#   select(-Intervention, -WPV_smart, -WPV_vtype, -any_WPV) %>%
#   tbl_summary(by = IndexVisit,
#               missing = "no") %>%
#   modify_header(label = "**PW Flow**") %>%
#   gtsummary::as_tibble() %>% 
#   writexl::write_xlsx(., str_c(out_path, "/cpt_int_flow.xlsx"))

# *** Does not work
# int_vtype <- 
#   tab_data %>% 
#   filter(Intervention == 1,
#          WPV_vtype == 1) %>%
#   select(-Intervention, -WPV_PW_flow, -WPV_smart, -any_WPV) %>%
#   tbl_summary(by = IndexVisit,
#               missing = "no") %>%
#   modify_header(label = "**PW Visit Type**") %>%
#   gtsummary::as_tibble() %>% 
#   writexl::write_xlsx(., str_c(out_path, "/cpt_int_vtype.xlsx"))

# *** Does not work
# int_smart <- 
#   tab_data %>% 
#   filter(Intervention == 1,
#          WPV_smart == 1) %>%
#   select(-Intervention, -WPV_PW_flow, -WPV_vtype, -any_WPV) %>%
#   tbl_summary(by = IndexVisit,
#               missing = "no") %>%
#   modify_header(label = "**PW Smart**") %>%
#   gtsummary::as_tibble() %>% 
#   writexl::write_xlsx(., str_c(out_path, "/cpt_int_smart.xlsx"))

# *** Does not work
# int_any <- 
#   tab_data %>% 
#   filter(Intervention == 1,
#          any_WPV == 1) %>%
#   select(-Intervention, -WPV_PW_flow, -WPV_vtype, -WPV_smart, -any_WPV) %>%
#   tbl_summary(by = IndexVisit,
#               missing = "no") %>%
#   modify_header(label = "**Any WPV**") %>%
#   gtsummary::as_tibble() %>% 
#   writexl::write_xlsx(., str_c(out_path, "/cpt_int_any.xlsx"))

# Combine the WPV tables --------------------------------------------
# Performed in Excel outside of R.

# No. of codes per encounter -----------------------------------------
tab_data %>%
  select(all_of(either_code)) %>%
  mutate(code_count = rowSums(., na.rm=T)) %>%
  select(code_count) %>%
  tbl_summary() %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., str_c(out_path, "/cpt_n_codes_per_encounter.xlsx"))



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

# Concatenate all of the cell values, removing NAs
# cd_cmb code combinations
cd_cmb_data %>%
  unite(code_combination, all_of(either_code), na.rm = TRUE) %>%
  mutate(code_combination = ifelse(code_combination == "", NA, code_combination)) %>%
  filter(code_count %in% c(2, 3, 4)) %>%
  tbl_summary(by = code_count) %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., str_c(out_path, "/cpt_code_combinations.xlsx"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Modify code combinations -----------------------------------------------------
# Note: Code combinations result in codes that should not be present
# e.g. billing for 10 minutes and 15 minutes. Need to revert to the highest
# billing so only one is present. Involves codes that begin with 992 and 993
# Create a modified tab_data df where for each encounter with multiple codes
# that begin with either 992 or 993, only one is select. Strategy is to select
# the columns that start with 992 or 993, covert to long, create a prefix,
# group by EncounterId and prefix, arrange descending by code, and slice the top
# row to obtain one code for each 992 and 993 code for each encounter.
# This approach results in losing out on 99206-99210 codes, but those codes were
# not observed in the data to begin with.

# Get the names of the columns in the tab_data df after merging in the EncounterId
tab_data_names <- 
  names(
  bind_cols(
    (wpv_visits %>% select(Arb_EncounterId)), # Get the encounter ids to track records
    tab_data)
  )


# Create modified_tab_data by joining two data frames by encounter id
modified_tab_data <- 
  
  left_join(
    
    # Data frame 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Add encounter Ids to tab_data then remove all 992 and 993 columns, but 
    # keep their respective .source columns
    # *** Uses indexing rather than select() clauses. 
    (bind_cols(
    (wpv_visits %>% select(Arb_EncounterId)), # Get the encounter ids to track records
    tab_data) %>%
    select(all_of(tab_data_names[c(1:9, 24:36, 43:112)]))),
  
    # Data frame 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # slice the max from the 992 and 993 codes after grouping by encounter id and 
    # the prefix
    (bind_cols(
      (wpv_visits %>% select(Arb_EncounterId)), # Get the encounter ids to track records
      tab_data) %>%
      select(Arb_EncounterId, starts_with("992"), starts_with("993"), -ends_with(".source")) %>%
      pivot_longer(!Arb_EncounterId, names_to = "code") %>%
      filter(value == 1) %>%
      mutate(prefix = str_sub(code, 1, 3)) %>%
      group_by(Arb_EncounterId, prefix) %>%
      arrange(desc(code)) %>%
      slice_head() %>%
      ungroup() %>%
      select(-prefix)%>%
      pivot_wider(names_from = code, values_from = value)),
    
    by ="Arb_EncounterId")

# Fill NAs with 0
modified_tab_data %<>%
  mutate(across("99214":"99387", ~ifelse(is.na(.x), 0, .x)))
  

# Code combinations per encounter -----------------------------------------
modified_codes <- either_code[!either_code %in% c("99206", "99207", "99208", "99209", "99210")]

# CR 20230917 data did not have the following codes and would make the subsequent
# code chunk error out
modified_codes <- modified_codes[!modified_codes %in% c("97804", "99215", "G2212", "99397")]

cd_cmb_data <- 
  modified_tab_data %>%
  select(all_of(modified_codes)) %>%
  mutate(code_count = rowSums(., na.rm=T))

# Modify each column so that the 1 are represented by the column name
for (i in modified_codes){
  cd_cmb_data %<>%
    mutate(!!sym(i) := ifelse(!!sym(i) == 1, i, NA))
}

# Concatenate all of the cell values, removing NAs
# cd_cmb code combinations
cd_cmb_data %>%
  unite(code_combination, all_of(modified_codes), na.rm = TRUE) %>%
  mutate(code_combination = ifelse(code_combination == "", NA, code_combination)) %>%
  filter(code_count %in% c(2, 3, 4)) %>%
  tbl_summary(by = code_count) %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., str_c(out_path, "/modified_cpt_code_combinations.xlsx"))


