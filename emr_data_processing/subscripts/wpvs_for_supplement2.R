# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Department of Family Medicine

# WPVs for Supplement 2:
# Supplement 2 in Leigh's draft submission to NEJM for the pathweigh study EMR
# data primary aims.

# The following code is used to tabulate the number of weight prioritized visits
# in a given clinic, the number of visits where the pathweigh tools were 
# utilized, and the total number of visits in each intervention period

# Inputs:
# Relies on loading the processed visits_post_id data frame that is generated
# after running 02_process_rdata_image.R script.

# Output:
# Generates a .csv file that consists of the required outcome counts grouped
# by clinic.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load Packages ----------------------------------------------------------------
pacman::p_load(here,         # For managing directory paths
               magrittr,     # For the assignment pipe operator %<>%
               tidyverse,    # For data manipulation tools
               gtsummary,    # For tables
               install = FALSE)    

# Load processed data ----------------------------------------------------------

RData <- "20231010.RData"

data_delivery_date <- str_remove(RData, ".RData")

load(here(str_c("data/processed_visits_post_id_", RData)))
     
# Set the input to a dta frame object named "data" to avoid typing visits_post_id
# and filter to only intervention phase visits, since pathweigh tools were
# available only during the intervention period.
data <- visits_post_id

# Filter visits to make the denominator only visits in intervention ------------
data %<>% filter(Intervention == 1)

# Number of visits of the enrolled and eligible patients by intervention
visits_post_id %>% 
  group_by(Intervention) %>% 
  count()

# Count the number of visits in the intervention phase only 
data %>%
  nrow()

# Total Visits -----------------------------------------------------------------
total_visits <- data %>%
  select(DepartmentExternalName, DepartmentEpicId) %>%
  group_by(DepartmentExternalName, DepartmentEpicId) %>%
  count() %>%
  rename(visits_total = n)


# Total WPVs -------------------------------------------------------------------;
total_pw_visits <- data %>%
  filter(WPV_WMQ == 1 | WPV_IP == 1 | WPV_TH == 1 | WPV_smart == 1) %>%
  select(DepartmentExternalName, DepartmentEpicId) %>%
  group_by(DepartmentExternalName, DepartmentEpicId) %>%
  count() %>%
  rename(pw_visits_total = n)


# WPV Visit type ---------------------------------------------------------------;
wpv_vtype <- data %>%
  filter(WPV_IP == 1 | WPV_TH == 1) %>%
  select(DepartmentExternalName, DepartmentEpicId) %>%
  group_by(DepartmentExternalName, DepartmentEpicId) %>%
  count() %>%
  rename(pw_vtype = n)


# WPV Weight management questionnaire -----------------------------------------;
wpv_wmq <- data %>%
  filter(WPV_WMQ == 1) %>%
  select(DepartmentExternalName, DepartmentEpicId) %>%
  group_by(DepartmentExternalName, DepartmentEpicId) %>%
  count() %>%
  rename(pw_wmq = n)

# WPV Smart set ----------------------------------------------------------------;
wpv_smart <- data %>%
  filter(WPV_smart == 1) %>%
  select(DepartmentExternalName, DepartmentEpicId) %>%
  group_by(DepartmentExternalName, DepartmentEpicId) %>%
  count() %>%
  rename(pw_smart = n)


# Generate an output data frame -----------------------------------------------;
output <- 
  left_join(total_visits,
          total_pw_visits,
          by = c("DepartmentExternalName", "DepartmentEpicId")) %>%
  left_join(.,
            wpv_wmq,
            by = c("DepartmentExternalName", "DepartmentEpicId")) %>%
  left_join(.,
            wpv_vtype,
            by = c("DepartmentExternalName", "DepartmentEpicId")) %>%
  left_join(.,
            wpv_smart,
            by = c("DepartmentExternalName", "DepartmentEpicId")) %>%
  ungroup() %>%
  mutate(across(pw_visits_total:pw_smart, ~ ifelse(is.na(.), 0, .)),
         percent_pw_visits = pw_visits_total/visits_total * 100,
         percent_vtype = pw_vtype/pw_visits_total * 100,
         percent_wmq = pw_wmq/pw_visits_total * 100,
         percent_smart = pw_smart/pw_visits_total * 100,
         ) %>%
  # Calculations for percent will introduce NaNs so another mutate statement
  # to clean those is needed after the above mutate statement
  mutate(across(percent_vtype:percent_smart, ~ if_else(is.na(.), 0, .)))
       
# Check the total for the wmq. Should equal the sum of output$pw_wmq
data %>%
  select(WPV_WMQ) %>%
  tbl_summary()


# Load the order file ----------------------------------------------------------;
clinic_index <- read_csv("S:/FM/PATHWEIGH/Quantitative/Projects/working_files/supp2_clinic_order.csv")


# Clean and prep the data file--------------------------------------------------;
clinic_index %>% 
  select(dept) %>%
  mutate(dept = sub("].*", "", dept),   # Remove everything from the right of last bracket
         dept = sub(".*\\[", "", dept), # Remove everything before of the left bracket
         dept = as.numeric(dept)) %>%   # Convert to numeric
  rowid_to_column(., "ind") %>%         # Create the index column
  rename(DepartmentEpicId = dept)       # Rename the column for merging with output


# Write file to working_files directory ---------------------------------------;
left_join(output, clinic_index, by = "DepartmentEpicId") %>%
  arrange(ind) %>%
  select(-ind) %>%
  writexl::write_xlsx(., path = str_c("S:/FM/PATHWEIGH/Quantitative/Projects/working_files/supp2_wpv_", data_delivery_date ,"_.xlsx"))


# Verify data for Depot Hill (spot check)
data %>% filter(DepartmentEpicId == 10416058) %>% 
  mutate(WPV_PW = WPV_IP + WPV_TH, WPV = ifelse(WPV_PW ==1 | WPV_WMQ == 1 | WPV_smart == 1, 1, 0)) %>% 
  select(WPV, WPV_PW, WPV_WMQ, WPV_smart) %>% tbl_summary()


# Remove data from workspace when done -----------------------------------------
rm(data)