# PART 1. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# Prepare a data set for doing the imputation
# Writes to a .csv file that is then used to perform the imputation of 6m, 12m,
# and 18m weight values in SAS.

# When imputation is completed, the remaining data manipulations and analyses
# are resumed in Part 2.
# END HEADER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load packages ---------------------------------------------------------------
library(magrittr, include = "%<>%")
pacman::p_load(tidyverse,
               gtsummary,
               here)


# Load data -------------------------------------------------------------------
# Using the PP_data for Aim1B because the sample of interest is the 9,358 
# patients analyzed in Aim1A. These are patients that have at least one follow
# up visit in both control and intervention phases. This datset contains all
# visits with a recorded weight value including the index for each phase
load("D:/PATHWEIGH/delivery_20240917/data/pp_data_20240917.RData")


# Check that index visits are included ----------------------------------------
table(pp_data$IndexVisit)

# Check the number of patients in each phase ----------------------------------
c("Control", "Intervention") %>%
  map(
  ~ pp_data %>%
  filter(Intervention == .x) %>%
  select(Arb_PersonId) %>%
  n_distinct()
  )

# Display the names of the dataframe
names(pp_data)


# Get quartiles ---------------------------------------------------------------
# Quartiles are used to establish the knots in the multiple imputation step
quartiles <- 
  c("Control", "Intervention") %>%
  map(
    ~ pp_data %>%
      filter(Intervention == .x) %>%
      pull(N_months_post_id) %>%
      quantile(., probs = c(0,0.25,0.5,0.75,1))
  )

# Save the quartiles to a .csv file
bind_cols(data.frame("Control" = quartiles[[1]]),
  data.frame("Intervention" = quartiles[[2]])) %>%
  rownames_to_column(., "Quartile") %>%
  write_csv("D:/PATHWEIGH/delivery_20240917/data/quartiles_aim1b.csv", na = "")

# Output to SAS ---------------------------------------------------------------
# outputs to a .csv file, but need to set date columns as character, difftime
# columns to numeric, before writing to ensure compatibility with SAS. When
# writing to .csv file, set na = "" to ensure missing values are imported
# properly.
pp_data %>%
  mutate_if(is.Date, as.character) %>%
  mutate_if(is.difftime, as.numeric) %>%
  select(Arb_PersonId, Intervention, N_months_post_id, Weight_kgs) %>%
  write_csv("D:/PATHWEIGH/delivery_20240917/data/data_aim1b.csv", na = "")


# SECTION 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Check the ee subset with index, those that have at least one subsequent weight
# with a value.
# Load data -------------------------------------------------------------------

# load("D:/PATHWEIGH/delivery_20240917/data/mod_data_w_ind_20240917.RData")

# # Gives 59,459
# mod_data_w_ind %>%
#   filter(EE == 1) %>%
#   drop_na(Weight_kgs) %>%
#   select(Arb_PersonId) %>%
#   n_distinct()

# bind_rows(
#   mod_data_w_ind %>%
#     filter(EE == 1, Intervention == "Control") %>%
#     drop_na(Weight_kgs) %>%
#     select(Arb_PersonId),
#   mod_data_w_ind %>%
#     filter(EE == 1, Intervention == "Intervention") %>%
#     drop_na(Weight_kgs) %>%
#     select(Arb_PersonId)
# ) %>%
#   n_distinct()

# ee <- 
#   mod_data_w_ind %>%
#   filter(EE == 1)

# ee %>%
#   mutate_if(is.Date, as.character) %>%
#   mutate_if(is.difftime, as.numeric) %>%
#   write_csv("D:/PATHWEIGH/delivery_20240917/data/data_aim1b_ee_12-05-2024.csv", na = "")