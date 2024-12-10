library(magrittr, include = "%<>%")
pacman::p_load(tidyverse,
               gtsummary,
               here)


    
# Load data -------------------------------------------------------------------
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


names(pp_data)

# Can you get the quartiles of the two datasets? I may only need the 9358 data. Also, the only variables I need are patid, number of months since index, weight. Many thanks

# Get quartiles ---------------------------------------------------------------
quartiles <- 
  c("Control", "Intervention") %>%
  map(
    ~ pp_data %>%
      filter(Intervention == .x) %>%
      pull(N_months_post_id) %>%
      quantile(., probs = c(0,0.25,0.5,0.75,1))
  )


bind_cols(data.frame("Control" = quartiles[[1]]),
  data.frame("Intervention" = quartiles[[2]])) %>%
  rownames_to_column(., "Quartile") %>%
  write_csv("D:/PATHWEIGH/delivery_20240917/data/quartiles_aim1b.csv", na = "")

# Output to SAS ---------------------------------------------------------------
# outputs to a .csv file
pp_data %>%
  mutate_if(is.Date, as.character) %>%
  mutate_if(is.difftime, as.numeric) %>%
  select(Arb_PersonId, Intervention, N_months_post_id, Weight_kgs) %>%
  write_csv("D:/PATHWEIGH/delivery_20240917/data/data_aim1b.csv", na = "")


# Check the ee subset with index, those that have at least one subsequent weight
# with a value.
# Load data -------------------------------------------------------------------
load("D:/PATHWEIGH/delivery_20240917/data/mod_data_w_ind_20240917.RData")

# Gives 59,459
mod_data_w_ind %>%
  filter(EE == 1) %>%
  drop_na(Weight_kgs) %>%
  select(Arb_PersonId) %>%
  n_distinct()

bind_rows(
  mod_data_w_ind %>%
    filter(EE == 1, Intervention == "Control") %>%
    drop_na(Weight_kgs) %>%
    select(Arb_PersonId),
  mod_data_w_ind %>%
    filter(EE == 1, Intervention == "Intervention") %>%
    drop_na(Weight_kgs) %>%
    select(Arb_PersonId)
) %>%
  n_distinct()

ee <- 
  mod_data_w_ind %>%
  filter(EE == 1)

ee %>%
  mutate_if(is.Date, as.character) %>%
  mutate_if(is.difftime, as.numeric) %>%
  write_csv("D:/PATHWEIGH/delivery_20240917/data/data_aim1b_ee_12-05-2024.csv", na = "")