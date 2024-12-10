# PART 2. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# After Imputed values from SAS are saved as .csv, they need to be merged into
# covariate data for modeling.

# INPUTS: 
# 1. pp_data for the 9,358 patients from the Aim1A analyses
# 2. Two data sets of the imputed weight values, one each for 
#    control/intervention
# END HEADER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pacman::p_load(tidyverse,
               geepack)

library(magrittr, include = "%<>%")

# Read in the ee data ---------------------------------------------------------
load("D:/PATHWEIGH/delivery_20240917/data/pp_data_20240917.RData")

# Read in the imputed data ----------------------------------------------------
# read in imputation control
con <- read_csv("D:/PATHWEIGH/delivery_20240917/data/imputation_control.csv") %>%
  mutate(Intervention = "Control")

# read in impuation intervention
int <- read_csv("D:/PATHWEIGH/delivery_20240917/data/imputation_intervention.csv") %>%
  mutate(Intervention = "Intervention")

# Stack the two data sets together
imputed_weights <- bind_rows(con, int)

# Remove the first 3 characters from id column, rename as Arb_PersonId and then
# convert to factor to be able to merge data, remove the id column since it
# will no longer be used
imputed_weights %>%
  mutate(Arb_PersonId = str_replace(id, "ID ", "")) %>%
  mutate(Arb_PersonId = factor(Arb_PersonId)) %>%
  select(-id)
                   

# Create a data frame of the observed weight for 6, 12, and 18 months if
# available. These will be used in a series of coalescing function calls
# to retain the observed weight in cases where it is available instead
# of the imputed weight.
observed_weights <- pp_data %>%
  filter(N_months_post_id == 6 |
         N_months_post_id == 12 |
         N_months_post_id == 18) %>%
  group_by(Arb_PersonId, Intervention) %>%
  slice_head() %>%
  ungroup() %>%
  select(Arb_PersonId, Intervention, N_months_post_id, Weight_dv) %>%
  arrange(N_months_post_id) %>%
  pivot_wider(., names_from = N_months_post_id, names_prefix = "weight_observed_", values_from = Weight_dv)


# Create a data frame where the observed weights are merged into the imputed weights.
# Because it's possible for a single patient to not have a 6, 12, or 18 month weight,
# the observed weights data frame will be smaller because those patients won't be
# represented in this dataframe. When merging is achieved, thes patients will show
# up as rows with missing values for the observe 6m, 12m, and 18m weight.
weight_loss <- 
  left_join(imputed_weights, 
            observed_weights, 
            by = c("Arb_PersonId", "Intervention")) %>%
  mutate(weight_6 = coalesce(weight_observed_6, weight_imputed_6),
                  weight_12 = coalesce(weight_observed_12, weight_imputed_12),
                  weight_18 = coalesce(weight_observed_18, weight_imputed_18)) %>%
  mutate(weight_loss_12 = ifelse(weight_12 < weight_6, 1, 0),
         weight_loss_18 = ifelse(weight_18 < weight_12, 1, 0))


# Merge in the coalesced 12m and 18m indicator variables ----------------------
# These are the binary indicators to be used as outcomes for the logistic
# regression models.
pp_data %<>%
  left_join(weight_loss %>% select(Arb_PersonId, Intervention, weight_loss_12, weight_loss_18),
            by = c("Arb_PersonId", "Intervention"))


# Create binary indicator variables for the age, sex, race, and year at index
# categorical variables to be used in modeling.
pp_data %<>%
  mutate(age_lt_45 = ifelse(Age_cat == "<=45", 1, 0),
         age_45_to_60 = ifelse(Age_cat == "45-60", 1, 0),
         age_gt_60 = ifelse(Age_cat == ">60", 1, 0),
         sex_m = ifelse(Sex == "Male", 1, 0),
         sex_f = ifelse(Sex == "Female", 1, 0),
         reth_nhw = ifelse(Race_Ethnicity == "Non-Hispanic White", 1, 0),
         reth_his = ifelse(Race_Ethnicity == "Hispanic or Latino", 1, 0),
         reth_blk = ifelse(Race_Ethnicity == "Black or African American", 1, 0),
         reth_asn = ifelse(Race_Ethnicity == "Asian", 1, 0),
         reth_oth = ifelse(Race_Ethnicity == "Other", 1, 0),
         reth_ukn = ifelse(Race_Ethnicity == "Unknown", 1, 0),
         year_at_ind0 = ifelse(Year_at_ind == "Year0", 1, 0),
         year_at_ind1 = ifelse(Year_at_ind == "Year1", 1, 0),
         year_at_ind2 = ifelse(Year_at_ind == "Year2", 1, 0),
         year_at_ind3 = ifelse(Year_at_ind == "Year3", 1, 0))

# Create a data set with only the index visits, to use as the modeling data set
# since it will contain one row per patient in each phase
mod_data <- pp_data %>% filter(IndexVisit == 1)

# What sort of model are we going to use? -------------------------------------
# This is the specification of the lmer model, with the weight_loss_12 set as
# the dependent variable, what to do with dependent variables like Arb_PersonId
# or does the analysis have to be stratified, what about the slope and interaction
# terms?
glm(weight_loss_12 ~

  # age
  age_45_to_60 + age_gt_60 +

  # sex
  sex_m +

  # race ethnicity
  reth_his + reth_blk + reth_asn + reth_oth + reth_ukn +

  # year at index
  year_at_ind1 + year_at_ind2 + year_at_ind3 +

  # Weight at baseline
  Weight_bl + slope1 +

  # Slope1 is the same as Phase/Intervention group
  slope1:N_days_post_id + slope1:N_days_post_180 +

  # Slope2 is the opposite
  slope2:N_days_post_id + slope2:N_days_post_180 +

  # Clustering, convergence issues with both dept and personid
  # (1| DepartmentExternalName) + (1| Arb_PersonId),
  (1| Arb_PersonId),

  # Input data frame
  data = mod_data
  )