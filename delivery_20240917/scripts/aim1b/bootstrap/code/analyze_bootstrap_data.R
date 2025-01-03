
# Load libraries --------------------------------------------------------------
pacman::p_load(tidyverse,
  geepack,
  gtsummary)
library(magrittr, include = "%<>%")

# Set the data directory where all of the bootstrap output is stored.
data_dir <- "D:/PATHWEIGH/delivery_20240917/scripts/aim1b/bootstrap/"


# Set the number of bootstraps to read ----------------------------------------
n_bootstraps <- 6

# Load and prep the pp_data set -----------------------------------------------
load("D:/PATHWEIGH/delivery_20240917/data/pp_data_20240917.RData")

# Create a data frame of the observed weights for 6, 12, and 18 months if
# available. These will be used in a series of coalescing function calls
# to retain the observed weight in cases where it is available instead
# of the imputed weight.
observed_weights <- pp_data %>%
  filter(N_months_post_id == 6 |
        N_months_post_id == 12 |
        N_months_post_id == 18) %>%
  group_by(Arb_PersonId, Intervention, N_months_post_id) %>%
  arrange(EncounterDate) %>%
  slice_head() %>% # Grab one row for each 6, 12, and 18 month values when there are more than 1
  ungroup() %>%
  select(Arb_PersonId, Intervention, N_months_post_id, Weight_dv) %>%
  arrange(N_months_post_id) %>%
  pivot_wider(., names_from = N_months_post_id, names_prefix = "weight_observed_", values_from = Weight_dv)


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


# For loop for each boot strapped data set ------------------------------------
# Preallocate a list to hold all results in. Using a list instead of an empty
# data frame that can be appended to as it will hog up memory
all_results <- list() #create an empty list

# For loop, for each i 
for (i in 1:n_bootstraps) {
  # Read files
  con_file <- str_c(data_dir, 'CONTROL', i, '.sas7bdat')
  int_file <- str_c(data_dir, 'INTERVENTION', i, '.sas7bdat')
  
  if (file.exists(con_file)) {
    con <- haven::read_sas(con_file) %>% mutate(Intervention = 'Control')
  } else {
    stop(str_c("Control file " ,i, " does not exist"))
  }
  
  if (file.exists(int_file)) {
    int <- haven::read_sas(int_file) %>% mutate(Intervention = 'Intervention')
  } else {
    stop(str_c("Intervention file ", i, " does not exist."))
  }

  # Stack the two data sets together
  imputed_weights <- bind_rows(con, int)

  # Remove the first 3 characters from id column, rename as Arb_PersonId and then
  # convert to factor to be able to merge data. Remove the id column since it
  # will no longer be used
  imputed_weights %<>%
    mutate(Arb_PersonId = ID) %>%
    mutate(Arb_PersonId = factor(Arb_PersonId)) %>%
    select(-ID)
  
  # Create a data frame of the imputed weights and the observed weights
  weight_loss <- 
  left_join(imputed_weights, 
            observed_weights, 
            by = c("Arb_PersonId", "Intervention"))
  
  # Create a data frame where the imputed weights are added if and only
  # if they don't already exist, to use the observed values if available
  weight_loss <- 
    weight_loss %>%
    mutate(weight_6 = coalesce(weight_observed_6, weight_imputed_6),
           weight_12 = coalesce(weight_observed_12, weight_imputed_12),
           weight_18 = coalesce(weight_observed_18, weight_imputed_18)) %>%
    mutate(weight_loss_12 = ifelse(weight_12 < weight_6, 1, 0),
           weight_loss_18 = ifelse(weight_18 < weight_12, 1, 0))
  
  # Should join each of the new variables to all rows within each phase for each patient
  mod_data <-
    left_join(pp_data %>% filter(IndexVisit == 1, Arb_PersonId %in% weight_loss$Arb_PersonId), 
              weight_loss %>% select(Arb_PersonId, Intervention, weight_loss_12, newid),
              by = c("Arb_PersonId", "Intervention")) %>%
    mutate(newid = factor(newid))
  
  # Logistic Regression Model  
  # specify the 12m weight loss model
  ivs <- " ~ age_45_to_60 + age_gt_60 +  
          sex_m +
          reth_his + reth_blk + reth_asn + reth_oth + reth_ukn +
          year_at_ind1 + year_at_ind2 + year_at_ind3 +
          Weight_bl + Intervention"
  
  mod_spec <- as.formula(str_c("weight_loss_12", ivs))

  # Model for weight loss at 12 months
  model.12 <- geeglm(mod_spec,
    id = newid,
    family = "binomial",
    data = mod_data
  )

  # Save the results of the model output into a transposed and re-arranged
  # data frame
  result <- broom::tidy(model.12) %>%
    select(term, estimate) %>%
    pivot_wider(names_from = 'term', values_from = "estimate") %>%
    mutate(boot_iter = i) %>%
    select(boot_iter, everything())

  # Append the result of the current iteration into the pre defined list.
  all_results[[i]] <- result
  }

# Flattend all_results list
output <- list_rbind(all_results)

# Calculate the SE for each estimate

# Then output to a .csv or .Rdata
