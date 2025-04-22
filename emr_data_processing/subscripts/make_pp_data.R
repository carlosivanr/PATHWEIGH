# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# 08/28/2024
# Make per protocol data
# Outputs:
# pp_mod_data is the data set for modeling without the index visits
# pp_data is the pp_mod dataset but contains the index visits for modeling
# and for creating tables and figures.

# Subset to those with visits in both intervention and control
# Identify and flag those with PW tools visits

# Inputs:
# input is ee_ene data set to be able to also compare ee vs ene in modeling

# Dependencies:
# requires visits_post_id data set as a dependency because it contains all of
# the labs, procedures, meds, and vitals captured at the index visit in each
# phase
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load the mod_data_w_index to utilize the index visits to determine who
# has had a pw visit
pp_data <-
  mod_data[["mod_data_w_ind"]] %>%
  filter(Arb_PersonId %in% (mod_data[["ee"]]$Arb_PersonId))

# Filter data set to less than or equal to 18 months after the index date
# as this is a requirement for the modelling
pp_data <-
  pp_data %>%
  filter(N_months_post_id <= 18)

# Get the patient ids that have 2 or more visit in either phase
ids <- c("Control", "Intervention") %>%
  purrr::map(
    ~ pp_data %>%
      filter(Intervention == .x) %>%
      group_by(Arb_PersonId) %>%
      count() %>%
      ungroup() %>%
      filter(n >= 2)
  )

pp_data <-
  pp_data %>%
    filter(Arb_PersonId %in% ids[[1]]$Arb_PersonId & Arb_PersonId %in% ids[[2]]$Arb_PersonId)


# Identify and flag patients that have at least one pathweigh visit
# First create a list of Arb_PersonIds that have at least one PW_Visit, then use
# that list to create a new time invariant variable (pw) that denotes whether or
# not that person had a pathweigh visit.
pw_ids <-
  pp_data %>%
  filter(PW_Visit == 1) %>%
  distinct(Arb_PersonId) %>%
  pull(Arb_PersonId)

# Make a pathweigh exposed time invariant variable
pp_data %<>%
  mutate(pw = ifelse(Intervention == "Intervention" & Arb_PersonId %in% pw_ids, 1, 0))

# QA checks
# Check that there are no visits in control phase that have evidence of 
# pathweigh tool use.

# Any rows with pw_tools in control? Should be zero.
if ((pp_data %>%
       filter(PW_Visit == 1,
              Intervention == "Control") %>%
       nrow()) != 0) {
  stop("Error! Control phase rows w PATHWEIGH tools.")
}

# Check that all patients in the intervention only have 1 unique value for pw.
# If they have more than 1 unique value, then there's a problem?
if ((pp_data %>%
     filter(Intervention == "Intervention") %>%
     group_by(Arb_PersonId) %>%
     summarise(n = n_distinct(pw)) %>%
     filter(n > 1) %>%
     nrow()) != 0) {
  stop("Error! Intervention phase rows with more than 1 unique pw value.")
}

if ((pp_data %>%
     group_by(Arb_PersonId, Intervention) %>%
     count() %>%
     filter(n == 1) %>%
     nrow()) != 0) {
  stop("Error! Patients with only one visit in a phases detected. Should be 2. Check code")
}

# Create a data set for modeling the pp_data
# The difference between pp_mod and pp_data is that pp_data contains the index
# visits, whereas pp_mod does not. mod_data[["ee"]] is already set up for 
# modeling, so filter it for the encounter ids in pp_data. Since ee does not 
# contain any index visits the result should be the subset of the patients of 
# interest without any index visits
pp_mod_data  <-
  pp_data %>%
  filter(IndexVisit == 0)


# Write out the data set -----------------------------------------------------
# Save mod_data to the data directory on the network drive
save(pp_mod_data, file = here(proj_root_dir, "data", str_c("pp_mod_data_", data_delivery_date, ".RData")))
save(pp_data, file = here(proj_root_dir, "data", str_c("pp_data_", data_delivery_date, ".RData")))