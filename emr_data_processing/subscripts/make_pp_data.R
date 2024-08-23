# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# 08/05/2024
# Make per protocol data
# Outputs:
# pp_mod_data is the data set for modeling
# pp_data is the dataset for making tables, it's a subset of visits_post_id

# Subset to those with visits in both intervention and control
# Identify and flag those with PW tools visits

# Inputs:
# input is ee_ene data set to be able to also compare ee vs ene in modeling

# Dependencies:
# requires visits_post_id data set as a dependency because it contains all of
# the labs, procedures, meds, and vitals captured at the index visit in each 
# phase
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Identify and flag patients that have PW tools --------------------------------
# Use the index visits in addition to the mod data visits to  find who has PW
# tools.

# Extract the index visits from visits post id to utilize the index visits to
# determine whether or not there was any use of pathweigh tools.
# It is not sufficient to filter visits_post_id simply by the Arb_PersonIds in 
# ee because it is possible that the same patient has two or more visits in the
# control phase, but not in the intervention phase. So patient ids in ee must be
# filtered in conjunction with each intervention phase.
ind_visits <- 
  c("Control", "Intervention") %>%
  purrr::map_df(
    ~visits_post_id %>%
      filter(Arb_PersonId %in% (mod_data[["ee"]] %>% 
                                  filter(Intervention == .x) %>% 
                                  distinct(Arb_PersonId) %>% 
                                  pull(Arb_PersonId))) %>%
      filter(IndexVisit == 1,
             Enrolled == 1,
             Intervention.factor == .x)
  )



# Create per protocol data (pp_data)
# Manipulate ind_visits and ee visits for merging, then stack ee data with the 
# respective index visits. n.b. ee will not have all of the labs and procedures
# columns, bc pp_data is only made to determine which patients had PW tools 
# including the index visits
pp_data <- 
  bind_rows(mod_data[["ee"]], 
            (ind_visits %>% 
               mutate(Arb_PersonId = factor(Arb_PersonId),
                      Intervention = Intervention.factor))) %>%
  arrange(Arb_PersonId, EncounterDate)


# Filter pp_data to visits that are less than or equal to 18 months after index
pp_data %<>%
  filter(N_months_post_id <= 18)


# Filter pp_data to patients that have visits in both intervention and control phases
# First create a list of Arb_PersonIds that have two distinct types of 
# Intervention values, then use that list to filter pp_data since having two 
# distinct Intervention values suggests that a given patient has "Control" and 
# "Intervention" rows.
pp_ids <- 
  pp_data %>%
  group_by(Arb_PersonId) %>%
  summarise(n = n_distinct(Intervention)) %>%
  filter(n == 2) %>%
  pull(Arb_PersonId)

pp_data %<>%
  filter(Arb_PersonId %in% pp_ids)


# Identify and flag patients that have at least one pathweigh visit
# First create a list of Arb_PersonIds that have at least one PW_Visit, then use
# that list to create a new time invariant variable, pw, that denotes whether or
# not that person had a pathweigh visit.
pw_ids <- 
  pp_data %>%
  mutate(PW_Visit = if_else(WPV_WMQ == 1 | WPV_IP == 1 | WPV_TH == 1 | WPV_smart == 1, 1, 0)) %>%
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
     nrow()) !=0){
  stop("Error! Control phase rows w PATHWEIGH tools.")
}

# Check that all patients in the intervention only have 1 unique value for pw.
# If they have more than 1 unique value, then there's a problem?
if ((pp_data %>%
     filter(Intervention == "Intervention") %>%
     group_by(Arb_PersonId) %>%
     summarise(n = n_distinct(pw)) %>%
     filter(n > 1) %>%
     nrow()) !=0){
  stop("Error! Intervention phase rows with more than 1 unique pw value.")
}

# Create a data set for modeling the pp_data
# The difference between pp_mod and pp_data is that pp_data contains the index
# visits, whereas pp_mod does not. mod_data[["ee"]] is already set up for 
# modeling, so filter it for the encounter ids in pp_data. Since ee does not 
# contain any index visits the result should be the subset of the patients of 
# interest without any index visits
pp_mod_data  <- 
  mod_data[["ee"]] %>% 
  filter(Arb_EncounterId %in% pp_data$Arb_EncounterId) %>%
  mutate(pw = ifelse(Intervention == "Intervention" & Arb_PersonId %in% pw_ids, 1, 0))

# Create the pw variable in the ee data set that is filtered without the index 
# visits for modeling. The number of non-index rows in pp_data. 
# Should match the number of rows filtered in ee
if ((pp_mod_data %>% nrow()) != (pp_data %>% filter(IndexVisit != 1) %>% nrow())){
  stop("Error in creating pp_mod_dat")
}

# Write out the data set -----------------------------------------------------
# Save mod_data to the data directory on the network drive
save(pp_mod_data, file = here(proj_root_dir, "data", str_c("pp_mod_data_", data_delivery_date, ".RData")))










