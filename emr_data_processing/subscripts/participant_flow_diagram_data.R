#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine 

# Participant Flow Diagram Table
# 06-14-2024

# This script creates a .csv table and places it in the respective data 
# delivery ./table directory. The purpose is to have a table that can be used
# to fill in values into the participant flow diagram.

# ** Could try swapping out ee for visits_post_id, since sometimes ee can get
# modified in the workspace by other subscripts.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Participant Flow Diagram Values for Aim 1A

# Initialize an empty data frame -----------------------------------------------
pt_flow <- NULL

pt_flow <- data.frame(pt_flow) %>%
  mutate(type = NA,
         value = NA,
         source = NA)%>%
  mutate(type = as.character(type),
         value = as.numeric(value),
         source = as.character(source))

# 1. Total number of encounters
# 1. Unique number of encounters ---------------------------------------------------
# Use the visits data frame bc it has been filtered for duplicates
pt_flow <- 
  bind_rows(pt_flow,
    (visits %>% 
       distinct(Arb_EncounterId) %>% 
       summarise(value = n()) %>%
       mutate(type = "Unique encounters",
              source = "visits") %>%
       select(type, everything())))

# 2. Total number of unique patients
# 2. Unique number of patients ----------------------------------------------------
# Use the visits data frame, bc it will give unique # of patients in the data
# set that has been filtered
pt_flow <- 
  bind_rows(pt_flow,   
            (visits %>% 
               distinct(Arb_PersonId) %>% 
               summarise(value = n()) %>%
               mutate(type = "Unique patients",
                      source = "visits") %>%
               select(type, everything())))

# 3. Total number of eligible patients
# 3. Unique number of eligible patients -------------------------------------------
# Use ee_ene_consort, since these records represent those that met eligibility
# criteria of having age >= 18, BMI >= 25, and in the case of EE had a weight
# prioritized visit
# This number is before filtering by date: 385,090
pt_flow <-
  bind_rows(pt_flow,
            (ee_ene_consort %>%
               distinct(Arb_PersonId) %>%
               summarise(value = n()) %>%
               mutate(type = "Unique eligible patients (before date cutoff)",
                      source = "ee_ene_consort") %>%
               select(type, everything())))

# 2.1 Difference between #2 and #3
# 4. Unique number of exclusions ---------------------------------------------------------
# The unique number of patients excluded is given by the number of unique patient
# ids in visits that are not in ee_ene.
# Capture the Arb_PersonIds of those that have an eligible visit
eligible_ids <- ee_ene_consort %>%
  distinct(Arb_PersonId) %>%
  pull(Arb_PersonId)

pt_flow <-
  bind_rows(pt_flow,
            (visits %>%
               filter(!Arb_PersonId %in% eligible_ids) %>%
               distinct(Arb_PersonId) %>%
               summarise(value = n()) %>%
               mutate(type = "Unique patients excluded for ineligibility",
                      source = "visits") %>%
               select(type, everything()))
            )


# 4. Total number of analyzed patients
pt_flow <-
  bind_rows(pt_flow,
            (pp_mod_data %>%
              select(Arb_PersonId) %>%
              distinct() %>%
              summarise(value = n()) %>%
              mutate(type = "Unique patients analyzed (after date cut off)",
                     source = "pp_mod_data") %>%
              select(type, everything()))
            )

# 3.1 difference between #3 and #4
pt_flow <-
  bind_rows(pt_flow,
            (ee_ene_consort %>% 
              filter(EE == 0) %>% 
              select(Arb_PersonId) %>% 
              distinct() %>%
              summarise(value = n()) %>%
              mutate(type = "Excluded for not receiving any discernible care for weight ever",
                      source = "ee_ene_consort") %>%
              select(type, everything()))
  )


# 3.2 exlucded for not having 2 or more weights in each phase
pt_flow <-
  bind_rows(pt_flow,
            (ee_ene_consort %>%
              filter(EE == 1,
                    !Arb_PersonId %in% pp_mod_data$Arb_PersonId) %>%
              select(Arb_PersonId) %>%
              distinct() %>%
              summarise(value = n()) %>%
              mutate(type = "Excluded for not having index + follow up with weight in both phases",
                    source = "ee_ene_consort") %>%
              select(type, everything()))
              )



# 5. Subset the analyzed patients in control
# and then determine the number of patients in each cohort
pt_flow <-
  bind_rows(pt_flow,
            (pp_mod_data %>%
              filter(Intervention == "Control") %>%
              group_by(Arb_PersonId) %>%
              slice_head() %>%
              ungroup() %>%
              group_by(Cohort) %>%
              count() %>%
              ungroup() %>%
              rename(value = n) %>%
              mutate(source = "pp_mod_data",
                    type = str_c("Enrolled in ", Cohort)) %>%
              select(type, everything(), -Cohort))
  )

# Output table to .csv file ----------------------------------------------------
write_csv(pt_flow,here(proj_root_dir, "tables", str_c("pt_flow_aim1a_", date_max, "_", Sys.Date(), ".csv")))
# *****************************************************************************







  