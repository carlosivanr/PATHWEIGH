#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine 

# Participant Flow Diagram Table
# 08-05-2024

# This script creates a .csv table and places it in the respective data 
# delivery ./table directory. The purpose is to have a table that can be used
# to fill in values into the participant flow diagram.

# ** Could try swapping out ee for visits_post_id, since sometimes ee can get
# modified in the workspace by other subscripts.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Load pp_data -----------------------------------------------------------------


# Initialize an empty data frame -----------------------------------------------
pt_flow <- NULL

# Set up a structure in pt_flow to fill in with values
pt_flow <- data.frame(pt_flow) %>%
  mutate(type = NA,
         value = NA)%>%
  mutate(type = as.character(type),
         value = as.numeric(value))

# 1. Unique number of encounters ---------------------------------------------------
# Use the visits data frame because it is bounded by the study start date and by
# the latest deliver date
pt_flow <- 
  bind_rows(pt_flow,
    (visits %>% 
       distinct(Arb_EncounterId) %>% 
       summarise(value = n()) %>%
       mutate(type = "Unique encounters") %>%
       select(type, everything())))

# 2. Unique number of patients ----------------------------------------------------
# Use the visits data frame
pt_flow <- 
  bind_rows(pt_flow,   
            (visits %>% 
               distinct(Arb_PersonId) %>% 
               summarise(value = n()) %>%
               mutate(type = "Unique patients") %>%
               select(type, everything())))

# 3. Unique number of eligible patients -------------------------------------------
# Eligibility defined by Age, BMI, in addition to restrictions on records 
# imposed on Height and Weight
pt_flow <-
  bind_rows(pt_flow,
            (visits %>%
               filter(Eligible == 1) %>%
               distinct(Arb_PersonId) %>%
               summarise(value = n()) %>%
               mutate(type = "Unique eligible patients") %>%
               select(type, everything())))


# Capture the Arb_PersonIds of those that have an eligible visit
eligible_ids <- visits %>%
  filter(Eligible == 1) %>%
  distinct(Arb_PersonId) %>%
  pull(Arb_PersonId)


# 4. Unique number of enrolled patients ----------------------------------------
pt_flow <- 
  bind_rows(pt_flow,
            pp_data %>%
            distinct(Arb_PersonId) %>%
            summarise(value = n()) %>%
            mutate(type = "Unique enrolled patients") %>%
            select(type, everything())
  )

# Eligible and enrolled:
# for 2024-03-26 76,057
# means that 359998 - 76057 = 283,941 did not have any discernible care for weight
# 76,057 were eligible and enrolled
visits_post_id %>%
  distinct(Arb_PersonId) %>%
  nrow()


# How many have two or more in each phase, not censored
# 54,197 unique patients in mod_data[["ee"]]
# Means that 76,057 - 54,197 = 21,860 were excluded
mod_data[["ee"]] %>%
  distinct(Arb_PersonId) %>%
  nrow()


# How many are not in both phases?
# 46,619 are not in both phase

mod_data[["ee"]] %>%
  group_by(Arb_PersonId) %>%
  summarise(n = n_distinct(Intervention)) %>%
  filter(n <2) %>%
  nrow()






# 5. Unique number of enrolled patients by cohort & intervention ---------------
# In pp_data Cohort is time invariant. This capturing the number of patient at
# the control index visit will be used.
pt_flow <- 
  bind_rows(pt_flow,
            (c("Cohort1", "Cohort2", "Cohort3") %>%
            purrr::map_df(
              ~pp_data %>%
                filter(IndexVisit ==1, Cohort == .x, Intervention == "Control") %>%
                distinct(Arb_PersonId) %>%
                summarise(value = n()) %>%
                mutate(type = str_c("Unique patients ", .x)) %>%
                select(type, everything()))
            )
            )



# Of patients eligible 359998 - 7578 enrolled per protocol, equals 352,420 
# patients that are excluded.
# How many of the 352,420 were excluded because they didn't have a visit for weight
# How many did not have 2 or more visits (making of mod data)
# How many removed for censored

# How many were eligible but not enrolled ? 
# 333,931
  bind_rows(pt_flow,
            (visits_post_id %>%
               distinct(Arb_PersonId) %>%
               summarise(value = n()) %>%
               mutate(type = "Unique eligible not enrolled patients") %>%
               select(type, everything()))
            )
  
# How many did not have 2 or more visits in each phase
# visits %>%
#   filter(E)


# Output table to .csv file ----------------------------------------------------
write_csv(pt_flow, here("tables", str_c("pp_pt_flow_", date_max, "_", Sys.Date(), ".csv")))




  