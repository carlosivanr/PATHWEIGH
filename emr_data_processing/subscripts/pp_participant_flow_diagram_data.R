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
# Loads visits df
load("D:/PATHWEIGH/delivery_20240917/data/processed_all_visits_20240917.RData")
load("D:/PATHWEIGH/delivery_20240917/data/pp_mod_data_20240917.RData")
load("D:/PATHWEIGH/delivery_20240917/data/pp_data_20240917.RData")

# Initialize an empty data frame -----------------------------------------------
pt_flow <- NULL

# Set up a structure in pt_flow to fill in with values
pt_flow <- data.frame(pt_flow) %>%
  mutate(type = NA,
         value = NA)%>%
  mutate(type = as.character(type),
         value = as.numeric(value))


# 1. Unique number of encounters -----------------------------------------------
# Use the visits data frame because it is bounded by the study start date and by
# the latest deliver date
pt_flow <- 
  bind_rows(pt_flow,
            (visits %>% 
               distinct(Arb_EncounterId) %>% 
               summarise(value = n()) %>%
               mutate(type = "Unique encounters") %>%
               select(type, everything())))


# 2. Unique number of patients -------------------------------------------------
# Use the visits data frame
pt_flow <- 
  bind_rows(pt_flow,   
            (visits %>% 
               distinct(Arb_PersonId) %>% 
               summarise(value = n()) %>%
               mutate(type = "Unique patients") %>%
               select(type, everything())))


# 3. Unique number of eligible patients ----------------------------------------
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


# 4. How many excluded for not having a discernible visit for weight (ene) -----
pt_flow <-
  bind_rows(pt_flow,
            (visits %>%
               filter(Eligible == 1,
                      Enrolled == 0) %>%
               distinct(Arb_PersonId) %>%
               summarise(value = n()) %>%
               mutate(type = "Excluded for not having discernible care for weight") %>%
               select(type, everything())))


# 5. How many were excluded for not having 2 visits in both phase --------------
pt_flow <- 
  bind_rows(pt_flow,
            (visits %>%
               filter(Eligible == 1,
                      Enrolled == 1,
                      !Arb_PersonId %in% pp_mod_data$Arb_PersonId) %>%
               distinct(Arb_PersonId) %>%
               summarise(value = n()) %>%
               mutate(type = "Excluded for not having 1 additional weight") %>%
               select(type, everything())))


# 6. Unique number of enrolled patients by cohort & intervention ---------------
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

# Output table to .csv file ----------------------------------------------------
write_csv(pt_flow,
          here(str_c("delivery_", data_delivery_date), 
               "manuscript_tbls_figs", 
               "participant_flow_diagram.csv")
          )
