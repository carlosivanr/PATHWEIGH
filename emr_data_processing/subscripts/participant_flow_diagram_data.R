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



# Initialize an empty data frame -----------------------------------------------
pt_flow <- NULL

pt_flow <- data.frame(pt_flow) %>%
  mutate(type = NA,
         value = NA,
         source = NA)%>%
  mutate(type = as.character(type),
         value = as.numeric(value),
         source = as.character(source))

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

# 3. Unique number of eligible patients -------------------------------------------
# Use ee_ene, since these records represent those that met eligibility criteria
# of having age >= 18, BMI >= 25, and in the case of EE had a weight prioritized
# visit
pt_flow <-
  bind_rows(pt_flow,
            (ee_ene %>%
               distinct(Arb_PersonId) %>%
               summarise(value = n()) %>%
               mutate(type = "Unique eligible patients",
                      source = "ee_ene") %>%
               select(type, everything())))



# 4. Unique number of exclusions ---------------------------------------------------------
# The unique number of patients excluded is given by the number of unique patient
# ids in visits that are not in ee_ene.
# Capture the Arb_PersonIds of those that have an eligible visit
eligible_ids <- ee_ene %>%
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


# 5. Specific exclusions based on height, weight, and/or BMI -------------------
# *** To be developed at a later date to prioritize other values
# Some individuals could have been excluded for multiple reasons ie, BMI and/or
# Height

# # Excluded for BMI less than 25 or missing BMI
# pt_flow <-
#   bind_rows(pt_flow,
#             (visits %>%
#   filter(Eligible == 0) %>%
#   filter(!Arb_PersonId %in% eligible_ids) %>%
#   filter(BMI < 25 | is.na(BMI)) %>%
#   distinct(Arb_PersonId) %>%
#   summarise(value = n()) %>%
#   mutate(type = "Excluded for BMI < 25") %>%
#   select(type, everything())))

# Version 2
# For each patient, find the highest Age available,then filter the Arb_PersonIds
# to those who are not eligible, because ee and ene meet age and bmi criteria
# at some point in time. 
# pt_flow <- 
#   bind_rows(pt_flow,
#             (visits %>%
#                select(Arb_PersonId, BMI) %>%
#                group_by(Arb_PersonId) %>%
#                arrange(desc(BMI)) %>%
#                slice_head() %>%
#                ungroup() %>%
#                filter(BMI < 25, 
#                       !Arb_PersonId %in% eligible_ids) %>%
#                distinct(Arb_PersonId) %>%
#                summarise(value = n()) %>%
#                mutate(type = "Excluded for BMI < 25",
#                       source = "visits") %>%
#                select(type, everything())))

# Excluded for Age less than 18 or age is missing
# pt_flow <- 
#   bind_rows(pt_flow,
#             (visits %>%
#   filter(Eligible == 0) %>%
#   filter(!Arb_PersonId %in% eligible_ids) %>%
#   filter(Age < 18 | is.na(Age)) %>%
#   distinct(Arb_PersonId) %>%
#   summarise(value = n()) %>%
#   mutate(type = "Excluded for Age < 18") %>%
#   select(type, everything())))

# Version 2
# For each patient, find the highest Age available,then filter the Arb_PersonIds
# to those who are not eligible, because ee and ene meet age and bmi criteria
# at some point in time. 
# pt_flow <- 
#   bind_rows(pt_flow,
#             (visits %>%
#                select(Arb_PersonId, Age) %>%
#                group_by(Arb_PersonId) %>%
#                arrange(desc(Age)) %>%
#                slice_head() %>%
#                ungroup() %>%
#                filter(Age < 18, 
#                       !Arb_PersonId %in% eligible_ids) %>%
#                distinct(Arb_PersonId) %>%
#                summarise(value = n()) %>%
#                mutate(type = "Excluded for Age < 18",
#                       source = "visits") %>%
#                select(type, everything())))


# Both BMI and Age
# pt_flow <- 
#   bind_rows(pt_flow,
#             (visits %>%
#   filter(Eligible == 0) %>%
#   filter(!Arb_PersonId %in% eligible_ids) %>%
#   filter(Age < 18 | is.na(Age),
#          BMI < 25 | is.na(BMI)) %>%
#   distinct(Arb_PersonId) %>%
#   summarise(value = n()) %>%
#   mutate(type = "Excluded for Age and BMI") %>%
#   select(type, everything())))

# Excluded for Height; Height <54in or > 90in
# pt_flow <- 
#   bind_rows(pt_flow,
#             (visits %>%
#   filter(!Arb_PersonId %in% eligible_ids) %>%
#   filter(Height < 54 | Height > 90) %>% 
#   distinct(Arb_PersonId) %>% 
#   summarise(value = n()) %>%
#   mutate(type = "Excluded for out of bound height") %>%
#   select(type, everything())))



# Excluded for Weight; Weight <80lbs or >600lbs
# Weight variable is in ounces, use Weight_lbs
# *** This does not currently work bc out of bound heights/weights were set to
# NA - Could try setting to -999
# pt_flow <- 
#   bind_rows(pt_flow,
#             (visits %>%
#   filter(!Arb_PersonId %in% eligible_ids) %>%
#   filter(Weight_lbs < 80 | Weight_lbs > 600) %>% 
#   distinct(Arb_PersonId) %>% 
#   summarise(value = n()) %>%
#   mutate(type = "Excluded for out of bound weight") %>%
#   select(type, everything())))

# 6. Unique enrolled patients --------------------------------------------------
# *** first value was 75,952 just after setting index visits
# but then later was 75,728
# why did the number go down?
pt_flow <- 
  bind_rows(pt_flow,                  
            (visits %>%
              filter(Enrolled == 1) %>%
              distinct(Arb_PersonId) %>%
              summarise(value = n()) %>%
              mutate(type = "Unique patients enrolled",
                     source = "visits") %>%
              select(type, everything()))
             )


visits_post_id %>%
  distinct(Arb_PersonId) %>%
  nrow()

# Capture the Arb_PersonIds of those that have an eligible visit
enrolled_ids <- visits %>%
  filter(Enrolled == 1) %>%
  distinct(Arb_PersonId) %>%
  pull(Arb_PersonId)


# 7. Unique patients that are eligible but not enrolled ---------------------------
# Use the visits data frame
pt_flow <-
  bind_rows(pt_flow,
            (visits %>%
               filter(Enrolled == 0,
                      Eligible == 1) %>%
               filter(!Arb_PersonId %in% enrolled_ids) %>%
               distinct(Arb_PersonId) %>%
               summarise(value = n()) %>%
               mutate(type = "Unique eligible patients excluded for lack of weight-related care, age, BMI, or out of bound height or weight.",
                      source = "visits") %>%
               select(type, everything()))
  )


# 8. Eligible and enrolled in control ---------------------------------------------
pt_flow <- 
  bind_rows(pt_flow,
            (visits_post_id %>% 
              filter(Intervention.factor == "Control") %>%
              distinct(Arb_PersonId) %>%
              summarise(value = n()) %>%
              mutate(type = "Unique enrolled patients in control",
                     source = "visits_post_id") %>%
              select(type, everything()))
  )

# (visits_post_id %>% 
#     filter(Intervention == 0) %>%
#     distinct(Arb_PersonId) %>%
#     summarise(value = n()) %>%
#     mutate(type = "Unique enrolled patients in intervention") %>%
#     select(type, everything()))

# 9. Eligible and enrolled in intervention ----------------------------------------
pt_flow <- 
  bind_rows(pt_flow,
            (visits_post_id %>% 
               filter(Intervention.factor == "Intervention") %>%
               distinct(Arb_PersonId) %>%
               summarise(value = n()) %>%
               mutate(type = "Unique enrolled patients in intervention",
                      source = "visits_post_id") %>%
               select(type, everything()))
  )


# (visits_post_id %>% 
#     filter(Intervention == 1) %>%
#     distinct(Arb_PersonId) %>%
#     summarise(value = n()) %>%
#     mutate(type = "Unique enrolled patients in intervention") %>%
#     select(type, everything()))

# 10. Patients with 2 or more visits in each phase ---------------------------------
# Update mod_data script to include Cohort variable

# Unique total patients control with 2 or more visits
pt_flow <- 
  bind_rows(pt_flow,
            (mod_data[["ee"]] %>%
              filter(Intervention == "Control") %>%
              distinct(Arb_PersonId) %>%
              summarise(value = n()) %>%
              mutate(type = str_c("Patients w 2+ visits in control mod_data"),
                     source = "mod_data") %>%
              select(type, everything()))
  )


# Unique total patients intervention with 2 or more visits
pt_flow <- 
  bind_rows(pt_flow,
            (mod_data[["ee"]] %>%
              filter(Intervention == "Intervention") %>%
              distinct(Arb_PersonId) %>%
              summarise(value = n()) %>%
              mutate(type = str_c("Patients w 2+ visits in intervention mod_data"),
                     source = "mod_data") %>%
              select(type, everything()))
  )


# Number of patients in control phase in visits_post_id minus number of patients
# in control in mod_data
pt_flow <- bind_rows(pt_flow,
          (pt_flow %>%
  filter(type %in% c("Unique enrolled patients in control",
                     "Patients w 2+ visits in control mod_data")) %>%
  summarise(value = diff(value) * -1) %>%
  mutate(type = "Excluded for 2+ visits control",
         source = "pt_flow") %>%
    select(type, everything())),
  (pt_flow %>%
    filter(type %in% c("Unique enrolled patients in intervention",
                       "Patients w 2+ visits in intervention mod_data")) %>%
    summarise(value = diff(value) * -1) %>%
    mutate(type = "Excluded for 2+ visits intervention",
           source = "pt_flow") %>%
      select(type, everything()))
) 

# Total number of patients excluded for 2+ visits in both phases
pt_flow <- bind_rows(pt_flow,
(pt_flow %>%
  filter(type %in% c("Excluded for 2+ visits control","Excluded for 2+ visits intervention")) %>%
  summarise(value = sum(value))%>%
  mutate(type = "Exclude for 2+ visits both phases",
         source = "pt_flow") %>%
  select(type, everything())))
  
# The number of patients that were in eligible and enrolled, and had "Unknown" 
# for Sex, and were not in the ee modeling data frame. These represent the 
# number of patients that were excluded from the modeling data set for having
# an unknown sex. Uses ee_ene data frame because mod_data is created from ee_ene

ee_ene %>%
  filter(Enrolled == 1, 
         Sex == "Unknown",
         !Arb_PersonId %in% mod_data[["ee"]] ) %>%
  distinct(Arb_PersonId) %>%
  nrow()

# Only one patient met criteria for Enrollment and had Sex Unknown
unknown_sex_ids <-  ee_ene %>%
  filter(Enrolled == 1,
         Sex == "Unknown") %>%
  distinct(Arb_PersonId) %>%
  pull(Arb_PersonId)

# The patient with unknown sex only had 1 visit in the intervention phase so
# they wouldn't have met criteria for mod_data from 20240326 data delivery.
ee_ene %>%
  filter(Arb_PersonId %in% unknown_sex_ids) %>%
  select(Arb_PersonId, Arb_EncounterId, Intervention.factor)

# Some patients had censored visits, but were able to have enough data to meet
# the modeling data criteria of two or more visits in a single phase even after
# filtering those censored visits out

# The number of unique patients that were enrolled, but had at least one 
# censored visit
# 224 patients had at least one censored visit in 20240326
censored_ids_ee <- ee_ene %>% 
  filter(Enrolled == 1, Censored == 1) %>% 
  distinct(Arb_PersonId) %>% 
  pull(Arb_PersonId)


# How many patients with at least one censored visit have visits across both phases?
# All 224 patients with at least one censored visit and have visits across both
# phases
ee_ene %>%
  filter(Arb_PersonId %in% censored_ids_ee) %>%
  group_by(Arb_PersonId) %>%
  summarise(n = n_distinct(Intervention.factor)) %>%
  pull(n) %>%
  table()
  
# 151 of the 224 patients end up in mod_data, which means that 73 get excluded  
mod_data[["ee"]] %>%
  filter(Arb_PersonId %in% censored_ids_ee) %>%
  distinct(Arb_PersonId) %>%
  nrow()
  
# Capture the ids of the 73 Ids excluded from mod_data because of censored visits
excl_censored_ids <-
  ee_ene %>%
  filter(Arb_PersonId %in% censored_ids_ee) %>%
  filter(!Arb_PersonId %in% mod_data[["ee"]]$Arb_PersonId) %>%
  distinct(Arb_PersonId) %>%
  pull(Arb_PersonId)
  
# The number of visits per phase for each patient that got excluded from mod_data
# for having at least one censored visit.
# left_join(
# (ee_ene %>%
#   filter(Arb_PersonId %in% excl_censored_ids) %>%
#   group_by(Arb_PersonId, Intervention.factor) %>%
#   count() %>%
#   ungroup()),
# 
# (ee_ene %>%
#   filter(Arb_PersonId %in% excl_censored_ids) %>%
#   filter(Censored == 0) %>%
#   group_by(Arb_PersonId, Intervention.factor) %>%
#   count() %>%
#    ungroup()),
# by = c("Arb_PersonId", "Intervention.factor")) %>%
#   mutate(n_visits_before = n.x,
#          n_visits_after = n.y) %>%
#   mutate(n_visits_after = ifelse(is.na(n_visits_after), 0, n_visits_after)) %>%
#   filter(Arb_PersonId == 5891823092) %>%
#   mutate(excluded = ifelse(n_visits_after < 2, 1, 0)) %>%
#   select(Intervention.factor, excluded) %>%
#   tbl_summary( by = Intervention.factor)

left_join(
  (ee_ene %>%
     filter(Arb_PersonId %in% excl_censored_ids) %>%
     group_by(Arb_PersonId, Intervention.factor) %>%
     count() %>%
     ungroup()),
  
  (ee_ene %>%
     filter(Arb_PersonId %in% excl_censored_ids) %>%
     filter(Censored == 0) %>%
     group_by(Arb_PersonId, Intervention.factor) %>%
     count() %>%
     ungroup()),
  by = c("Arb_PersonId", "Intervention.factor")) %>%
  mutate(n_visits_before = n.x,
         n_visits_after = n.y) %>%
  mutate(n_visits_after = ifelse(is.na(n_visits_after), 0, n_visits_after)) %>%
  mutate(excluded = ifelse(n_visits_after < 2, 1, 0)) %>%
  group_by(Arb_PersonId) %>%
  summarise(n_phases = sum(excluded)) %>%
  ungroup() %>%
  filter(n_phases == 0) 
  # pull(n_phases) %>% # This line caused an error bc of a missing pipe operator %>% in the line above
  # table()

# The number of patients that had at least one censored visit, but met the 
# remaining criteria for mod_data (i.e. 2 or more visits in a single phase)
# 151 patients in mod data had censored visits. The censored visits were not 
# used in the creation of mod_data[["ee"]], but their non-censored visits were
# used.
# pt_flow <- 
#   bind_rows(pt_flow,
#             (mod_data[["ee"]] %>%
#               filter(Arb_PersonId %in% censored_ids_ee_ene) %>%
#               distinct(Arb_PersonId) %>%
#               summarise(value = n()) %>%
#               mutate(type = "Excluded for censored visits") %>%
#               select(type, everything()) %>%
#               mutate(value = length(censored_ids_ee_ene) - value)))

# The number of enrolled patients with at least one censored visits in the 
# control phase of the full ee_ene dataset that were not included in the 
# mod_data. 
# 135
ee_ene %>% 
  filter(Enrolled == 1, Censored == 1, Intervention.factor == "Control") %>% 
  distinct(Arb_PersonId) %>% 
  filter(!Arb_PersonId %in% 
           (mod_data[["ee"]] %>% 
              filter(Intervention == "Control") %>%
              pull(Arb_PersonId))
  ) %>%
  nrow()

# The number of enrolled patients with at least one censored visits in the 
# intervention phase of the full ee_ene dataset that were not included in the 
# mod_data.
# 108
ee_ene %>% 
  filter(Enrolled == 1, Censored == 1, Intervention.factor == "Intervention") %>% 
  distinct(Arb_PersonId) %>% 
  filter(!Arb_PersonId %in% 
           (mod_data[["ee"]] %>% 
              filter(Intervention == "Intervention") %>%
              pull(Arb_PersonId))
  ) %>%
  nrow()


# The number of enrolled patients with at least one censored visits in either
# phase of the full ee_ene dataset that were not included in the mod_data.
# 73
ee_ene %>% 
  filter(Enrolled == 1, Censored == 1) %>% 
  distinct(Arb_PersonId) %>% 
  filter(!Arb_PersonId %in% 
           (mod_data[["ee"]] %>%
              pull(Arb_PersonId))
  ) %>%
  nrow()


# 11. Stepped wedge table values -----------------------------------------------
# For each cohort
cohorts <- c("Cohort1", "Cohort2", "Cohort3")

# For loop through all cohorts
for (i in cohorts){
  # print(i)
  
  # Develop a function to get
  # 1. control phase ee for cohort
  # 2. intervention phase ee for cohort
  # 3. The number of patients enrolled in control that continued to intervention
  # 4. The number of patients enrolled in intervention only
  # 5. The number of patients that enrolled in control, but did not continue to
  #     intervention
  
  # 1. Unique patients in control phase of cohorts[i]
  n_con <- 
    mod_data[["ee"]] %>%
    filter(Cohort == i, Intervention == "Control") %>%
    distinct(Arb_PersonId) %>%
    summarise(value = n()) %>%
    mutate(type = str_c("Patients in control ", i),
           source = "mod_data") %>%
    select(type, everything())
  
  # 2. Unique patients in control phase of cohorts[i]
  n_int <- 
    mod_data[["ee"]] %>%
    filter(Cohort == i, Intervention == "Intervention") %>%
    distinct(Arb_PersonId) %>%
    summarise(value = n()) %>%
    mutate(type = str_c("Patients in intervention ", i),
           source = "mod_data") %>%
    select(type, everything())
  
  # 3. The number of patients enrolled in control that did not continue to intervention
  # Arb_PersonIds in control not in intervention
  ids_in_con <- 
    mod_data[["ee"]] %>%
    filter(Cohort == i, Intervention == "Control") %>%
    distinct(Arb_PersonId) %>%
    pull(Arb_PersonId)
  
  ids_in_int <- 
    mod_data[["ee"]] %>%
    filter(Cohort == i, Intervention == "Intervention") %>%
    distinct(Arb_PersonId) %>%
    pull(Arb_PersonId)
  
  # length(!ids_in_con %in% ids_in_int)
  
  # 4. Patients in control only
  n_con_notin_int <- 
    mod_data[["ee"]] %>%
    filter(Cohort == i, Intervention == "Control") %>%
    distinct(Arb_PersonId) %>%
    filter(!ids_in_con %in% ids_in_int) %>%
    summarise(value = n()) %>%
    mutate(type = str_c("Patients in control only ", i),
           source = "mod_data") %>%
    select(type, everything())
  
  # 5. Patients in both control and intervention
  n_con_in_int <- 
    mod_data[["ee"]] %>%
    filter(Cohort == i, Intervention == "Control") %>%
    distinct(Arb_PersonId) %>%
    filter(ids_in_con %in% ids_in_int) %>%
    summarise(value = n()) %>%
    mutate(type = str_c("Patients in control and intervention ", i),
           source = "mod_data") %>%
    select(type, everything())
  
  # 6. Patients in intervention only
  n_int_notin_con <- 
    mod_data[["ee"]] %>%
    filter(Cohort == i, Intervention == "Intervention") %>%
    distinct(Arb_PersonId) %>%
    filter(!ids_in_int %in% ids_in_con) %>%
    summarise(value = n()) %>%
    mutate(type = str_c("Patients in intervention only ", i),
           source = "mod_data") %>%
    select(type, everything())
  
  
  pt_flow <-   
  bind_rows(pt_flow,
    n_con,
    n_int,
    n_con_notin_int,
    n_con_in_int,
    n_int_notin_con
  )

  # Unique total patients in cohort[i]
  # mod_data[["ee"]] %>%
  #   filter(Cohort == i) %>%
  #   distinct(Arb_PersonId) %>%
  #   summarise(value = n()) %>%
  #   mutate(type = str_c("Patients in ", i)) %>%
  #   select(type, everything())
}

# Output table to .csv file ----------------------------------------------------
write_csv(pt_flow, here("tables", str_c("pt_flow_", date_max, "_", Sys.Date(), ".csv")))




  