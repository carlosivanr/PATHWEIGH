# Set Last Visits %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. UC Anschutz Medical Campus, Dept. of Family Medicine

# Description:
# Sets the last visit for each patient in each phase. This script sets two
# variables one for the last visit in each phase with a weight and another
# for the last visit with out weight

# - LastVisit_Weight
# - LastVisit

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Mark the last visit in a phase -----------------------------------------------
# Each last visit must have a weight value to be eligible for a last visit
set_last_visit <- function(temp){
  tic()
  # Set the last visit with weight ----
  # Get the encounter id of the last visit for each patient in each phase
  enc_ids <- 
    temp %>%
    filter(!is.na(Weight_kgs)) %>%
    group_by(Arb_PersonId, Intervention) %>%
    arrange(EncounterDate) %>%
    slice_tail() %>%
    pull(Arb_EncounterId)
  
  # Set a binary indicator of the last visit for each patient in each phase
  temp %<>%
    mutate(LastVisit_Weight = ifelse((Arb_EncounterId %in% enc_ids), 1, 0))
  
  # Set the last visit regardless of whether or not weight is captured ----
  # Get the encounter id of the last visit for each patient in each phase
  enc_ids <- 
    temp %>%
    group_by(Arb_PersonId, Intervention) %>%
    arrange(EncounterDate) %>%
    slice_tail() %>%
    pull(Arb_EncounterId)
  
  # Set a binary indicator of the last visit for each patient in each phase
  temp %<>%
    mutate(LastVisit = ifelse((Arb_EncounterId %in% enc_ids), 1, 0))
  

  # No visits with a missing weight are flagged as a LastVisit ----
  if ((temp %>% filter(LastVisit_Weight == 1, is.na(Weight_kgs)) %>% nrow()) != 0){
             stop("Last visits with NA weights detected. Revise code.")
  }
  
  toc()
  return(temp)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # How many folks have an index date and a last visit date that are within 30 days?
# # Need to exclude those that have only one index date, since the last will fall
# # within 30 days.
# # filter out data where index date == last visit
# # then we need to filter out index date and last visit
# # check to make sure we only have 2 visits per patient
# 
# # if 2 visits per patient, then select person id, encounter day, index date, 
# # index visit, and last visit
# # create a column of dates called Last_visit date with if else, should create
# # a column with NAs in rows that are index date
# 
# # Use the fill in function to fill in last visit into the rows with index date
# # mutate LastVisit_date minus IndexDate To give the number of days between
# # index and last visit.
# # then filter for those less than 30 days.
# 
# # Visits we don't want investigate as falling within 30 days of index since
# # they are the same
# # 17,087
# enc_ids_to_exclude <- 
#   visits_post_id %>%
#   filter(IndexVisit == 1,
#          LastVisit == 1) %>%
#   pull(Arb_EncounterId)
# 
# # Check the number of visits per patient is not more than 2 in each phase of
# # the intervention
# visits_post_id %>%
#   filter(!Arb_EncounterId %in% enc_ids_to_exclude) %>%
#   filter(IndexVisit == 1, LastVisit == 1) %>%
#   group_by(Arb_PersonId, Intervention) %>%
#   count() %>%
#   filter(n > 2)
# 
# # Create the Number of days the last visit took place after the index date
# # Then capture the encounter ids of the last visit that is less than or equal to
# # 30 days from the index
# 3,191 encounters that occur within 30 days of the index visit for 2023-03-22
# enc_id_lt_30d_ind <-
#   visits_post_id %>%
#   filter(!Arb_EncounterId %in% enc_ids_to_exclude) %>%
#   filter(IndexVisit == 1 | LastVisit == 1) %>%
#   group_by(Arb_PersonId, Intervention) %>%
#   mutate(LastVisitDate = if_else(LastVisit ==1, EncounterDate, NA)) %>%
#   fill(LastVisitDate) %>%
#   arrange(Arb_PersonId, EncounterDate, Intervention) %>%
#   ungroup() %>%
#   select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexDate, LastVisitDate, IndexVisit, LastVisit, Intervention) %>%
#   mutate(N_days_last_visit = LastVisitDate - IndexDate) %>%
#   filter(N_days_last_visit <= 30,
#          LastVisit == 1) %>%
#   pull(Arb_EncounterId)
# # 
# # 
# # visits_post_id %<>%
# #   mutate(lv_lt_30d_ind = ifelse(Arb_EncounterId %in% enc_ids_lt_30_ind, 1, 0))
# # 
# # # Then use the lv_lt_30d_ind to filter in the labs_procedures medications etc.
# 
# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # How many intervention index dates are within 30 days of the control index
# enc_ids_to_include <- 
#   visits_post_id %>%
#   filter(Censored == 0,
#          IndexVisit == 1) %>%
#   group_by(Arb_PersonId) %>%
#   count() %>%
#   filter(n>1) %>%
#   pull(Arb_PersonId)
# 
# #  The number of patient that have an intervention index date 30 days from the
# # control index date is 32. These folks will have overlapping labs procedures
# # and medications for 2023-03-22
# enc_ids_to_investigate <- 
#   visits_post_id %>%
#   filter(Censored == 0,
#          IndexVisit == 1,
#          Arb_PersonId %in% enc_ids_to_include) %>%
#   select(Arb_PersonId, IndexDate, Intervention.factor) %>%
#   #select(Arb_PersonId, Arb_EncounterId, EncounterDate, IndexVisit, IndexDate, Intervention.factor) %>%
#   arrange(Arb_PersonId) %>%
#   pivot_wider(names_from = Intervention.factor, values_from = IndexDate) %>%
#   mutate(N_days_ind_to_ind = Intervention - Control) %>%
#   filter(N_days_ind_to_ind <= 30) %>%
#   pull(Arb_PersonId)
# 
# 
# test <- visits_post_id %>%
#   filter(Censored == 0,
#          IndexVisit == 1,
#          Arb_PersonId %in% enc_ids_to_investigate)
