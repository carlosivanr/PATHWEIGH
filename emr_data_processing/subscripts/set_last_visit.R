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
set_last_visit <- function(temp) {
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

  # Set the LastVisit to the last visit with weight
  # Capture labs procedures & comorbidities on the last visit with a weight
  # value
  temp %<>%
    mutate(LastVisit = LastVisit_Weight)

  # No visits with a missing weight are flagged as a LastVisit ----
  if ((temp %>%
         filter(LastVisit_Weight == 1, is.na(Weight_kgs)) %>%
         nrow()) != 0) {
    stop("Last visits with NA weights detected. Revise code.")
  }

  return(temp)
}