# Define WPVS by use of Smart Set %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The smart table in the COMPASS data delivery contains the patient and visit
# IDs where the smart set was used. WPV_smart is defined as 1 if the encounter
# ID of the patient in the input data frame is found in the smart table.


# SMARTTOOL Logger captures the following Ids for SMARTTEXT
# Those with SMARTTEXT_ID = 2109052067 are AMB WPV NOTE
# All ecnounters are Flag == SMARTEXT
# These are supposed to define the disappearing smart text

# Those with SMARTTEXT_ID = 2107060002 are AMB FOLLOWUP VISIT WEIGHT MANAGEMENT
# All encounters are Flag == SMARTEXT

# Those with SMARTTEXT_ID = 2107060001 are AMB INITIAL VISIT WEIGHT MANAGEMENT
# All encounters are Flag == SMARTTEXT

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wpv_smart <- function(temp) {
  temp %<>%
    mutate(WPV_smart = ifelse(
      Arb_EncounterId %in% smart$Arb_EncounterId & Intervention == 1, 1, 0
    ))

  return(temp)
}
