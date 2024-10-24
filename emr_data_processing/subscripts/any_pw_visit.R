# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# 03/07/2024

# Set Any_PW_Visit
# Any_PW_Visit is a binary indicator of whether or not the patient had a prior
# PW_Visit. A PW_Visit is defined as an encounter in which the PW Tools were
# utilized.

# 3 Types of PW Visits
# 1. PW Visit Type
# 2. PW Smart Set
# 3. PW Weight Management Questionnaire (WMQ)

# PW_Visit is created by assigning a 1 to any visit that has any of evidence of
# the pathweigh tools.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_any_pw_visit <- function(data) {

  # Set PW_Visit as binary indicator of whether or not the visit was associated
  # with one of the 3 types of pathweigh tools.
  # WPV_PW_flow and WPV_WMQ are the same thing, except that one is discovered
  # through a template id the refers to the entire questionnaire and another
  # through an epic id that refers to a specific question from a questionnaire
  data %<>%
    mutate(PW_Visit = if_else(WPV_PW_flow == 1 | WPV_WMQ == 1 | WPV_IP == 1 | WPV_TH == 1 | WPV_smart == 1, 1, 0)) # nolint: line_length_linter.

  return(data)
}