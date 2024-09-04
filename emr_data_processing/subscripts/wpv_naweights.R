# WPV NA Weights %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Sets any WPV variables to 0 if weight is NA, because one of the criteria for
# a WPV is to have a recorded weight.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wpv_naweights <- function(temp) {
  # Set any WPV variable to 0 if there is no weight recorded
  temp %<>%
    mutate(across(starts_with("WPV"), ~ ifelse(is.na(Weight_kgs), 0, .x)))

  return(temp)
}
