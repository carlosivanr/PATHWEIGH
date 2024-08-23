# WPV NA Weights %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Sets any WPV variables to 0 if weight is NA, because one of the criteria for
# a WPV is to have a recorded weight.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wpv_naweights <- function(temp){
  # Print the number of WPVs including those without captured weight
  # n.b. must be deployed before changing WPV variables with NA in Weight
  # print(
  #   str_c("Number of Encounters by count of WPVs including those without a recorded weight: ",
  #         temp %>% filter(if_any(starts_with("WPV_"), ~ .x == 1)) %>% nrow())) 
  
  # Set any WPV variable to 0 if there is no weight recorded
  temp %<>%
    mutate(across(starts_with("WPV"), ~ ifelse(is.na(Weight_kgs), 0, .x)))
  
  # Print the number of WPVs including only those with weight
  # print(
  #   str_c("Number of Encounters by count of WPVs including only those with a recorded weight: ",
  #         temp %>% filter(if_any(starts_with("WPV_"), ~ .x == 1)) %>% nrow()))
  
  return(temp)
}
