# Define WPVs by Visit Type %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# WPVs can be defined using Visit Type IDs, where in-person and virtual 
# respectively
# 1. VisitTypeID = 413519 
# 2. VisitTypeID = 415110 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create in person or telehealth (virtual) indicator variables
wpv_vtype <- function(temp){
  temp %<>%
    mutate(WPV_IP = ifelse(Intervention == 1 & 
                             VisitTypeId == 413519, 1, 0),
           WPV_TH = ifelse(Intervention == 1 &
                             VisitTypeId == 415110, 1, 0))
  
  # print(paste0("Number of Encounters using In-Person Visit Type: ",
  #        length(which(temp$WPV_IP==1))))
  
  # print(paste0("Number of Encounters using Virtual Visit Type: ",
  #        length(which(temp$WPV_TH==1))))
  
  # Remove the visit type ID column since it will no longer be needed
  temp %<>% select(-VisitTypeId)
  
  return(temp)
}