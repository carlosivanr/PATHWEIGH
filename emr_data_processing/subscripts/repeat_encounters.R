# Repeat Encounters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The encounters table includes duplicated encounter Ids. This function was 
# written to determine which duplicate encounter to keep and which one to 
# discard.

# Description:
# This function counts the number of NAs in each row. Then it groups the rows
# by Arb_EncounterId, arranges by descending eligibility, NA count, then slices
# the first row to keep

# There are discrepancies in smoking status (keep the more extreme of 
# Smoking_Status. Many rows will have the same EncounterId but will differ
# only in the Smoking_Status.

# One problem here is that some patients have multiple visits on the same day
# and are at different locations, see Arb_PersonId == 4597227314 for an example.
# join2 %>% filter(Arb_PersonId == 4597227314)

# This section has been modified to remove duplicate encounterIds rather than 
# visits occurring in the same day, such as labs. The original version sorted 
# all rows by personId, encounter date, the number of NAs, and smoking status, 
# and then removed any remaining rows matching for the person ID and encounter 
# date. However, since we don't know for sure which visit types are being 
# eliminated the WPV could inadvertently be excluded. This new version only 
# eliminates the repeat encounter ids, so that the rest of the script can 
# determine which encounter is the WPV if any. 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
repeat_encounters <- function(temp) {
  # Display the total number of encounters
  # print(paste0("Number of Encounters including duplicates: ", nrow(temp)))

  # Display the number of duplicate encounters by counting encounter Ids
  # print(paste0("Number of Duplicate Encounters by EncounterId: ",
  #        nrow(temp) - n_distinct(temp$Arb_EncounterId)))
  
  # Create a column that counts the NAs per row to be used in ranking duplicate
  # encounters
  temp %<>%
    mutate(NA_Count = rowSums(is.na(.)))
  
  # Find the duplicated encounter ids
  # Option 2
  # dup_enc_ids <- temp[duplicated(temp$Arb_EncounterId)]$Arb_EncounterId
  
  dup_enc_ids <- 
    temp %>% 
    select(Arb_EncounterId) %>% 
    group_by(Arb_EncounterId) %>% 
    count() %>%
    ungroup() %>%
    filter(n > 1) %>%
    pull(Arb_EncounterId)
  
  # create a separate data frame for the data from duplicated encounter ids

  duplicates <- temp %>%
    filter(Arb_EncounterId %in% dup_enc_ids)
  
  # remove the duplicated encounter ids from the main data frame
  temp %<>% filter(!Arb_EncounterId %in% dup_enc_ids)
  
  
  # Process the duplicates
  duplicates %<>%
    group_by(Arb_PersonId, Arb_EncounterId) %>%
    arrange(desc(IndexVisitEligible), NA_Count, desc(Smoking_Status)) %>%
    slice_head() %>%
    ungroup()
  
  # Merge the data frames back together
  temp <- bind_rows(temp, duplicates)
  
  # The number of unique encounters
  # print(paste0("Number of Encounters without duplicates: ",
  #        n_distinct(temp$Arb_EncounterId)))
  
  # Remove the NA_Count column
  temp %<>%
    select(-NA_Count)
  

  return(temp)
}
