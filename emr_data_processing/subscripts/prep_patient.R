# Prepare Patient Table %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 1. Sex - set to NA if X
# 2. Set Race to the values used in the 05/03/2022 delivery
# 3. Ethnicity - set to NA if Unknown or *Unspecified
# 4. Create Race_Ethnicity
#   a. set Hispanic to Hispanic or Latino
#   b. set NA to "Unknown"
#   c. set Non-Hispanic White to White or Caucasian
#   d. collapse other_race to other
# 5. Clean up asterisks
# 6. Print warnings if the number of race/ethnic categories are != 9

# Dependencies:
# clean_asterisks.R function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prep_patient <- function(temp){
  
  # Check for duplicated patient ids -------------------------------------------
  if (n_distinct(patient$Arb_PersonId) != dim(patient)[1]){
    stop("Duplicated patient ids detected, review data")
  }
  
  # Load clean_asterisks function ----------------------------------------------
  # This function cleans values with asterisks and is used to clean the 
  # encounter and patient tables.
  # source(str_c(emr_dir, "functions/", "clean_asterisks.R"))
  

  # Cull columns ---------------------------------------------------------------
  temp <-
    temp %>%
    select(-DeathDate_EPIC, -InjuryDescription, -OtherSignificantCondition, -CauseOfDeath)

  # Clean up asterisks ---------------------------------------------------------
  # temp <- clean_asterisks(temp)
  
  # Sex ------------------------------------------------------------------------
  temp %<>% 
    mutate(Sex = na_if(Sex, "X"),
           Sex = ifelse(is.na(Sex), "Unknown", Sex))
  
  # Check if sex can be carried forward from a previous or prior encounter -----
  # carry sex forward or backward?
  carry_sex <- temp %>%
    group_by(Arb_PersonId) %>%
    summarise(n = n_distinct(Sex)) %>%
    filter(n > 1) %>%
    nrow()
  
  # If there are multiple values for the same patient, then use fill() function
  # make the same values consistent across all visits.
  if (carry_sex > 0){
    temp %<>%
    group_by(Arb_PersonId) %>%
    fill(Sex, .direction = "updown") %>%
    ungroup()
  }
  
  # For data delivery 20230322 and beyond
  # Ethnicity ------------------------------------------------------------------
  temp %<>%
    mutate(Ethnicity = ifelse((grepl("Unknown|Unable", Ethnicity)), NA, Ethnicity))
  
  # Set Race/Ethnicity Column -----------------------------------------------
  other_race <- c("American Indian or Alaska Native", "Multiple Race",
                  "Native Hawaiian and Other Pacific Islander", "Native Hawaiian",
                  "Other Pacific Islander", "Guamanian or Chamorro", "Samoan")
  
  temp %<>% 
    mutate(
      Race_Ethnicity = ifelse(Ethnicity == "Hispanic, Latino/a, or Spanish Origin", "Hispanic or Latino", Race),
      Race_Ethnicity = ifelse(is.na(Race_Ethnicity), "Unknown", Race_Ethnicity),
      Race_Ethnicity = as_factor(Race_Ethnicity),
      Race_Ethnicity = fct_collapse(Race_Ethnicity,
                                    `Non-Hispanic White` = "White or Caucasian",
                                    "Other" = other_race,
                                    "Unknown" = "",
                                    "Unknown" = "Patient Declined"))

  
  # Check that there are 6 final available values for Race_Ethnicity. If not,
  # then throw an error
  if (length(names(table(temp$Race_Ethnicity))) !=6){
    stop("The number of Race_Ethnicity values is not expected. Review code")
  }
  
  # Set the levels of Race_Ethnicity
  temp %<>%
  mutate(Race_Ethnicity = factor(Race_Ethnicity, 
                                  levels = c("Non-Hispanic White", 
                                            "Hispanic or Latino", 
                                            "Black or African American", 
                                            "Asian",
                                            "Other",
                                            "Unknown")))
    
  


  
  # Print a check to output ----------------------------------------------------
  if (length(unique(temp$Race_Ethnicity)) != 6){
    warning("The number of pre-defined race and ethnic categories is not correct. Consider reviewing code.")
  }
  
  # Clean up variables from workspace ------------------------------------------
  rm(other_race)

  return(temp)
}
