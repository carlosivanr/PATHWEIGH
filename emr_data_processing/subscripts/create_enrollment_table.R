#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Medical Campus, Dept. of Family Medicine
# Feb 08, 2023
# Create enrollment table

# Dependencies: ----------------------------------------------------------------
# This script functions on the visits_post_id data frame. If using
# the entire visits data frame then enrollment == 1 needs to be used to capture
# the same patients.

# Description: -----------------------------------------------------------------
# 1) This function will generate a gt_summary table in pdf format displaying the
# number of enrolled patients. 


# 2) Additionally, this function will create an NIH style participant level data
# file in .csv format. The values of the output .csv file must be copy-pasted 
# into the file provided by NIH. The NIH-provided file is found in 
# /PATHWEIGH/Quantitative/Projects/working_file/ParticipantLevelData-Template.csv

# As of 02/09/2023, based on:
# https://grants.nih.gov/sites/default/files/Participant-level%20data%20template%20tip%20sheet.pdf
# The participant level data spreadsheet has 4 columns, 
# Race, Ethnicity, Gender, Age, and Age Unit for each patient that is enrolled

# Race values must be: 
# American Indian, Asian, Black, Hawaiian, More than one race, Unknown, or White

# Ethnicity values must be: 
# Not Hispanic or Latino, Hispanic or Latino, or Unknown

# Gender must be: Female, Male, or Unknown

# Set Age Unit to Years unless Age is >=90, in which case set to Ninety Plus

# Set all Age values to "" if Age Unit is Ninety Plus
# Age must be a whole number

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_enrollment_table <- function(data){
  # Capture one record per patient that is enrolled and eligible ---------------
  
  # Censored may be broken, modify censor to convert NAs to 0, then run to see
  # if the values equal the same
  # If not it could be that we need to filter to the EE only too.
  # Option 1: Modify Censored
  data %<>% 
    filter(Eligible == 1, 
           Enrolled == 1, 
           EncounterDate >= IndexDate)
  
  # Set to capture the most recent visit
  data %<>%
    filter(Censored == 0) %>%
    group_by(Arb_PersonId) %>%
    arrange(EncounterDate) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(Arb_PersonId, Race, Ethnicity, Sex, Age) %>%
    rename(Gender = Sex)
  
  # Check the # of rows that result in data after filtering
  # *** Suggest that Censored is broken.
  # data %>%
  #   filter(Censored == 0) %>%
  #   nrow()
  
  # Prepare the race column ----------------------------------------------------
  # Race values for NIH have to be one of the following (02/09/2023)
  race_vals_nih <- c("White",
                     "Black",
                     "Asian", 
                     "American Indian", 
                     "Hawaiian", 
                     "More than one race", 
                     "Unknown")
  
  # Capture the unique values in the Race column of the input data
  race_vals_data <- names(table(data$Race))
  
  # Test if the values in the data match those that are required by NIH?
  if(length(race_vals_data[(!race_vals_data %in% race_vals_nih)]) != 0){
    message("The number of unique Race values in data do not match those required by NIH. Attempting to modify values.")
    
    # These vectors can be appended to incorporate additional values to properly 
    # categorize the EPIC data with the NIH values
    white <- c("White or Caucasian")
    
    black <- c("Black or African American")
    
    asian <- c("Asian") 
    
    american_indian <- c("American Indian or Alaska Native")
    
    hawaiian <- c("Native Hawaiian",
                  "Other Pacific Islander", 
                  "Native Hawaiian and Other Pacific Islander", 
                  "Samoan", 
                  "Guamanian or Chamorro")
    
    gt_1_race <-  c("Multiple Race")
    
    unknown_race <- c("", 
                  "Patient Refused",
                  "Patient Declined",
                  "Other")
    
    # Recode the values in the race column
    data %<>%
      mutate(
        Race = ifelse(Race %in% white, "White", Race),
        Race = ifelse(Race %in% black, "Black", Race),
        Race = ifelse(Race %in% american_indian, "American Indian", Race),
        Race = ifelse(Race %in% hawaiian, "Hawaiian", Race),
        Race = ifelse(Race %in% gt_1_race, "More than one race", Race),
        Race = ifelse(Race %in% unknown_race, "Unknown", Race)
      )
    
    # Check the number of values at this stage of the pipeline
    data %>%
      select(Race) %>%
      tbl_summary()

  
    # Check for any NAs
    if(sum(is.na(data$Race)) != 0){
      message("There are NAs in the Race variable.")
      }
    
    # Update the values of race data 
    race_vals_data <- names(table(data$Race))
    
    # Test if there are any values that have not been categorized
    if(length(race_vals_data[(!race_vals_data %in% race_vals_nih)]) !=0){
      print(race_vals_data[(!race_vals_data %in% race_vals_nih)])
      
      stop("The preceding values have not been correctly set.")
    } else {
      message("Race values set!")
      }
  }
  
  
  # Prepare the ethnicity column -----------------------------------------------
  # Ethnicity values have to be one of the following:
  eth_vals_nih <-   c("Hispanic or Latino", 
                           "Not Hispanic or Latino",
                           "Unknown")
  
  eth_vals_data <- names(table(data$Ethnicity))
  
  # Test if the values in the data match those that are required by NIH?
  if(length(eth_vals_data[(!eth_vals_data %in% eth_vals_nih)]) != 0){
    
    message("The number of unique Ethnicity values in data do not match those required by NIH. Attempting to modify values.")
    
    data %<>%
      mutate(
      Ethnicity = ifelse(Ethnicity %in% "Hispanic, Latino/a, or Spanish Origin", "Hispanic or Latino", Ethnicity),
      Ethnicity = ifelse(Ethnicity %in% "Non-Hispanic, Latino/a, or Spanish Origin", "Not Hispanic or Latino", Ethnicity),
      Ethnicity = ifelse(Ethnicity %in% c("*Unspecified", "Patient Unable to Answer"), "Unknown", Ethnicity),
      Ethnicity = ifelse(is.na(Ethnicity), "Unknown", Ethnicity)
    )
  
    # Check for any NAs
    if(sum(is.na(data$Ethnicity)) != 0){
      message("There are NAs in the Ethnicity variable.")
    }
    
    # Update the values of race data 
    eth_vals_data <- sort(names(table(data$Ethnicity)))
    
    
    # Test if there are any values that have not been categorized
    if(length(eth_vals_data[(!eth_vals_data %in% eth_vals_nih)]) !=0){
      print(eth_vals_data[(!eth_vals_data %in% eth_vals_nih)])
      stop("The preceding values have not been correctly set.")
    } else {
      message("Ethnicity values set!")
      }
  }
  
  
  # Prepare the gender column --------------------------------------------------
  # NIH values have to be one of the following:
  gender_vals_nih <- c("Male", "Female", "Unknown")
  
  gender_vals_data <- names(table(data$Gender))
  
  # Test if the values in the data match those that are required by NIH?
  if(length(gender_vals_data[(!gender_vals_data %in% gender_vals_nih)]) != 0){
    
    message("The number of unique Gender values in data do not match those required by NIH. Attempting to modify values.")
    
    # This is left as a scaffold in case any values do not match in future data 
    # deliveries
    # male <- c({values that correspond to male})
    # female <- c({values that correspond to female}
    # unknown_gend <- c({values that correspond to unkown}))
    # data %<>%
    #   mutate(
    #     Gender = ifelse(Gender %in% male, "Male", Gender),
    #     Gender = ifelse(Gender %in% female, "Female", Gender),
    #     Gender = ifelse(Gender %in% unknown_gend, "Unknown", Gender)
    #   )
    
    # Check for any NAs
    if(sum(is.na(data$Gender)) != 0){
      message("There are NAs in the Gender variable.")
    }
    
    # Update the values of race data 
    gender_vals_data <- sort(names(table(data$Gender)))
    
    # Test if there are any values that have not been categorized
    if(length(gender_vals_data[(!gender_vals_data %in% gender_vals_nih)]) !=0){
      print(gender_vals_data[(!gender_vals_data %in% gender_vals_nih)])
      stop("The preceding values have not been correctly set.")
    } else {
      # Return the gender values back to the environment for 
      message("Gendere values set!")
    }
  }
  
  
  # Prepare the Age Unit column ------------------------------------------------
  # Create a new columns
  data$`Age Unit` <- "Years"
  
  # Modify those that have 90 or above in age
  # Set age unit to "Ninety Plus" for any individual with Age >= 90 and 
  # then set Age to blank, per NIH instructions
  data %<>%
    mutate(`Age Unit` = ifelse(Age >= 90, "Ninety Plus", `Age Unit`),
           Age = ifelse(`Age Unit` == "Ninety Plus", "", Age))
  
  # Check the frequencies of the Age_Unit
  table(data$`Age Unit`)
  
  # Set age to 0 significant digits.
  data$Age <- round(as.numeric(data$Age, 0))
  
  
  # Write participant level data to .csv ---------------------------------------
  write.csv((select(data, -Arb_PersonId)),
            file = here(proj_root_dir, "tables", str_c("ParticipantLevelData_", date_max, ".csv")),
            row.names=FALSE)
  
  
  # Save enrollment table to PDF -----------------------------------------------
  # Set caption
  cap <- str_c("**PATHWEIGH:** Enrollment table for data delivery ", date_max)
  
  # Save table to pdf
  data %>%
    select(-Arb_PersonId, -Age, -`Age Unit`) %>%
    # could add the ordering for the enrollment table here
    #select(White, Black, `American Indian`, Hawaiian, `More than one race`, Unknown) %>%
    gtsummary::tbl_summary(by = Ethnicity) %>%
    gtsummary::add_overall() %>%
    gtsummary::modify_caption(cap) %>%
    gtsummary::as_gt() %>%
    gt::gtsave( # save table as image
      filename = here(proj_root_dir, "tables", str_c("enrollment_table_", date_max, "_", Sys.Date(), ".pdf")))
  
}
