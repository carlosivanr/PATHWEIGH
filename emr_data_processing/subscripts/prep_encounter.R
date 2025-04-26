# Prep Encounter Table %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Height - set to NA if -999
# 2. Weight - set to NA if -999 and creates Weight_lbs
# 3. Insurance - set Financial class to factor with commercial, self-pay,
#     medicaid, and medicare as levels
# 4. Smoking Status
#   a. create new var set to factor with current, former, never, unknown as
#      levels
#   b. set original var to factor with specified levels
# 5. Vitals - set to NA if *Restricted, across HR, BR, BP, Temp, etc.
# 6. BMI
#   a. Compute BMI if BMI not available and height and weight are available
#   b. Set BMI > 65 to NA
#   c. Use BMI is set from BMI_comp unless BMI is available
# 7. Removes ProviderName
# 8. Only visits with a valid NPI are eligible for index visit/index date

# Dependencies:
# clean_asterisks.R function
# check_duplicates.R function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prep_encounter <- function(temp) {

  # Provider Name --------------------------------------------------------------
  # Remove provider name from the data to maintain privacy
  temp %<>%
    select(-ProviderName, -HeightMeasuredDate)

  # Fill in missing values of height from the last available value
  temp %<>%
    # filter(Arb_EncounterId %in% test$Arb_EncounterId) %>%
    arrange(Arb_PersonId, EncounterDate) %>%
    group_by(Arb_PersonId) %>%
    fill(Height, .direction = "down") %>%
    ungroup()


  # Set credible weight and height bounds --------------------------------------
  # Exclude encounters in which abnormal heights or weights are recorded to
  # guard against possible data entry errors.

  # Create Weight_lbs since Weight unit is in ounces
  temp %<>%
    mutate(Weight_lbs = Weight / 16) %>%
    select(-Weight)

  # Exclude encounters in which abnormal heights are likely to be erroneous
  # 600 lbs is set as highest credible weight, by setting their weight values to
  # NA
  temp %<>%
    mutate(Weight_lbs_bu = Weight_lbs) %>%
    mutate(Weight_lbs = ifelse(Weight_lbs > 600 | Weight_lbs < 80, NA,
                               Weight_lbs))

  # Exclude encounters in which abnormal height may have been recorded by
  # setting their values to NA
  temp %<>%
    mutate(Height_in_bu = Height) %>%
    mutate(Height = ifelse(Height > 90 | Height < 54, NA, Height))

  # Set height and weigh to metric scale ---------------------------------------
  temp %<>%
    mutate(Height_cm = Height * 2.54,
           Weight_kgs = Weight_lbs / 2.20462)

  # Factor and Collapse Insurance Categories -----------------------------------
  commercial <- c("CU - UA NET", "Managed Care", "Special Accounts", "Tricare",
                  "UCHEALTH EMPLOYEE PLAN", "Worker's Comp")
  self_pay <- c("Indigent Care")
  medicaid <- c("Managed Medicaid",
                "Colorado Medicaid",
                "Out of State Medicaid")
  medicare <- c("Managed Medicare", "Medicare")

  # Collapse factors for financial class
  temp %<>%
    mutate(Insurance = as.factor(FinancialClass),
           Insurance = fct_collapse(Insurance,
                                    Commercial = commercial,
                                    `Self-Pay` = self_pay,
                                    Medicaid = medicaid,
                                    Medicare = medicare)) %>%
    select(-FinancialClass)

  # Set the levels for Insurance
  temp %<>%
    mutate(Insurance = factor(Insurance,
                              levels = c("Commercial",
                                         "Medicare",
                                         "Medicaid",
                                         "Self-Pay")))

  # Smoking --------------------------------------------------------------------
  # Create a new variable of collapsed smoking categories
  # current, former, never, other

  # The values of smoking status has been known to change across deliveries
  # the purpose of this section is to compare the actual values available
  # witht he expected values. If they don't match up, then throw an error.

  # Check the actual labels with the expected labels,
  # Actual labels
  actual_smoke <- names(table(temp$Smoking_Status))

  #order actual labels
  actual_smoke <- actual_smoke[actual_smoke %>% str_order()]

  # Expected labels
  # Smoking status labels changed in 20221017 delivery
  current <- c("Every Day", "Some Days",
               "Heavy Smoker", "Light Smoker")

  former <- c("Former")

  never <- c("Never", "Passive Smoke Exposure - Never Smoker")

  unknown <- c("Smoker, Current Status Unknown", "Unknown",
               "Never Assessed")

  # Concatenate labels
  expected_smoke <- c(current, former, never, unknown)

  # Order the expected labels
  expected_smoke <- expected_smoke[expected_smoke %>% str_order()]

  # Check if the labels match, if the sum does not equal 10 then there is
  # a mismatch
  if (sum(expected_smoke == actual_smoke) == 10) {
    temp$SmokingStatus <- temp$Smoking_Status %>%
      as_factor() %>%
      fct_collapse(.,
                   "Current" = current,
                   "Former" = former,
                   "Never" = never,
                   "Unknown" = unknown) %>%
      recode_factor(., Unknown = NA_character_)

  } else {
    stop("Smoking Status for the specified RData file has not been incorporated into processing stream. Review prep_encounter() function and Smoking Status variables before proceeding.")
  } # close the if statement


  # Modify vitals --------------------------------------------------------------
  # Modify vitals based on pre-specified reasonable ranges
  # Prevents analyzing values that could have been due to data entry errors
  source(str_c(emr_dir, "subscripts/", "reasonable_ranges.R"))
  temp <- reasonable_ranges(temp)

  # BMI ------------------------------------------------------------------------
  # Create a computed BMI variable based on Height and Weight, set any BMIs
  # greater than 65 to NA, then create the BMI_use variable in which values are
  # set to BMI_comp if BMI is NA.
  temp %<>%
    mutate(BMI_comp = (Weight_lbs) / (Height^2) * 703) %>%
    mutate_at(c("BMI", "BMI_comp"), ~ifelse(. > 65, NA, .)) %>%
    mutate(BMI_use = BMI,
           BMI_use = ifelse(is.na(BMI), BMI_comp, BMI_use)) %>%
    select(-BMI, -BMI_comp, -Smoking_Status) %>%
    rename(BMI = BMI_use, Smoking_Status = SmokingStatus)


  # Check for the number of clinics --------------------------------------------
  # Although 57 clinics were randomized, there are actually only 56. One clinic
  # had two separate DepartmentEpicIds due to having two locations.
  if (length(table(temp$DepartmentEpicId)) != 57) {
    warning("The number of clinics in the encounter table does not equal 57!")
  }

  return(temp)

}
