# Comorbidities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Description:

# This subscript will utilized either the dx or dx_co data to extract all of the
# diagnosis for the patients of interest and arranges the data in wide format
# that is then joined to the visits data frame. In addition, this script will 
# calculate the total number of comorbidities for each patient at the index
# visit. The rationale for this approach is to be able to create a table



# Requirements:
# Visits
# dx or dx_co
# comorbidities of interest .RData file



# Creates a data frame of the most commonly observed comorbidities to merge with
# input data. Only the top 3% of comorbidities of interest are merged. Columns 
# will reflect a disease state and values will be binary to be used with creating
# tables with gtsummary package.

# Usage:
# data <- comorbidites(data, dx) # where data is the data frame to merge into,
# and dx is the Compass table for diagnoses. Could use dx or dxco. Dx is for 
# diagnoses at the encounter and dxco are for diagnoses anywhere in the EMR.

# Parameters to decide on:
# 1. Which table to use Dx or Dxco?
# 2. How far back to look for a comorbidity?
# 3. How far forward to look for a comorbidity? I.e. do we count those that
#     occurred after the index date?
#     What about NAs?
# 4. How many times must a comorbidity be diagnosed?
# 5. Which provenance values do we use, what about NAs?
# 6. Calculate separate medians for each cohort.
# 7. Use the baseline index visit only.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comorbidities <- function(visits, dx){
  # Load comorbidities of interest here so that it is only loaded once
  # *** update comorbidities of interest with Leigh's new file 11/15/2022
  load(here("../working_files/comorbidities/comorbidities_of_interest_220121.RData"))
  
  
  # Get the comorbidities functions ____________________________________________
  get_comorbidities <- function(temp){
    
    # Set parameters -----------------------------------------------------------
    # Set the number of times a diagnosis is to be detected to classify it as
    # a comorbidity
    co_times <- 2
    
    
    # 1. Prep dx_sub -----------------------------------------------------------
    # 1.1 Create a subset of the diagnoses data frame that is filtered by the 
    # patient ids in the visits (temp) data frame and for ICD_headers in the
    # comorbidities of interest data frame (cois).
    dx_sub <- 
      dx %>% 
      mutate(ICD_Header = sub("\\..*", "", DiagnosisCode),
             code = sub("\\.", "", DiagnosisCode)) %>%
      filter(ICD_Header %in% cois$ICD_Header,
             Arb_PersonId %in% temp$Arb_PersonId)
    
    # 1.2 merge the Index Date and Cohort into dx_sub by person id
    dx_sub %<>%
      left_join(., 
                select(temp, all_of(c("Arb_PersonId", "IndexDate", "Cohort"))), 
                by = "Arb_PersonId")
    
    # 1.3 merge the encounter date into dx sub by encounter id to capture a 
    # DiagnosisDate from the EncounterDate in the case that there is no 
    # DiagnosisDate available in dx_sub
    dx_sub %<>%
      left_join(., 
                select(temp, all_of(c("EncounterDate", "Arb_EncounterId"))), 
                by = "Arb_EncounterId") %>%
      mutate(DiagnosisDate = if_else(is.na(DiagnosisDate), EncounterDate, DiagnosisDate)) %>%
      select(-EncounterDate)
    
    # 1.4 merge the unique category and disease.state values from cois into 
    # dx_sub by ICD_Header. ICD_Header was chosen as the matching variable
    # because some comorbidities of interest may have been excluded when relying
    # on the full code variable such as F32.A.
    dx_sub %<>%
      left_join(., 
                distinct(select(cois, all_of(c("category", "disease.state", "ICD_Header")))), 
                by = "ICD_Header")
    
    
    # 2 Prep dx_sub_coi_count --------------------------------------------------
    # Group dx_sub by patient and disease.state. Disease.state is a character
    # variable used in place of an ICD_header and displays text in tables rather
    # than Icd header/prefix
    dx_sub_coi_count <- dx_sub %>% 
      group_by(Arb_PersonId, disease.state) %>%
      count() %>%
      ungroup() %>%
      filter(n >= co_times) %>%
      mutate(n = 1) %>%
      pivot_wider(names_from = disease.state, values_from = n) 
    
    # Get the names of the comorbidities when using disease.state
    test_como <- dx_sub_coi_count %>%
      select(-Arb_PersonId) %>%
      names() %>%
      data.frame
    
    test_como <- data.frame(disease.state = test_como) %>%
      left_join(., distinct(select(cois, disease.state, ICD_Header)), by = "disease.state")
    
    test_como <- test_como %>% select(ICD_Header, everything())
    
    
    
    # ICD_headers when grouping by ICD_Header 
    # *** Check why the number of columns don't match
    # If we use the ICD_headers, then we will get extra columns for M15 M16, and M17,
    # eventhough they all represent osteoarthritis. So if we use disease.state,
    # then M15, M16, and M17 will be collapsed and could potentially alter the
    # comorbidity counts.
    ICD_headers <- dx_sub %>% 
      group_by(Arb_PersonId, ICD_Header) %>%
      count() %>%
      ungroup() %>%
      filter(n >= co_times) %>%
      mutate(n = 1) %>%
      pivot_wider(names_from = ICD_Header, values_from = n) %>%
      select(-Arb_PersonId) %>%
      names()
    
    test_icd <- data.frame(ICD_Header = ICD_headers) %>% 
      left_join(., distinct(select(cois, disease.state, ICD_Header)), by = "ICD_Header")
    
    
    
    anti_join(test_como, test_icd)
    
    # results in M15, M16, K41, Osteoarthritis, and Hernia 
    
    
    # 3. Merge visits and dx_sub_coi_count and create a variable that indicates
    # the number of comorbidities per patient ----------------------------------
    
    # Merge dx_sub_coi count with visits/temp, then change all NAs to 0, and 
    # sum across all of the different cois to get a total for each patient.
    coi_names <- names(select(dx_sub_coi_count, -Arb_PersonId))
    
    temp %<>% left_join(dx_sub_coi_count, by = "Arb_PersonId") %>%
      mutate(across(all_of(coi_names), ~ ifelse(is.na(.), 0, .))) %>%
      mutate(comorbidities = select(., all_of(coi_names)) %>% rowSums())
    
    # 4. Return the modified temp data frame
    return(temp)
    
  }
  
  
  # Apply get_comorbidities function -------------------------------------------
  temp <- visits %>%
    filter(EncounterDate == IndexDate, IndexVisit == 1, Intervention == 0) %>%
    select(Arb_PersonId, IndexDate, Cohort, Arb_EncounterId, EncounterDate, Intervention) %>%
    get_comorbidities()
  
  # Count the number of patients per cohort in temp  
  temp %>%
    group_by(Cohort) %>%
    count()
  
  # *** Could also categorize the # of comorbidities to 0, 1, 2, and 3+
  
  temp %>%
    select(Cohort, comorbidities) %>%
    mutate(comorbidities = factor(comorbidities)) %>%
    gtsummary::tbl_summary(by = Cohort)
  
  
  
  # Histogram of comorbidities function ________________________________________
  hist_como <- function(temp){
    colors = c( "#143585","#4ca4dc")
    
    temp %>%
      ggplot(., aes(x = comorbidities)) +
      geom_histogram(binwidth = 1, color = colors[1], fill = colors[2], alpha = 0.3) +
      scale_y_continuous(breaks = scales::breaks_pretty(10)) +
      theme_minimal() +
      theme(axis.line = element_line(color = "grey70"), legend.position = "none") +
      labs(x = "Number of unique comorbidities", y = "Count",
           title = str_wrap("The number of unique comorbidites per patient in the 2 years prior to the baseline index visit", 50))
  }
  
  # Plot comorbidities ---------------------------------------------------------
  ## Comorbidities Overall -----------------------------------------------------
  ggsave(
    hist_como(temp) + 
      scale_x_continuous(breaks = c(0:max(temp$comorbidities))) +
      geom_vline(aes(xintercept = median(comorbidities)), color = "red"),
    filename = here("plots", "como_distribution_overall.pdf"), device = "pdf")
  
  # Print to console the number of patients grouped by the number of comorbidities
  temp %>%
    group_by(comorbidities) %>%
    count()
  
  
  
  # Comorbidities by cohort ----------------------------------------------------
  # Calculate the median by cohort. Plotting medians by cohort requires a new df
  medians <- temp %>% 
    group_by(Cohort) %>% 
    summarise(median = median(comorbidities))
  
  ggsave(
    hist_como(temp) +
      facet_wrap(~ Cohort) +
      scale_x_continuous(breaks = scales::breaks_pretty(10)) +
      geom_vline(aes(xintercept = median), color = "red", data = medians),
    filename = here("plots", "como_distribution_by_cohort.pdf"), device = "pdf")
  
  
  # Print to console the number of patients grouped by the number comorbidities
  # and cohort
  temp %>%
    group_by(Cohort, comorbidities) %>%
    count()
  
  
  
  
  # # 3. Create top_comorbidities (top 3%) from dx_sub_coi_count
  # #   a. Count codes after grouping by disease.state
  # #   b. Sum the count for each group and arrange in descending order
  # #   c. Calculate a percent
  # #   d. Filter the comorbidities that occur in 3% or great of patients
  # top_comorbidities <- dx_sub_coi_count %>% 
  #   group_by(disease.state) %>%
  #   count(code) %>%
  #   arrange(desc(n)) %>%
  #   summarise(sum = sum(n)) %>%
  #   arrange(desc(sum)) %>%
  #   mutate(percent_overall = sum/nrow(temp) * 100) %>%
  #   filter(percent_overall >= 3)
  # 
  # # Create a wide data frame of all of the non-NA comorbidities to filter the 
  # # dx_sub_coi_count with
  # unique_disease_states <- cois %>% 
  #   filter(ICD_Header %in% dx_sub_coi_count$ICD_Header) %>% 
  #   distinct(disease.state) %>% 
  #   pull(disease.state)
  # 
  # # Use the unique_disease_states to filter dx_sub_coi_count and count the num
  # # of patients endorsing each disease state to order the disease states.
  # ordered_disease_states <- dx_sub_coi_count %>%
  #   filter(disease.state %in% unique_disease_states) %>%
  #   group_by(disease.state) %>%
  #   count() %>%
  #   arrange(desc(n)) %>%
  #   pull(disease.state)
  # 
  # # Use the ordered_disease_states to generate a table 
  # dx_sub_coi_count %>%
  #   mutate(count = 1) %>%
  #   pivot_wider(names_from = disease.state, values_from = count) %>%
  #   select(Arb_PersonId, all_of(ordered_disease_states)) %>%
  #   group_by(Arb_PersonId) %>% 
  #   summarise(across(where(is.double), ~ sum(., na.rm = TRUE))) %>%
  #   mutate(across(all_of(ordered_disease_states), ~ifelse(. > 0, 1, 0))) %>%
  #   right_join(., temp, by = "Arb_PersonId") %>%
  #   mutate(across(all_of(ordered_disease_states), ~replace_na(., 0))) %>%
  #   select(all_of(ordered_disease_states)) %>%
  #   gtsummary::tbl_summary()
  # 
  # # Need to double check the number 
  # #
  # 
  #   
  # 
  #   
  #   
  #   
  #   select(disease.state) %>%
  # gtsummary::tbl_summary(., sort = ~ "frequency")
  # 
  # # 4. Modify dx_sub_coi_count
  # #   a. Filter dx_sub_coi_count by the top comorbidities
  # #   b. Create a count column set to 1
  # #   c. Convert to wide
  # #   d. Select PersonId and all of the top_comorbidities
  # #   e. group by patient and then summarise by sum to get one count for each 
  # #     row
  # dx_sub_coi_count %<>% 
  #   filter(disease.state %in% top_comorbidities$disease.state) %>%
  #   mutate(count = 1) %>%
  #   pivot_wider(names_from = disease.state, values_from = count) %>%
  #   select(Arb_PersonId, all_of(top_comorbidities$disease.state)) %>% 
  #   group_by(Arb_PersonId) %>% 
  #   summarise(across(where(is.double), ~sum(., na.rm = TRUE)))
  
  # Test that there are no values greater than one in the comorbidities
  if (dx_sub_coi_count %>% 
      filter(if_any(!starts_with("Arb_Person"), ~.x > 1)) %>%
      nrow() != 0){
    print("QA check for values did not pass, some values greater than one!")
  } else {
    return(dx_sub_coi_count)
  }
  
}

# %%%%%%%%%%%%%%%%%%%%%%%% Vestigial Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Kept around in case the decisions on handling comorbidity data change

# Creates a look-back period and identifies comorbidities that are observed
# at least twice

# Set the number of years prior to the index date to capture comorbidities
#co_years <- 2 

# Set the vector of allowable provenance codes
# *** To be implemented in the future.
# co_prov <-  c("","","","")

# Calculate the difference between the DiagnosisDate and the IndexDate, 
# then filter only those within the specified time period prior to the Index
# Date, then count after grouping by person Id and disease.state. Finally, 
# filter out those that have the same diagnosis the number of times
# specified by the function parameters.

# nota bene:
# When subtracting DiagnosisDate from IndexDate, positive numbers represent 
# days in the past, and suggest that the Diagnosis occurred before the 
# IndexDate. Negative numbers indicate that the Diagnosis date occurred 
# after the IndexDate. Zeros indicate the diagnosis occurred on the same
# date as the IndexDate

# dptId = days prior to IndexDate
# co_years = num of years to look back
# co_time = num of times a dx must be present
# dx_sub_coi_count <- dx_sub %>% 
#   mutate(dptId = IndexDate - DiagnosisDate) %>%
#   filter(dptId <= (co_years * 365), dptId > 0) %>%
#   group_by(Arb_PersonId, disease.state) %>%
#   count() %>%
#   ungroup() %>%
#   filter(n >= co_times) %>%
#   mutate(n = 1) %>%
#   pivot_wider(names_from = disease.state, values_from = n) %>%
#   select(-Arb_PersonId) %>%
#   names()