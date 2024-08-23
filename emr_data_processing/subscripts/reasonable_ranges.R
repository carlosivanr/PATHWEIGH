# Reasonable Ranges %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Converts values based on reasonable ranges provided by Leigh as values outside
# of reasonable ranges could be due to data entry errors.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reasonable_ranges <- function(temp){
  
  # HeartRate
  out_of_range_hr <- temp %>% 
    filter(HeartRate < 30 | HeartRate > 200)
  
  # Respiratory range is 6 - 50
  out_of_range_rr <- filter(temp,
                            Respiratoryrate < 6 | Respiratoryrate > 50)
  
  # How many observations that fall outside of range?
  out_of_range_bpd <-
    filter(temp,
           Diastolic_blood_pressure < 40 | Diastolic_blood_pressure > 140)
  
  # How many observations outside of temp range
  out_of_range_temp <- filter(temp, Temperature < 96)
  
  # concatenate row wise the out of range folks
  out_of_range_exclusions <- bind_rows(list(out_of_range_hr,
                                            out_of_range_rr,
                                            out_of_range_bpd,
                                            out_of_range_temp))
  
  # Remove duplicates from the data frame
  out_of_range_exclusions <- out_of_range_exclusions %>% 
    group_by(Arb_PersonId) %>%
    slice_head() %>%
    nrow()
  
  # print(paste0("The number of patients with out of range vitals is: ", 
  #       out_of_range_exclusions))
  
  # Set out of range exclusions to NA
  temp %<>%
    mutate(Temperature = ifelse(Temperature < 96, NA, Temperature),
           HeartRate = ifelse(HeartRate < 30, NA, HeartRate),
           Respiratoryrate = ifelse(Respiratoryrate < 6 | 
                                    Respiratoryrate > 50, 
                                    NA, Respiratoryrate),
           Diastolic_blood_pressure = ifelse(Diastolic_blood_pressure < 40 | 
                                             Diastolic_blood_pressure > 140, 
                                             NA, Diastolic_blood_pressure))

    
  # Return the data frame
  return(temp)
  
}

