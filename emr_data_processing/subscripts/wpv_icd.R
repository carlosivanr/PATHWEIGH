# Definte WPVs by ICD Codes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The following seres of E66 and Z68 codes are usef for defining WPVS:

# 1. E66.01 E66.09  E66.1  E66.2  E66.3  E66.8  E66.9 
# 2. Z68.25-Z68.45

# Notes from previous version:
# Z76.89 was inclusion criteria for WPV
# Ensure columns for %in% are of the same class, numeric to function properly

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wpv_icd <- function(temp){
  # 1. E66 Codes -----------------------------------------------------------------
  e66_codes <- str_c("E66.", c("01", "09", "1", "2", "3", "8", "9"), 
                     collapse = "|")
  
  e66_index <- str_detect(dx$DiagnosisCode, e66_codes)
  
  # print("Number of Encounters with E66 codes")
  # print(table(dx$DiagnosisCode[which(e66_index=="TRUE")]))
  
  
  # 2. Z68 Codes -----------------------------------------------------------------
  z68_codes <- str_c("Z68.", 25:45, collapse = "|")
  
  z68_index <- str_detect(dx$DiagnosisCode, z68_codes)
  
  # print("Number of Encounters with Z68 codes")
  # print(table(dx$DiagnosisCode[which(z68_index=="TRUE")]))
  
  # 3. Filter Dx Codes by Encounter IDs meeting an E66 or Z68 Code ---------------
  # Create a vector of encounter ids that had relevant ICD 10 codes
  icd_encounters <- names(table(
      dx$Arb_EncounterId[which(e66_index=="TRUE" | z68_index=="TRUE")])
      )
  
  # Create WPV icd codes indicator in temp
  # temp %<>%
  #   mutate(WPV_ICD = ifelse(Arb_EncounterId %in% icd_encounters, 1, 0))
  # 
  # print(paste0("Number of Encounters with a relevant ICD code: ",
  #        length(which(temp$WPV_ICD==1)))
  # )
  
  
  # 4. Create WPV icd for billing only indicator in temp
  e66_codes <- str_c("E66.", c("01", "09", "1", "2", "3", "8", "9"), collapse = "|")

  z68_codes <- str_c("Z68.", 25:45, collapse = "|")

  e66_enc_ids <-
    dx %>%
    filter(grepl("Billing",Provenance),
           str_detect(DiagnosisCode, e66_codes)) %>%
    pull(Arb_EncounterId)

  z68_enc_ids <-
    dx %>%
    filter(grepl("Billing",Provenance),
           str_detect(DiagnosisCode, z68_codes)) %>%
    pull(Arb_EncounterId)

  temp %<>%
    mutate(WPV_ICD = ifelse(Arb_EncounterId %in% c(e66_enc_ids, z68_enc_ids), 1, 0))
  
  # sum(temp$WPV_ICD)
  # sum(temp$WPV_ICD_billing)
  
  ## Return needed here to prevent assignment of print statement to output
  return(temp)
}
