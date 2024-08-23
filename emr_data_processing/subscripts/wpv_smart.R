# Define WPVS by use of Smart Set %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The smart table in the COMPASS data delivery contains the patient and visit
# IDs where the smart set was used. WPV_smart is defined as 1 if the encounter
# ID of the patient in the input data frame is found in the smart table.


# SMARTTOOL Logger captures the following Ids for SMARTTEXT
# Those with SMARTTEXT_ID = 2109052067 are AMB WPV NOTE
# All ecnounters are Flag == SMARTEXT
# These are supposed to define the disappearing smart text

# Those with SMARTTEXT_ID = 2107060002 are AMB FOLLOWUP VISIT WEIGHT MANAGEMENT
# All encounters are Flag == SMARTEXT

# Those with SMARTTEXT_ID = 2107060001 are AMB INITIAL VISIT WEIGHT MANAGEMENT
# All encounters are Flag == SMARTTEXT

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wpv_smart <- function(temp){
  if (RData == "20240326.RData"){
    
    # Reverted back to original algorithm that classifies any visit with
    # smart tool use as WPV_smart == 1
    temp %<>%
      mutate(WPV_smart = 
               ifelse(Arb_EncounterId %in% smart$Arb_EncounterId & 
                        Intervention == 1, 1, 0))
    # 
    # # List the structure of summary
    # # str(temp)
    # 
    # # Display the encounters with the values of Flag and Name
    # smart %>%
    #   select(Flag, Name) %>%
    #   tbl_summary()
    # 
    # # Display the names of the records where SMARTTEXT is the Flag
    # smart %>% 
    #   filter(Flag == "SMARTTEXT") %>% 
    #   select(Name) %>% 
    #   tbl_summary()
    # 
    # # Disappearing text is identified by Flag == "SMARTTEXT" & Name "AMB WPV NOTE"
    # smart %>%
    #   mutate(Type = if_else(Flag == "SMARTTEXT" & Name == "AMB WPV NOTE", "Disappearing Text", "Smart Text"),
    #          Type = if_else(Flag == "SMARTPHRASE", "Smart Phrase", Type),
    #          Type = if_else(Flag == "SMARTSET", "Smart Set", Type)) %>%
    #   select(Type) %>%
    #   tbl_summary()
    # 
    # # create the WPV_smart data element
    # temp %<>%
    #   mutate(WPV_smart = 
    #            ifelse(Arb_EncounterId %in% smart$Arb_EncounterId & 
    #                     Intervention == 1, 1, 0))

    
  } else {
    
    temp %<>%
      mutate(WPV_smart = 
               ifelse(Arb_EncounterId %in% smart$Arb_EncounterId & 
                        Intervention == 1, 1, 0))
  }
    
  return(temp)
}
