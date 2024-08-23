# Summaries/Excluded Patients %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# When set to TRUE in the main script, the following code will print the number
# of patients meeting the following criteria:
# 1. Patients that are Eligible with weight over 600lbs
# 2. Encounters that are Eligible with weight over 600 lbs
# 3. Patients that are Eligible with height less than 54 inches
# 4. Encounters that are Eligible with height less than 54 inches
# 5. Patients that are Eligible with height over 90 inches 
# 6. Encounters that are Eligible with height over 90 inches 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Summaries of Excluded patients - as these thresholds represent unreasonable 
# values per Leigh's criteria for weight and height
patient_summary <- function(df){
  print(paste0("# Eligible Patients with Weight >600lbs: ",
       df %>% 
         filter(Weight_lbs>600, Eligible==1) %>% 
         select(Arb_PersonId) %>%
         n_distinct()))
  
  print(paste0("# Eligible Patient Encounters with Weight >600lbs: ",
       df %>% 
         filter(Weight_lbs>600, Eligible==1) %>% 
         select(Arb_EncounterId) %>%
         n_distinct()))

  print(paste0("# Eligible Patients with Height < 4.5': ",
         df %>% 
           filter(Height<54, Eligible==1) %>% 
           select(Arb_PersonId) %>%
           n_distinct()))
  
  print(paste0("# Eligible Encounters with Height < 4.5': ",
         df %>% 
           filter(Height<54, Eligible==1) %>% 
           select(Arb_EncounterId) %>%
           n_distinct()))
  
  print(paste0("# Eligible Patients with Height > 7.5': ",
         df %>% 
           filter(Height>90, Eligible==1) %>% 
           select(Arb_PersonId) %>%
           n_distinct()))
  
  print(paste0("# Eligible Patient Encounters with Height > 7.5': ", 
         df %>% 
           filter(Height>90, Eligible==1) %>% 
           select(Arb_EncounterId) %>%
           n_distinct()))
}