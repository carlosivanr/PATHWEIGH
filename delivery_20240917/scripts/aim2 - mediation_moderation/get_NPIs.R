# Get NPIs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine

# Get the NPIs for all providers that provided visits for those in the Aim1A
# sample of 9,358.

# END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load Libraries
library(tidyverse)

# Load data
load("D:/PATHWEIGH/delivery_20240917/data/pp_data_20240917.RData")


# Load all of the visits post id_data
load("D:/PATHWEIGH/delivery_20240917/data/visits_post_id_20240917.RData")

# Get Unique NPIs
output <- visits_post_id %>%
  filter(Arb_EncounterId %in% pp_data$Arb_EncounterId) %>%
  select(ProviderNpi) %>%
  distinct()

# Export to .csv
write_csv(output, file = "D:/PATHWEIGH/delivery_20240917/data/aim1a_provider_npis.csv")

