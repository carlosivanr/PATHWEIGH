# Load the file with the patients in pp_data who have missing BMI
# Create a new algorithm to fill in height
# Then create BMI
# Then see how many are missing

# Load in the people in pp_mod data that have missing bmi
delivery <- data_delivery_date
test <- read_csv(here::here(str_c("delivery_", delivery),
                            "manuscript_tbls_figs/bmi_test_data.csv")
)


# Count the number of missing values before modification
encounter %>% 
  filter(Arb_EncounterId %in% test$Arb_EncounterId) %>%
  filter(is.na(Height)) %>%
  nrow()


# Count the number of missing values after modification
# to see if the algorithm is working.
encounter %>%
  filter(Arb_EncounterId %in% test$Arb_EncounterId) %>%
  arrange(Arb_PersonId, Arb_EncounterId) %>%
  group_by(Arb_PersonId) %>%
  fill(Height, .direction = "down") %>%
  filter(is.na(Height)) %>%
  nrow()


# Check one of the records with height still missing
test_records <- encounter %>%
  filter(Arb_EncounterId %in% test$Arb_EncounterId) %>%
  filter(Arb_PersonId == 2066784987)
