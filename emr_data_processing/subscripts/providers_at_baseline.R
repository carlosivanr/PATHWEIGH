# Providers at baseline --------------------------------------------------------
# # Create a data frame of the providers who had a WPV
# provider_npis <- visits_on_id %>%
#   ungroup() %>%
#   filter(WPV_v2 == 1) %>%
#   drop_na(ProviderNpi) %>%
#   select(ProviderNpi, Cohort)
#
# # Number of unique NPIs is 514 for the baseline period
# n_distinct(provider_npis$ProviderNpi)
#
# # Get the cohort for each NPI for cases where one provider may be associated with
# # more than one Cohort
# provider_npis <- provider_npis %>%
#   group_by(ProviderNpi) %>%
#   slice_head()
#
# # Get the number of WEIGHT PRIORITIZED VISITS by Cohort
# visits %>%
#   filter(ProviderNpi %in% provider_npis$ProviderNpi,
#          WPV_v2 == 1) %>%
#   select(Cohort, WPV_v2) %>%
#   tbl_summary(by = Cohort) %>%
#   add_overall()
#
#
# visits %>%
#   drop_na(ProviderNpi) %>%
#   filter(WPV_v2 == 1) %>%
#   select(Cohort, WPV_v2) %>%
#   tbl_summary(by = Cohort,
#               label = list(WPV_v2 ~ "Weight Prioritized Visits")) %>%
#   add_overall()

# # To get all of the visits, will need to use the encounter sub
# encounter_sub %>%
#   filter(ProviderNpi %in% provider_npis$ProviderNpi) %>%
#   left_join(., provider_npis, by = "ProviderNpi") %>%
#   mutate(All_visits = 1) %>%
#   select(Cohort, All_visits) %>%
#   tbl_summary(by = Cohort) %>%
#   add_overall()