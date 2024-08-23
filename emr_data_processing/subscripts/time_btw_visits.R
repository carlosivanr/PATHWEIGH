# Averag number of WPVs and time between visits %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculates the average number of WPVs per enrolled patient
# Counts the average number days between WPVs per each enrolled patient
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Average number of WPVs per patient in the time period
time_btw_visits <- function(temp){
  
# The number of WPVS per patient
number_of_WPVs <- temp %>% 
  group_by(Arb_PersonId) %>%
  summarize(count = sum(WPV_v2)) %>%
  filter(count > 0)

# Average number of WPVs in the period
print(paste0("The average number of weight prioritized visits in the period: ",
       round(mean(number_of_WPVs$count),2)))

# Minimum
print(paste0("The minimum number of visits: ", min(number_of_WPVs$count)))

# Maximum
print(paste0("The maximum number of visits: ", max(number_of_WPVs$count)))


# Average time in between WPVs for those with greater than 2 visits
patients_gt_2_visits <- filter(number_of_WPVs, count > 1)

# Filter those that have more than two visits
time_btw_WPVs <- filter(temp, 
                        Arb_PersonId %in% patients_gt_2_visits$Arb_PersonId,
                        WPV_v2 == 1) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  summarise(avg_time_btw_visits = mean(diff(EncounterDate)))

# Average time in between visits for those with more than 2 visits
print("Average time between WPVs for patients with at least two WPVs: ")
print(mean(time_btw_WPVs$avg_time_btw_visits))
}