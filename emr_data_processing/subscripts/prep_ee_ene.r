# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, Ph.D. CU Anschutz Dept. of Family Medicine
# Description:
# This script will do the following
# 2. Create N_days after the IndexVisit
# 3. Create N_months after the IndexVisit
# 4. Create Year and Quarter variables
# 5. Create the Censor variable
# 6. Create the LastVisit indicator variable
# 7. Create N_months after the last visit
# 8. Capture the labs, medications and EOSS
# 9. Capture comorbidities at the index and last visits in each phase
# 10. Create Weight_kgs variable
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prep_ee_ene <- function(data) {

  # Censor visits --------------------------------------------------------------
  # Censors visits in which in which the intervention WPV occurred before the
  # control visit or where the PW_flow of WMQ were used in the control phase
  # Censoring was previously downstream, but had to be moved up because the
  # index date of control visits that occurred after the intervention index had
  # to be modified. Placing the algorithm here will ensure that these visits
  # have the appropriate N_days/N_months after the index date set.
  data <- censor_visits(data, version = 1)

  # Days since the Index Visit  ------------------------------------------------
  data %<>%
    mutate(N_days_post_id = as.numeric(EncounterDate - IndexDate))

  # Months since Index Visit ---------------------------------------------------
  # Determines the number of months after the index date
  # Create a lubridate interval value and then perform integer division by
  # one month as a date value. Integer division %/% is the opposite of the
  # modulo %%. 10 %/% 3 = 3; 10 %% 3 = 1. 
  data %<>%
    mutate(
      N_months_post_id =
        lubridate::interval(IndexDate, EncounterDate) %/% months(1)
    )


  # Set Year_Quarter relative to study start date ------------------------------
  # Year 0 March 17, 2020 - March 16, 2021, Baseline
  # Year 1 March 17, 2021 - March 16, 2022, Group 1 crosses over to intervention
  # Year 2 March 17, 2022 - March 16, 2023, Group 2 crosses over to intervention
  # Year 3 March 17, 2023 - March 16, 2024, Group 3 crosses over to intervention

  # Quarter is set relative to the study start date 3-17-2020
  # Quarter 1 March 17 - June 16
  # Quarter 2 June 17 - Sept 16
  # Quarter 3 Sept 17 - Dec 16
  # Quarter 4 Dec 17 - March 16

  data %<>%
    mutate(
      Year = case_when(
        EncounterDate < "2021-03-17" ~ "Year0",
        EncounterDate >= "2021-03-17" & EncounterDate < "2022-03-17" ~ "Year1",
        EncounterDate >= "2022-03-17" & EncounterDate < "2023-03-17" ~ "Year2",
        EncounterDate >= "2023-03-17" & EncounterDate < "2024-03-17" ~ "Year3",
        EncounterDate >= "2024-03-17" & EncounterDate < "2025-03-17" ~ "Year4",
        EncounterDate >= "2025-03-17" & EncounterDate < "2026-03-17" ~ "Year5"
      ),
      Month_Day = format(EncounterDate, "%m-%d"),
      Quarter = case_when(
        Month_Day >= "03-17" & Month_Day < "06-17" ~ "Q1",
        Month_Day >= "06-17" & Month_Day < "09-17" ~ "Q2",
        Month_Day >= "09-17" & Month_Day < "12-17" ~ "Q3",
        Month_Day >= "12-17" | Month_Day < "03-17" ~ "Q4"
      ),
      Year_Quarter = str_c(Year, "_", Quarter)
    ) %>%
    select(-Month_Day, -Quarter)

  # Set Last visit and  Last visit w/ weight -----------------------------------
  # Set the last visit for each patient in each phase with and without weight
  # n.b. Creates two "LastVisit" columns, one for the visit with weight and one
  # for any visit to capture labs, procedures, medications, etc.
  # 2024-10-23 last visit was made from last visit with weight for the pp paper
  # to make the last followup visit in the modeling data set as the reference
  # to capture labs, procedurers, etc.
  data <- set_last_visit(data)

  # Categorize N_months_post_lv ------------------------------------------------
  # n.b. will only calculate for observations that are the last visit
  # could try a group_by(Arb_PersonId, Intervention) %>% fill() if those values
  # are needed.
  data %<>%
    mutate(
      N_months_post_lv =
        ifelse(LastVisit == 1,
          round(as.numeric(EncounterDate - IndexDate) / 30, digits = 0),
          NA)) %>%
    mutate(N_months_post_lv_cat = case_when(
      N_months_post_lv < 7 ~ "0-6",
      N_months_post_lv >= 7 & N_months_post_lv < 12 ~ "7-12",
      N_months_post_lv >= 13 & N_months_post_lv < 18 ~ "13-18",
      N_months_post_lv > 18 ~ "18+"
    ))

  # Modify the weight variable ---------------------------------------------
  # Modifies the weight variable in the input data frame so that the
  # weight value captured at the baseline index visit is set to be used as a
  # covariate and the weight values from non-index visits are set to be used as
  # dependent variables.
  data %<>%
    mutate(Weight_bl = ifelse(IndexVisit == 1, Weight_kgs, NA),
           Weight_dv = ifelse(IndexVisit != 1, Weight_kgs, NA))

  # Fill the NA Weight_bl values with the available value at the index visit
  # in each intervention phase
  data %<>%
    group_by(Arb_PersonId, Intervention) %>%
    arrange(Intervention, desc(IndexVisit), EncounterDate) %>%
    fill(Weight_bl, .direction = "down") %>%
    ungroup()

  # Create Any_PW_Visit variable -------------------------------------------
  # This function sets a binary indicator to each encounter if any of the
  # pathweigh tools are used.
  data %<>%
    set_any_pw_visit(.)

  return(data)
}