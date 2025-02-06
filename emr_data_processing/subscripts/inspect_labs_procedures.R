#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Inspect Labs & Procedures 
# Carlos Rodriguez, PhD. Dept. of Fam. Med. Univ. of CO Anschutz
# 06/21/2023
# Description:
# This script was developed to test the latest version of the labs and 
# procedures function to inspect the number of missing values in each phase of
# the pathweigh study. The original algorithm for capturing labs and procedures
# was intended for the baseline characteristics paper. The original approach did
# not consider multiple index dates, for those that have visits in control and
# in intervention phases of the study. In addition, the data capture window of
# 14 days prior to index and 180 days after index, was done because it was un-
# certain how many missing values would be encountered in the data. 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# source(str_c(emr_dir, "subscripts/labs_procedures_(dev).R"))
library(gtsummary)

visits_post_id %>%
  filter(IndexVisit == 1,
         Censored == 0) %>%
  select(Intervention, O2CPAPBIPAP:Ref_WellnessClinic,
         -ReferralDate) %>%
  mutate(across(Ref_BariatricSurgery:Ref_WellnessClinic, ~ifelse(is.na(.), 0, .))) %>%
  mutate(across(O2CPAPBIPAP:Ref_WellnessClinic, ~ifelse(is.na(.), 1, 0))) %>%
  tbl_summary(by = Intervention,
              digits = everything() ~ 2) %>%
  modify_header(label = "**Measure**") %>%
  modify_caption("**Number of missing values by intervention index visit**") %>%
  as_gt() %>%
  gt::gtsave( # save table as image
    filename = here("tables", str_c("missing_labs_n_procedures_table_", date_max, "_", Sys.Date(), ".html"))
  )