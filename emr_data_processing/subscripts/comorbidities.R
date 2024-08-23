#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# 09/19/2023
# Capture comorbidities at index and last visits

# The comorbidities.R script loads a function that was designed to capture 
# the comorbidities for each patient that with the restriction that the comor-
# bidity appeared at least 2 times in the medical record within a span of 2 
# years prior to the baseline index date. 

# Since interest was developed in looking at comorbidities at the last visit
# this script relies on the get_comorbidities() function to capture the 
# comorbidities at the index visit in control, intervention and at the last 
# visit in control or intervention. The last visit is the last non-index visit
# in each phase control or intervention.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load the capture_comorbidities function --------------------------------------
source(str_c(emr_dir, "subscripts/capture_comorbidities.R"))

# Create separate subsets of data to capture comorbidities ---------------------
## 1. index visits in control phase ----
ind_con <- 
  visits_post_id %>%
  filter(IndexVisit == 1,
         Intervention == 0,
         Censored == 0) 

## 2. last visits in control phase ----
lv_con <- 
  visits_post_id %>%
  filter(IndexVisit == 0,
         LastVisit == 1,
         Intervention == 0) # *** Does it matter if this subset is censored or not?

## 3. index visits in intervention phase ----
ind_int <- 
  visits_post_id %>%
  filter(Censored == 0,
         IndexVisit == 1,
         Intervention == 1)

## 4. last visits in intervention phase ----
lv_int <- 
  visits_post_id %>%
  filter(IndexVisit == 0,
         LastVisit == 1,
         Intervention == 1)

# Put dfs into a list and then process -----------------------------------------
df_list <- list(ind_con, lv_con, ind_int, lv_int)

# Pare down each data set in df_list
df_list <- 
  lapply(df_list, 
       function(x) 
         x %<>% 
         select(Arb_PersonId, IndexDate, Cohort, Arb_EncounterId, EncounterDate, Intervention))

# Apply the comorbidities function to each data set in df_list
df_list <- lapply(df_list, capture_comorbidities, 2, 2)

# capture_comorbidities(df_list[[1]], 2, 2)

# Merge the comorbidities of interest and their counts into each subset df ----
#  1. Index visits in control period 
ind_con %<>%
  left_join(., 
            df_list[[1]][["dx_sub_coi_count"]],
            by = "Arb_PersonId")

# 2. Last visits in control period, must not include index visits
lv_con %<>%
  left_join(., 
            df_list[[2]][["dx_sub_coi_count"]],
            by = "Arb_PersonId")

# 3. Index visits in intervention
ind_int %<>%
  left_join(., 
            df_list[[3]][["dx_sub_coi_count"]],
            by = "Arb_PersonId")

# 4. Last visits in control period, must not include index visits
lv_int %<>%
  left_join(., 
            df_list[[4]][["dx_sub_coi_count"]],
            by = "Arb_PersonId")

# Bind rows --------------------------------------------------------------------
# Stack the index and last visits with the comorbidities, which will replace
# the encounters in the visits_post_id data frame
comorbidities <- 
  bind_rows(ind_con, lv_con, ind_int, lv_int)

# Stack all of the visits back with visits_post_id, after removing the encounters
# from which the comorbidities were linked to i.e. index, last vist, in each
# phase
visits_post_id %<>%
  filter(!Arb_EncounterId %in% comorbidities$Arb_EncounterId) %>%
  bind_rows(., comorbidities)

# Fix the NAs ------------------------------------------------------------------
# Applied her to also address NAs in non-index and non last visit encounters for
# comorbidities
visits_post_id %<>%
  mutate(across(Dyslipidemia:`Binge eating`, ~ ifelse(is.na(.), 0, .)))

# Arrange column names by percentage -------------------------------------------
comorbidity_names <- names(
  visits_post_id %>% 
    select(Dyslipidemia:`Binge eating`)
  )

# Create an index of the order of comorbidity column names arranged by the 
# sum of the column's values. This index is used to order the columns by 
# frequency for displaying in a table
index <- 
  visits_post_id %>% 
  select(all_of(comorbidity_names)) %>%
  colSums() %>%
  order(decreasing = TRUE)

# Reorder comorbidity names
comorbidity_names <- comorbidity_names[index]

# Rearrange the columns so that the cormorbidities are ordered from most 
# frequently observed comorbidities that are observed at least 2x within the 2
# years from the index/last visit to the least frequently observed comorbidities.
visits_post_id %<>%
  select(Arb_PersonId:EOSS,
         all_of(comorbidity_names))

# Remove intermediary data frames and memory resources -------------------------
rm(ind_int,
   ind_con,
   lv_con,
   lv_int,
   df_list,
   comorbidities)

invisible(gc())

# # Generate plots of the comorbidities, for eligible and enrolled patients, i.e.
# # visits after index date since index date is required
# # Requires a dataset with comorbidities count
# #comorbidities_plot <- hist_como(visits, "Eligible and enrolled")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%