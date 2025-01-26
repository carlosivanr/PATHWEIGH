# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, Ph.D. CU Anschutz Dept. of Family Medicine
# Description:
# process ee and ene (eligible and enrolled and eligible not enrolled data)
# This script was created to process the ENE and EE data in the same way
# to obtain the same outcome variables for any potential comparisons
# 02/02/2024: Initial creating to consolidate data processing functions
# 08/09/2024: Major updates to store the created columns resulting from the
#             proc_ee_ene() function. To avoid having to re-process the data
#             when changes to data handling are made, the script now merges in
#             the necessary columns from the stored data.
# 09/11/2024: Additional updates to break apart the labs/procedures, eoss, and
#             comorbidities processing into separate steps for better trouble
#             shooting and for parallel processing. Modified the comorbidities
#             step to resemble labs/procedures and eoss.
# 01/24/2025: Updated Meds_weight_gain and Meds_weight_loss, since they were
#             placed on the back burner to prioritize N_meds_aom

# Capturing, labs/procedures, referrals, meds, eoss, and comorbidities is one of
# the most time consuming sections of the pathweigh data processing pipeline. In
# order to facilitate pipeline development, labs/procedures, meds, eoss, and
# comorbidities are processed once then stored. The purpose is to re-use the
# captured data if and when changes to the pipeline are made. Instead of
# capturing these data from scratch every single time, it should retrieve them.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Set data_file to the processed ee_ene data which contains all of the
# labs procedures, comorbidities, etc. if already processed.
data_file <- here(
                  str_c("delivery_", data_delivery_date),
                  "data",
                  str_c("processed_ee_ene_", RData))

# If processed data exists, load it and merge it with the previous step's data
# in the pipeline. Otherwise process the data and store it.
if (file.exists(data_file)) {
  # processed ee_ene data is saved as proc_ee_ene.Rdata. In order to avoid,
  # naming confusiong with proc_ee_ene() function. The following function
  # allows to load proc_ee_ene.RData as processed_ee_ene in the workspace.

  source("D:/PATHWEIGH/emr_data_processing/functions/load_rdata.R")

  # Load the processed ee_ene, that already has labs, procedures, and
  # comorbidities to avoid capturing them again when the script is run
  processed_ee_ene <- load_rdata(data_file)

  # If all of the encounters in ee_ene are in processed_ee_ene, then merge in
  # the labs, procedures, referrals, eoss, and comorbidities.
  if (all.equal(sort(ee_ene$Arb_EncounterId), sort(processed_ee_ene$Arb_EncounterId))) { # nolint: line_length_linter.
    
    # Load the column names that were added if the comorbidities were processed
    # previously processed. This will ensure that the column names that will be
    # merged will match with downstream steps
    load("D:/PATHWEIGH/delivery_20240917/data/new_col_names_20240917.RData")
    
    ee_ene <-
      left_join(ee_ene,
                (processed_ee_ene %>%
                   select(Arb_PersonId,
                          Arb_EncounterId,
                          EncounterDate, all_of(new_col_names))),
                by = c("Arb_PersonId", "Arb_EncounterId", "EncounterDate"))
  } else {
    stop("Not all visits in the input data frame match data that was previously processed. Review code.") # nolint: line_length_linter.
  }

} else {
  # If the file does not exist, capture labs/procedures, eoss, and comorbidities
  # Set the future plan and options to exceed default memory limits
  plan(multisession, workers = 4)

  # Set the max globals size to 10 GB
  options(future.rng.onMisuse = "ignore",
          future.globals.maxSize = (10 * 1024^3))

  # PROC LABS MEDS PROCEDURES --------------------------------------------------
  # *** n.b. meds, bariatric procedures, and referrals captured at the last
  # visit are not valid, because their time window to capture these metrics is
  # from the date of the last visit to either the cross over date for control
  # phase visits, or 9-16-2024 for the intervention visits.

  # Get the names before processing to be able to capture which columns were
  # added
  names_1_pre <- names(ee_ene)

  source(str_c(emr_dir, "subscripts/proc_labs_meds.R"))
  invisible(gc())
  ee_ene <- proc_labs_meds(ee_ene)

  # Get the names after processing to get the new columns
  names_1_post <- names(ee_ene)

  # Get the difference of the new names added to get the added columns
  names_1 <- names_1_post[!names_1_post %in% names_1_pre]

  # PROC EOSS ------------------------------------------------------------------
  names_2_pre <- names_1_post
  source(str_c(emr_dir, "subscripts/proc_eoss.R"))
  invisible(gc())
  ee_ene <- proc_eoss(ee_ene)

  names_2_post <- names(ee_ene)

  names_2 <- names_2_post[!names_2_post %in% names_2_pre]

  # PROC COMORBIDITIES ---------------------------------------------------------
  names_3_pre <- names_2_post

  source(str_c(emr_dir, "subscripts/proc_comorbidities.R"))
  invisible(gc())
  ee_ene <- proc_comorbidities(ee_ene)

  names_3_post <- names(ee_ene)

  names_3 <- names_3_post[!names_3_post %in% names_3_pre]

  # Save processed ee_ene added columns only for future use to merge in
  # labs, meds, procedures, eoss, and comorbidities
  # processed_ee_ene <-
  #   ee_ene %>%
  #   select(Arb_PersonId, Arb_EncounterId, EncounterDate,
  #          # These are the column names in ee_ene not in visits
  #          all_of(names(ee_ene)[!names(ee_ene) %in% names(visits)]))

  # Collect all of the added column names and select them along with the
  # Arb_PersonId, Arb_EncounterId, and EncounterDate

  new_col_names <- c(names_1, names_2, names_3)

  processed_ee_ene <-
    ee_ene %>%
    select(Arb_PersonId, Arb_EncounterId, EncounterDate,
           all_of(new_col_names))

  # save new_col_names
  save(new_col_names,
       file = here(str_c("delivery_", data_delivery_date),
                   "data",
                   str_c("new_col_names_", RData)))

  # save processed ee_ene
  save(processed_ee_ene,
       file = here(str_c("delivery_", data_delivery_date),
                   "data",
                   str_c("processed_ee_ene_", RData)))

  rm(processed_ee_ene)
}