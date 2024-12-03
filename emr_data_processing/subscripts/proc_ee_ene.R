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

  # File loading function that will allow loading a file and assign it to a
  # different name in the global environment
  loadRData <- function(file_name) {
    #loads an RData file, and returns it
    load(file_name)
    get(ls()[ls() != "file_name"])
  }

  # Load the processed ee_ene, that already has labs, procedures, and
  # comorbidities to avoid capturing them again when the script is run
  processed_ee_ene <- loadRData(data_file)

  # If all of the encounters in ee_ene are in processed_ee_ene, then merge in
  # the labs, procedures, referrals, eoss, and comorbidities.
  if (all.equal(sort(ee_ene$Arb_EncounterId), sort(processed_ee_ene$Arb_EncounterId))) { # nolint: line_length_linter.
    ee_ene <-
      left_join(ee_ene,
                (processed_ee_ene %>%
                   select(Arb_PersonId,
                          Arb_EncounterId,
                          EncounterDate, A1C:`Binge eating`)),
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
  source(str_c(emr_dir, "subscripts/proc_labs_meds.R"))
  invisible(gc())
  ee_ene <- proc_labs_meds(ee_ene)

  # PROC EOSS ------------------------------------------------------------------
  source(str_c(emr_dir, "subscripts/proc_eoss.R"))
  invisible(gc())
  ee_ene <- proc_eoss(ee_ene)

  # PROC COMORBIDITIES ---------------------------------------------------------
  source(str_c(emr_dir, "subscripts/proc_comorbidities.R"))
  invisible(gc())
  ee_ene <- proc_comorbidities(ee_ene)

  # Save processed ee_ene added columns only for future use to merge in
  # labs, meds, procedures, eoss, and comorbidities
  processed_ee_ene <-
    ee_ene %>%
    select(Arb_PersonId, Arb_EncounterId, EncounterDate,
           # These are the column names in ee_ene not in visits
           all_of(names(ee_ene)[!names(ee_ene) %in% names(visits)]))

  # save processed ee_ene
  save(processed_ee_ene,
       file = here(str_c("delivery_", data_delivery_date),
                   "data",
                   str_c("processed_ee_ene_", RData)))

  rm(processed_ee_ene)

}