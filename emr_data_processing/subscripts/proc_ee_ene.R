# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, Ph.D. CU Anschutz Dept. of Family Medicine
# 02/02/2024
# 08/09/2024: Major updates to store the created columns resulting from the 
#             proc_ee_ene() function. To avoid having to re-process the data
#             when changes to data handling are made, the script now merges in
#             the necessary columns from the stored data.

# process ee and ene (eligible and enrolled and eligible not enrolled data)
# This script was created to process the ENE and EE data in the same way
# to obtain the same outcome variables for any potential comparisons

# This script will do the following
# 1. Create NewId variable which is a concatenation of the Arb_PersonId and the
#     intervention variable
# 2. Create N_days after the IndexVisit
# 3. Create N_months after the IndexVisit
# 4. Create Year and Quarter variables
# 5. Create Censor variable
# 6. Create the LastVisit indicator variable
# 7. Create N_months after the last visit
# 8. Capture the labs, medications and EOSS at the index and last visits in 
#     each phase
# 9. Capture comorbidities at the index and last visits in each phase
# 10. Create Weight_kgs variable


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%% TEST TO DETERMINE IF PROCESSED DATA EXISTS %%%%%%%%%%%%%%%%%%%
# If exists, load data
#   Then check if any records need processing, if so process separte into 
#     subsets. For those that do not need processing, simply merge in the labs
#     procedures, and comorbidities. For those that need proceessing, feed into 
#     proc_ee_ene(). Finally, stack the two subsets
#   if not then just merge in labs, procedures, and comorbidities
# If data doesn't exist, then process the input

data_file <- here(
  str_c("delivery_", data_delivery_date),"data", paste0("processed_ee_ene_", RData))

if (file.exists(data_file)) {
  # %%%%%%%%%%%%%%%%%%% DECLARE RDATA-LOADING FUNCTION %%%%%%%%%%%%%%%%%%%%%%%%%
  # File loading function that will allow loading a file and assign it to a 
  # different name to in the global environment
  loadRData <- function(fileName){
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  
  # Load the processed ee_ene, that already has labs, procedures, and
  # comorbidities to avoid capturing them again when the script is run
  processed_ee_ene <-loadRData(data_file)
  
  # If all of the encounters in ee_ene are in processed ee_ene, then merge in
  # the labs, procedures, referrals, eoss, and comorbidities
  
  if(all.equal(sort(ee_ene$Arb_EncounterId), sort(processed_ee_ene$Arb_EncounterId))) {
    ee_ene <-
      left_join(ee_ene,
                        (processed_ee_ene %>% select(Arb_PersonId, Arb_EncounterId, EncounterDate,
                                                     A1C:`Binge eating`)),
                        by = c("Arb_PersonId", "Arb_EncounterId", "EncounterDate"))
  } else {
    stop("Not all visits in the input data frame match data that was previously processed. Review code.")
  }
  
  } else {
    # Declare function
    proc_ee_ene <- function(data, proc_labs = TRUE, proc_dx = TRUE) {
      
      if (proc_labs == TRUE) {
        # Labs, Medications, EOSS ------------------------------------------------
        # Labs, medications, EOSS are captured at the index visit and last visit for
        # each patient in each phase. Since the same strategy of breaking apart the
        # main data frame and capturing these values applies to the subsets of the
        # data frame, this script applies all three functions to each
        # subset of data before binding them back together instead of breaking apart,
        # function, binding 3x, for labs, then for meds, then for eoss.
        # Replaced with lapply() but it
        # takes a longer to process, may need a re-write.
        source(str_c(emr_dir, "subscripts/proc_labs_meds.R"))
        invisible(gc())
        data <- proc_labs_meds(data)

        source(str_c(emr_dir, "subscripts/proc_eoss.R"))
        invisible(gc())
        data <- proc_eoss(data)

      }
      
      # Comorbidities ----------------------------------------------------------
      if (proc_dx == TRUE){
        # Clear out 
        invisible(gc())
        
        # Load dxco and dx into the workspace
        purrr::walk(
            .x  = c("dx", "dxco"),
            .f = read_pw_csv
          )
        
        # Set number of workers for furrr ----------------------------------------------
        # Sets the number of workers for parallel processing
        cores <- 4 # !!! 8 cores or more causes multisession failure
        labels <- seq_along(1:cores) # used for split_by group in nested functions
        plan(multisession, workers = cores)
        
        source(str_c(emr_dir, "subscripts/proc_comorbidities.R"))
        data <- proc_comorbidities(data)
      }
      
      return(data)
      
      invisible(gc())
      
    } # End of function 

    # Apply function
    ee_ene <- proc_ee_ene(ee_ene)
    
    # Save processed ee_ene added columns only
    processed_ee_ene <- 
      ee_ene %>%
      select(Arb_PersonId, Arb_EncounterId, EncounterDate,
             # These are the column names in ee_ene not in visits
             all_of(names(ee_ene)[!names(ee_ene) %in% names(visits)]))
    
    # save processed ee_ene
    save(processed_ee_ene, 
         file = here(str_c("delivery_", data_delivery_date), 
                     "data", 
                     paste0("processed_ee_ene_", 
                            RData)))
  }

