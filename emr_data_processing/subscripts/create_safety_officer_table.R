#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Medical Campus, Dept. of Family Medicine
# Mar 29, 2023
# Create safety officer table

# Description: This function will generate a gt_summary table in quarto format
# displaying the differences between intervention and control patients along a
# number of lab values, deaths, and hospitalizations

# Dependencies: This script was designed to work on the visits_post_id data 
# frame and the packages loaded in 02_process_rdata_image.R. This script also
# requires the safety_officer_table_layout.qmd file located in the 
# emr_data_processing/report_layouts folder.

# The rationale for requiring a separate .qmd file for this script is because 
# the SOT report requires a combination of text and tables which cannot be 
# accomplished in a regular R script.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# create_safety_officer_table <- function(data, date_1, date_2){ #older version of the function
create_safety_officer_table <- function(data, date_1 = NULL, date_2) {
  # Set date_1 to be 1 year prior to the input date
  date_1 <- date_max - 365
  
  # if Rdata is after 10-17-22 then the necessary variables for this script
  # will be available, i.e. it will not work with deliveries prior to 10-17-22.
  if (date_max > lubridate::ymd("20221017")) {
    
    # Create the safety officer table data frame -------------------------------
    # Captures one encounter per patient at their last visit. Last visit is 
    # defined as the last visit in each phase per patient where weight was 
    # captured regardless of WPV. In addition, data are filtered to look back
    # one year in the past.
    
    # The problem with the SOT is that the algorithm is set up to capture data 
    # from the last visit in the previous year. At a certain point, all 
    # encounters will be in the intervention phase because of the date window to 
    # restrict data to the prior year. There will not be any control phase 
    # visits to compare to intervention phase visits.
    
    # Possible solutions:
    # Create a different report layout template so that it doesn't break when
    # the tbl summary function does not detect any control phase visits.
    
    # Determine which algorithm to use for subsetting data, currently both are
    # the same.
    if (date_max >=  lubridate::ymd("20231010")) {
      
      # This is the original version of the sot data creation algorithm
      # kept here until modifications are determined
      sot <- 
      data %>%
        filter(Censored == 0,
               LastVisit == 1,
               EncounterDate >= date_1,
               EncounterDate <= date_2) %>%
        group_by(Arb_PersonId) %>%
        arrange(EncounterDate) %>%
        slice_tail(n = 1) %>%
        ungroup()
      
      # Code to investigate what is happening with the data
      # data %>% 
      #   filter(Censored == 0,
      #          # EncounterDate >= date_1,
      #          # EncounterDate <= date_2,
      #          LastVisit == 1) %>% 
      #  filter(Arb_PersonId == 2582832)
      
      
      } else {
        
        # This was the algorithm used for the datasets from 2022-10-17 to
        # 2023-03-23 which is kept here for reproducibility of prior sot but
        # may not have been the most accurate
        sot <- data %>%
          filter(Censored == 0,
                 LastVisit == 1,
                 EncounterDate >= date_1,
                 EncounterDate <= date_2) %>%
          group_by(Arb_PersonId) %>%
          arrange(EncounterDate) %>%
          slice_tail(n = 1) %>%
          ungroup()
      }
    
    # If the max date is greater than or equal to 2024-03-26 then capture the
    # control level data from the previous year available for control
    if (date_max >=  lubridate::ymd("20240326")) {
      # The previously available comparison one-year-period for control was from
      # 2022-03-17 through 2023-03-16
      ctrl_comparison <- 
      data %>%
        filter(Censored == 0,
               LastVisit == 1,
               EncounterDate >= "2022-03-17",
               EncounterDate <= "2023-03-16",
               Intervention == 0) %>%
        group_by(Arb_PersonId) %>%
        arrange(EncounterDate) %>%
        slice_tail(n = 1) %>%
        ungroup()
      
      ctrl_comparison %<>%
        mutate(IndexDate = EncounterDate)
      
      ctrl_comparison %<>% 
        select(-(O2CPAPBIPAP:Ref_WellnessClinic))
      
      ctrl_comparison <- labs_procedures_sot(ctrl_comparison)
    }

      
    
    # Modify the index date to reflect the encounter date instead since the 
    # safety officer table values are captured from the last visit and not the
    # index. Since the labs and procedures algorithm uses the IndexDate, the 
    # the Index date is modified to contain the EncounterDate
    sot %<>%
      mutate(IndexDate = EncounterDate)
    
    
    # Remove the labs and procedures from the dataset so that these can be
    # replaced with updated values
    sot %<>% 
      select(-(O2CPAPBIPAP:Ref_WellnessClinic))
    
    # Get new labs & procedures by applying the labs_procedures_sot() function 
    # The input sot needs to be one row per patient or else it will throw an 
    # error when trying to merge data.
    sot <- labs_procedures_sot(sot)
    invisible(gc())
    
    names(sot)
    
    if(date_max >=  lubridate::ymd("20240326")){
      sot <- bind_rows(sot, ctrl_comparison)
    }
    
    # Import the Admission data frame ------------------------------------------
    # Temporarily sets the working directory to the data delivery directory 
    # relative to the root project directory, then reverts back, to import the
    # appropriate data
    setwd(here())
    # setwd(str_c("../../../PATHWEIGH_DATA_SECURE/", data_delivery_date))
    setwd(str_c("D:/PATHWEIGH/data_raw/", data_delivery_date))
    file <- dir()[grepl("Admission", dir())] #Search for a file with "admission" in the name
    admissions <- data.table::fread(file,
                        header = TRUE, 
                        integer64 = "numeric", 
                        na.strings = c("", "NA"))
    setwd(here())
    
    # Pare & prep admissions ---------------------------------------------------
    admissions %<>% 
      mutate(AdmissionDate = as.Date(AdmissionInstant)) %>%
      filter(Arb_PersonId %in% sot$Arb_PersonId,
             AdmissionSource != "Transfer From Same Hospital",
             AdmissionDate >= date_1,
             AdmissionDate <= date_2)
    
    # Any_Hospitalizations -----------------------------------------------------
    sot %<>%
      mutate(any_hosp = ifelse(Arb_PersonId %in% admissions$Arb_PersonId, 1, 0))
    
    # Hospitalizations ---------------------------------------------------------
    # Count the number of hospitalizations per patient
    hospitalizations <- 
      admissions %>%
      group_by(Arb_PersonId) %>%
      count() %>%
      rename(n_hospitalizations = n) %>%
      ungroup()
    
    # Merge sot and hospitalizations
    sot <- 
      left_join(sot, hospitalizations, by = "Arb_PersonId")
    
    # Any of those with NA in hospitalization are set to 0
    sot %<>%
      mutate(n_hospitalizations = ifelse(is.na(n_hospitalizations), 0, 
                                         n_hospitalizations))
    
    # Deaths -------------------------------------------------------------------
    sot %<>%
      mutate(death = ifelse(is.na(DeathDate_CDPHE), 0, 1))
    
    # Safety Officer Table -----------------------------------------------------
    sot %<>%
    select(Intervention,
           Systolic_blood_pressure:Temperature,
           A1C:eGFR,
           O2CPAPBIPAP,
           death,
           n_hospitalizations,
           any_hosp)
    
    # Save sot to .Rdata so that it can be loaded by a Quarto (.qmd) file
    save(sot, file = here("data", str_c("sot_", RData)))

    # Set the input file path to the safety_officer_table layout in the emr
    # data processing scripts directory.
    if(date_max >=  lubridate::ymd("20240326")){
      file_in  <-  str_c(emr_dir, "report_layouts/safety_officer_table_layout2.qmd")
      
      # Set the intended file path of the rendered output to move and delete 
      # residual files
      html_output <- str_c(emr_dir, "/report_layouts/safety_officer_table_layout2.html")
    
    } else{
      
      file_in  <-  str_c(emr_dir, "report_layouts/safety_officer_table_layout.qmd")
      html_output <- str_c(emr_dir, "/report_layouts/safety_officer_table_layout.html")
    }
    
    # Set the output file name to include the data delivery date and file type
    file_out <- str_c("safety_officer_table_", lubridate::ymd(data_delivery_date), "_", Sys.Date(), ".html")
    
    # Render
    quarto::quarto_render(
      input = file_in,
      execute_params = list(data = RData))
    
    
    
    # Couldn't get file paths to output directly to the intended directory. The 
    # following chunks were written to hack around that problem by copying the
    # rendered output to the target directory and then deleting the residual
    # files in the directory they were made.
    
    # Copy the html output to the target directory and rename it.
    invisible(
    file.copy(
      from = html_output,
      to = here("tables", file_out),
      TRUE))
    
    # Remove the original html output file to clean up the report_layouts
    # directory
    invisible(
    file.remove(from = html_output))
  }
}
