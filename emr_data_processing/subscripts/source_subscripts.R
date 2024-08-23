# Source subscripts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. UC Anschutz Medical Campus, Dept. of Family Medicine

# Destription:
# Loads all of the subscripts/functions required to process the COMPASS data
# delivery for the PATHWEIGH Project

# Inputs:
# Requires a character vector of all subscripts the need to be loaded. All 
# subscripts must be saved in the scripts/subscripts directory

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Source subscripts ------------------------------------------------------------
# String vector of scripts to be loaded from the subscripts directory
# n.b. The subscripts vector should only contain functions, and not scripts that
# will process data. Each of these R files is actually a function to be able to
# apply to any data set, but started out as a hardcoded script.
subscripts <- c("prep_encounter.R",
                "prep_patient.R",
                "repeat_encounters.R",
                "wpv_cc.R",
                "wpv_icd.R",
                "wpv_flow.R",
                "wpv_vtype.R",
                "wpv_smart.R",
                "wpv_naweights.R",
                "set_index_date.R",
                "assign_last_visit_con.R",
                "set_last_visit.R",
                "censor_visits.R",
                "create_enrollment_table.R",
                "labs_procedures_sot.R",
                "create_safety_officer_table.R",
                # "proc_ee_ene.R",
                "any_pw_visit.R",
                "make_mod_data.R")

# Apply anonymous function to all inputs from subscripts character vector
walk(subscripts, ~ source(str_c(emr_dir, "subscripts/", .)))


# Source functions ------------------------------------------------------------
# String vector of scripts to be loaded from the functions directory.
functions <- c("clean_asterisks.R",
               "check_duplicates.R",
               "read_pw_rds.R",
               "read_pw_csv.R")

# Apply anonymous function to all inputs from subscripts character vector
walk(functions, ~ source(str_c(emr_dir, "functions/", .)))