#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Medical Campus, Dept. of Family Medicine
# check_duplicates

# Input is a data frame, and out puts a message of whether or not duplicates 
# were detected.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_duplicates <- function(data){
  if (sum(duplicated(data)) > 1){
    message(paste0(sum(duplicated(data)), " duplicate rows found!"))
  } else {
    message("No duplicate rows found!")
    }
  }