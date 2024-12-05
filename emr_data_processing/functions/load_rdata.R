#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Medical Campus, Dept. of Family Medicine
# load RData function

# Loads an R data frame but allows it to be renamed to prevent over-writing
# data frames that are already loaded in the workspace.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
# File loading function that will allow loading a file and assign it to a
# different name in the global environment
load_rdata <- function(file_name) {
  #loads an RData file, and returns it
  load(file_name)
  get(ls()[ls() != "file_name"])
}