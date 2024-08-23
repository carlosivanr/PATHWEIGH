#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Medical Campus, Dept. of Family Medicine
# convert_dates

# Input is a data frame, and out puts the same data frame with any column that
# has "Date" in the column name returned as as.Date().
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_dates <- function(df_name) {
  # Get the data frame from the global environment
  df <- get(df_name, envir = .GlobalEnv)
  
  if ("DateSource" %in% names(df) == TRUE){
  df <- df %>%
    select(-DateSource)
  }
  
  # convert columns with "Date" in the name to as.Date
  df <- df %>% 
    mutate(across(contains("Date"), as.Date))
  
  # Save the data frames containing summary statistics back to the global 
  # environment
  assign(df_name, df, envir = .GlobalEnv)
}