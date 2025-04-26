# Create SQL Database /////////////////////////////////////////////////////////

# /////////////////////////////////////////////////////////////////////////////

# %% Load Packages -------------------------------------------------------------
library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(duckdb)
library(here)

# Specify parameters -----------------------------------------------------------
# Set data delivery date as numerical
data_delivery_date <- 20240917

# Set the path to the emr_data_processing directory to load scripts/functions
# that are shared along all separate data deliveries
proj_root_dir <- str_c("delivery_", data_delivery_date)
proj_parent_dir <- str_remove(here(), proj_root_dir)
emr_dir <- str_c(proj_parent_dir, "/emr_data_processing/")

# Import the raw data files that will be minimally processed ------------------
con <- dbConnect(duckdb::duckdb(), dbdir = here(proj_root_dir, "db/pathweigh.duckdb"))


# Create a file map -----------------------------------------------------------
# This is a map/dictionary that assigns a name to be used in R to a .csv file
# that was delivered from COMPASS

# List all of the .csv files in the current delivery's data directory
file_map <- data.frame(file_name = 
  list.files(pattern = ".csv",
             path = str_c("D:/PATHWEIGH/data_raw/", 
             data_delivery_date))
            ) %>%
  as_tibble()

# Prepare the data frame names to assign to the .csv files
# Strip the last 13 characters from the end (delivery date)
# Then strip the first 6 character (C2976 prefix)
# Then substitue everything up to the underscore with ""
# Some extra files will be kept
file_map <- file_map %>%
  mutate(
    df_name = tolower(sub(".*_", "", substring(substring(file_name, 1, nchar(file_name)-13), 7))),
    df_name = if_else(df_name == "lab", "labs", df_name),
    df_name = if_else(df_name == "med", "meds", df_name),
    df_name = if_else(df_name == "referral", "referrals", df_name),
    df_name = if_else(df_name == "flowsheet", "flowsheets", df_name),
    df_name = if_else(df_name == "samrtdata", "smart", df_name),
    df_name = if_else(df_name == "dxcomo", "dxco", df_name)) %>%
  filter(df_name != 2)


# Create "raw" schema if it doesn't exist
dbExecute(con, "CREATE SCHEMA IF NOT EXISTS raw;")

# For-loop to import all of the COMPASS data delivery tables
for (i in 1:dim(file_map)[1]) {
  csv_file = file_map$file_name[i]
  table_name = file_map$df_name[i]
  
  message("Processing: ", csv_file)

  tryCatch({
    # Common processing steps across all compass data delivery files
    temp <- data.table::fread(
      file = str_c(proj_parent_dir,"/data_raw/",data_delivery_date, "/", csv_file),
      header = TRUE,
      integer64 = "numeric",
      na.strings = c("", "NA", -999, "*Restricted")) %>%
      mutate_if(lubridate::is.Date, as.Date) %>%
      as_tibble()

    # Write the dataframe to the DuckDB database
    dbWriteTable(con, DBI::Id(schema = "raw", table = table_name), temp, overwrite = TRUE)
  
    }, error = function(e) {
    warning("Failed to process: ", csv_file, "\n", e$message)
  })
}

# Shutdown the database connection
dbDisconnect(con, shutdown = TRUE)


# Show the schemas
dbGetQuery(con, "SELECT schema_name FROM information_schema.schemata;")

# Show the table names in the raw schema
dbGetQuery(con, "
  SELECT table_name 
  FROM information_schema.tables 
  WHERE table_schema = 'raw';
")

# Show the column names in the patient table
dbGetQuery(con, "PRAGMA table_info('raw.patient');")