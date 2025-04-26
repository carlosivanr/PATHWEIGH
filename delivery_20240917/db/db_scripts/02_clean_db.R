# Clean tables ////////////////////////////////////////////////////////////////

# /////////////////////////////////////////////////////////////////////////////


# %% Load Packages -------------------------------------------------------------
library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(duckdb)
library(here)
library(glue)

# %% -----------------------------------------------------------
table_sql <- function(con, schema, table, variable) {
  query <- glue("
    SELECT 
      COALESCE({variable}, 'Missing') AS value,
      COUNT(*) AS n
    FROM {schema}.{table}
    GROUP BY COALESCE({variable}, 'Missing')
    ORDER BY n DESC
  ")

  dbGetQuery(con, query)
}


# %% Specify parameters -----------------------------------------------------------
# Set data delivery date as numerical
data_delivery_date <- 20240917

# Set the path to the emr_data_processing directory to load scripts/functions
# that are shared along all separate data deliveries
proj_root_dir <- str_c("delivery_", data_delivery_date)
proj_parent_dir <- str_remove(here(), proj_root_dir)
emr_dir <- str_c(proj_parent_dir, "/emr_data_processing/")

# Import the raw data files that will be minimally processed ------------------
con <- dbConnect(duckdb::duckdb(), dbdir = here(proj_root_dir, "db/pathweigh.duckdb"))

# Create "raw" schema if it doesn't exist
dbExecute(con, "CREATE SCHEMA IF NOT EXISTS clean;")




# %% Patient table ------------------------------------------------------------
dbExecute(con, "
  CREATE OR REPLACE TABLE clean.patient AS
  SELECT * FROM raw.patient;
")


## Sex ------------------------------------------------------------------------
## Count the variables including any missing values
table_sql(con, "clean", "patient", "Sex")

# Set the X values to "Unknown"
dbExecute(con, "
  UPDATE clean.patient
  SET Sex = 'Unknown'
  WHERE REGEXP_MATCHES(Sex, 'X');
")

## Ethnicity ------------------------------------------------------------------
## Count the variables including any missing values
table_sql(con, "clean", "patient", "Ethnicity")

# Set the "Unknown" "Unable" and "Unspecified" values as Null
dbExecute(con, "
  UPDATE clean.patient
  SET Ethnicity = NULL
  WHERE REGEXP_MATCHES(Ethnicity, 'Unknown|Unable|Unspecified');
")


# Set Race/Ethnicity Column -----------------------------------------------
# Create the Race_Ethnicity column first
dbExecute(con, "
  ALTER TABLE clean.patient ADD COLUMN Race_Ethnicity TEXT;
")

# Next, fill in the values of the Race_Ethnicity column based on Race and 
# Ethnicity
dbExecute(con, "
  UPDATE clean.patient
  SET Race_Ethnicity = 
    CASE 
      WHEN Ethnicity = 'Hispanic, Latino/a, or Spanish Origin' THEN 'Hispanic or Latino'
      WHEN Race IS NULL THEN 'Unknown'
      ELSE Race
    END;
")

# Recode 'White or Caucasian' to 'Non-Hispanic White'
dbExecute(con, "
  UPDATE clean.patient
  SET Race_Ethnicity = 'Non-Hispanic White'
  WHERE Race_Ethnicity = 'White or Caucasian';
")

# Collapse the "Other" race variables variables
dbExecute(con, "
  UPDATE clean.patient
  SET Race_Ethnicity = 'Other'
  WHERE Race_Ethnicity IN (
    'American Indian or Alaska Native',
    'Multiple Race',
    'Native Hawaiian and Other Pacific Islander',
    'Native Hawaiian',
    'Other Pacific Islander',
    'Guamanian or Chamorro',
    'Samoan'
  );
")

# Set the missing values to "unknown" or patient declined
dbExecute(con, "
  UPDATE clean.patient
  SET Race_Ethnicity = 'Unknown'
  WHERE Race_Ethnicity IS NULL 
     OR TRIM(Race_Ethnicity) = '' 
     OR Race_Ethnicity = 'Patient Declined';
")

table_sql(con, "clean", "patient", "Race_Ethnicity")


# /////////////////////////////////////////////////////////////////////////////
# %% Prep encounter  ----------------------------------------------------------
dbExecute(con, "
  CREATE OR REPLACE TABLE clean.encounter AS
  SELECT * FROM raw.encounter;
")

# Drop provider name
dbExecute(con, "
  ALTER TABLE clean.encounter
  DROP COLUMN ProviderName;
")

# Fill in missing value for height
# Count the number of missing values for height
dbGetQuery(con, "
  SELECT 
    SUM(CASE WHEN Height IS NULL THEN 1 ELSE 0 END) AS num_missing,
    SUM(CASE WHEN Height IS NOT NULL THEN 1 ELSE 0 END) AS num_present
  FROM clean.encounter;
")

# Create a new column Height_filled
dbExecute(con, "
  CREATE OR REPLACE TABLE clean.encounter AS
  SELECT 
    *,
    MAX(CASE WHEN Height IS NOT NULL THEN Height ELSE NULL END) OVER (
      PARTITION BY Arb_PersonId
      ORDER BY EncounterDate
      ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
    ) AS Height_filled
  FROM clean.encounter;
")

dbExecute(con, "ALTER TABLE clean.encounter DROP COLUMN Height;")
dbExecute(con, "ALTER TABLE clean.encounter RENAME COLUMN Height_filled TO Height;")



dbGetQuery(con, "
  SELECT 
    SUM(CASE WHEN Height_filled IS NULL THEN 1 ELSE 0 END) AS num_missing,
    SUM(CASE WHEN Height_filled IS NOT NULL THEN 1 ELSE 0 END) AS num_present
  FROM clean.encounter;
")