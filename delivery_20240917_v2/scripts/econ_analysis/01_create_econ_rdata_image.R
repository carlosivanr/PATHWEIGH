#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Authors:
# Carlos Rodriguez Ph.D. CU Anschutz Dept. of Family Medicine
# 11/16/2023


# Description: _________________________________________________________________
# This script was drafted to import the compass data delivery tables needed to
# create the data for the economic analysis.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load Packages ----------------------------------------------------------------
pacman::p_load(here,         # For managing directory paths
               tidyverse,    # For data manipulation tools
               furrr,        # For parallel processing
               data.table,   # For reading .csv files
               openxlsx,     # For reading .xlsx files
               tictoc,       # For timing and benchmarking functions
               gtsummary,    # For tables,
               install = FALSE)  
               
library(magrittr, include.only = "%<>%")


# Specify parameters -----------------------------------------------------------
RData <- "20240917.RData"

# Set the path to the emr_data_processing directory to load scripts/functions
# that are shared along all separate data delivery dates
data_delivery_date <- str_remove(RData, ".RData")

# Specify delivery date
delivery <- data_delivery_date

# Specify file patterns to search for
# BillProc contains billing data from UC Health
# CU contains billing data from CU Medicine
patterns <- c("Table13_BillProc",
              "Table14_CU")

# Create a vector of paths to the specified data delivery directory that match
# the table vector for importing .csv files
files <- dir(here("data_raw", delivery), 
             pattern = paste(patterns, collapse = "|"))


# Parse the files paths to get the corresponding names to assign to the tables
# once imported 
df_names <- tolower(
  sub(".*_", "", 
      substring(substring(files, 1, nchar(files)-13), 7)
  )
)


# Set the file paths for importing COMPASS Tables
file_paths <- here("data_raw", delivery, files)


# Import COMPASS Tables --------------------------------------------------------
# Set furrr options
# plan(multisession, workers = 6) # timeout error if workers > 9 on hcp10
plan(sequential)
tables <- file_paths %>% 
  future_map(~ fread(., 
                     header = TRUE, 
                     integer64 = "numeric", 
                     na.strings = c("", "NA")))


# Set the names of the tables
tables <- setNames(tables, df_names)

# Place all compass table data frames into the global environment
invisible(
  list2env(tables, envir = .GlobalEnv)
)

# Clear out memory resources
rm(tables)

# Save image to data directory as delivery date .RData -------------------------
save.image(here(str_c("delivery_", delivery), "data", str_c("econ_", delivery, ".RData")))
invisible(gc())