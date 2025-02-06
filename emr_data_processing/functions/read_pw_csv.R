# Declare a function to read the pathweigh csv files into the global environment
file_map <-
  data.frame(file_name = dir(str_c("D:/PATHWEIGH/data_raw/",
                                   data_delivery_date))) %>%
  as_tibble()

file_map <-
  file_map %>%
  mutate(df_name = tolower(
                           sub(".*_", "", substring(substring(file_name, 1, nchar(file_name)-13), 7))),
         df_name = if_else(df_name == "lab", "labs", df_name),
         df_name = if_else(df_name == "med", "meds", df_name),
         df_name = if_else(df_name == "referral", "referrals", df_name),
         df_name = if_else(df_name == "flowsheet", "flowsheets", df_name),
         df_name = if_else(df_name == "samrtdata", "smart", df_name),
         df_name = if_else(df_name == "dxcomo", "dxco", df_name))

read_pw_csv <- function(x) {
  #' Read a pathweigh compass .csv file
  #'
  #' @param x a compass .csv file one of the following "labs", "meds",
  #' "referrals", "flowsheets", "smart", "dxco"
  #' read_pw_csv("labs")

  csv_file <-
    file_map %>%
    filter(df_name == x) %>%
    select(file_name) %>%
    pull()

  temp <-
    data.table::fread(
                      file = str_c(proj_parent_dir,
                                   "/data_raw/",
                                   data_delivery_date,
                                   "/", csv_file),
                      header = TRUE,
                      integer64 = "numeric",
                      na.strings = c("", "NA", -999, "*Restricted")) %>%
                      # na.strings = c("", "NA", -999)) %>%
    mutate_if(lubridate::is.Date, as.Date) %>%
    as_tibble()

  assign(x, temp, envir = .GlobalEnv)
  
  rm(temp)
}
