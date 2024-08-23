# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Medical Campus, Dept. of Family Medicine
# count_rows()

# Input is the name of the .csv file from a compass data delivery. Outputs a 
# numeric value indicating the number of rows given by a system command. This 
# function was created to ensure that the number of rows imported in to R can
# be cross referenced with the number of rows with a different counting 
# approach. The motivation for this function is to ensure all rows are properly 
# imported and do not surpass R RAM memory limitations.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

count_rows_sys <- function(file){
  # wsl version:
  # set path to the pathweigh data directory
  data_dir <- "FM/PATHWEIGH/PATHWEIGH_DATA_SECURE/"
  
  # concatenate the input command to system2()
  command <- str_c("cd ~; sudo mount -t drvfs S: /mnt/s; cd /mnt/s/",
                   data_dir,
                   delivery,
                   "; wc -l ",
                   file)
  
  # wc -l counts headers, therefore nrows is given by the value of wc -l - 1
  value <- as.integer(sub("\\ ..*", "",
                          system2("wsl", command, stdout = TRUE))) - 1
  
  # powershell version:
  # command <- str_c("Import-Csv ", 
  #                  here("../../..", "PATHWEIGH_DATA_SECURE", delivery, file), 
  #                  " | Measure-Object")
  # 
  # value <- system2("powershell", args = command, stdout = TRUE)
  # 
  # value <- as.data.frame(value) %>%
  #   filter(grepl("Count", value)) %>%
  #   pull(value) %>%
  #   sub(".*:", "", .) %>%
  #   str_trim() %>%
  #   as.numeric()
  return(value)
}
