# Clean asterisks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Description:
# Some values in the data begin with asterisks. Some examples include, *Not 
# applicable, *Restricted, and *Deleted. This function will go through 
# character columns and convert values that begins with an asterisk to NA.

# Dependencies:
# magrittr
# dplyr

# Usage:
# data <- clean_asterisks(data)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This function will replace any values that start with an asterisk with NA
clean_asterisks <- function(data){
  
  data %<>% 
    mutate_if(is.character, ~ str_replace(., "^\\*", replacement = NA_character_))
  
}
