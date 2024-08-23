# Declare a function to read the pathweigh rds files into the global environment
read_pw_rds <- function(x){
  assign(x, 
         readRDS(here("data", str_c(x,".rds"))),
         envir = .GlobalEnv)
}