# Save datasets -----------------------------------------------------------
if (save_data_sets == TRUE){
  
  path <- str_c(wdir, "/data/", str)
  write.csv(seq_df2, str_c(path,"_long.csv"), row.names=FALSE)
  write.csv(seq_df2.id, str_c(path,"_idIndex.csv"), row.names=FALSE)
  write.csv(seq_df2.last, str_c(path,"_idLast.csv"), row.names=FALSE)
  write.csv(meds_AOM, str_c(path,"_AOM.csv"), row.names=FALSE)
  tic()
  save(visits, file = str_c(path, "_full.Rda"))
  toc()
  # Additional requests -----------------------------------------------------
  #IDs for mortality and hospitalization data (sent to Kelli on Feb 2, 2021)
  # *** this does not function correctly in the function, when save is set to true
  #ids.mortality <- data.frame("Arb_PersonId"=unique(visits_on_id$Arb_PersonId))
  #write.csv(ids.mortality, str_c(wdir,"/RProjects/baseilne_char/",str,"_mortalityIDs_forKelli.csv"), row.names=FALSE)
  
  #Save Provider IDs for Leigh (to get DOB and FTE)
  # providers_WPV <- seq_df2.id[,c("ProviderName","ProviderNpi")]
  # providers_WPV <- providers_WPV[!duplicated(providers_WPV), ]
  # providers_WPV <- subset(providers_WPV, !is.na(providers_WPV$ProviderNpi))
  #
  # providers_fromLeigh <- read.csv(str_c(wdir,"/Working_Files/BaselinePaper/providerdemographics.csv"))
  # providers_forLeigh <- merge(providers_WPV, providers_fromLeigh, all.x=TRUE)
  # providers_forLeigh <- providers_forLeigh[!duplicated(providers_forLeigh), ]
  
  # write.csv(providers_forLeigh,
  #                     file=str_c(wdir,"/Working_Files/BaselinePaper/Providers.csv"),
  #                     row.names=FALSE)
  
  # write the comorbidities table
  # *** Output to .RData and then create table with RMD
  today.date <- Sys.Date()
  # write.csv(dx_sub_coi_count,
  #           file = str_c(wdir,
  #                         "/tables/comorbidities_of_interest_220121.RData",
  #                         today.date, ".csv"),
  #           quote = FALSE,
  #           row.names = FALSE)
  
  write.csv(top_comorbidities,
            file = str_c(wdir,
                         "/working_files/top_comorbidities_220121.csv"),
            quote = FALSE,
            row.names = FALSE)
  
  # save exclusions file, excluded due to out of range vitals
  exl_fout <- substr(str, 1, nchar(str)-4)
  write.csv(out_of_range_exclusions,
            file = str_c(wdir, "/tables/", exl_fout, "vitals_exclusions.csv"),
            quote = FALSE, row.names = FALSE)
}
