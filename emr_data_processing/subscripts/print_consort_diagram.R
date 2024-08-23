# Print Consort Diagram ########################################################
#sink(str_c(wdir,"/reports/CONSORT_",str,".txt"), append=FALSE)

print(str_c("Practices Eligible and Randomized: ", length(unique(visits_on_id$DepartmentEpicId))))
print(str_c("Cohort 1: ", length(unique(visits_on_id$DepartmentEpicId[visits_on_id$Cohort == "Cohort1"]))))
print(str_c("Cohort 2: ", length(unique(visits_on_id$DepartmentEpicId[visits_on_id$Cohort == "Cohort2"]))))
print(str_c("Cohort 3: ", length(unique(visits_on_id$DepartmentEpicId[visits_on_id$Cohort == "Cohort3"]))))

sub_inrange <- subset(visits, visits$EncounterDate >= date_min & visits$EncounterDate <= date_max)

print(str_c("Patients with an eligible visit type during the baseline period (", date_min, "–",  date_max, "): ",
            length(unique(sub_inrange$Arb_PersonId))))
print(str_c("Encounters: ", length(sub_inrange$Arb_PersonId)))
print(str_c("Cohort 1: ", length(unique(sub_inrange$Arb_PersonId[sub_inrange$Cohort == "Cohort1"]))))
print(str_c("Encounters: ", length(sub_inrange$Arb_PersonId[sub_inrange$Cohort == "Cohort1"])))
print(str_c("Cohort 2: ", length(unique(sub_inrange$Arb_PersonId[sub_inrange$Cohort == "Cohort2"]))))
print(str_c("Encounters: ", length(sub_inrange$Arb_PersonId[sub_inrange$Cohort == "Cohort2"])))
print(str_c("Cohort 3: ", length(unique(sub_inrange$Arb_PersonId[sub_inrange$Cohort == "Cohort3"]))))
print(str_c("Encounters: ", length(sub_inrange$Arb_PersonId[sub_inrange$Cohort == "Cohort3"])))

sub_naBMI <- subset(visits, !is.na(visits$BMI_use))

print(str_c("Patients with available BMI information: ", length(unique(sub_naBMI$Arb_PersonId))))
print(str_c("Encounters: ", length(sub_naBMI$Arb_PersonId)))
print(str_c("Cohort 1: ", length(unique(sub_naBMI$Arb_PersonId[sub_naBMI$Cohort=="Cohort1"]))))
print(str_c("Encounters: ", length(sub_naBMI$Arb_PersonId[sub_naBMI$Cohort=="Cohort1"])))
print(str_c("Cohort 2: ", length(unique(sub_naBMI$Arb_PersonId[sub_naBMI$Cohort=="Cohort2"]))))
print(str_c("Encounters: ", length(sub_naBMI$Arb_PersonId[sub_naBMI$Cohort=="Cohort2"])))
print(str_c("Cohort 3: ", length(unique(sub_naBMI$Arb_PersonId[sub_naBMI$Cohort=="Cohort3"]))))
print(str_c("Encounters: ", length(sub_naBMI$Arb_PersonId[sub_naBMI$Cohort=="Cohort3"])))

sub_Incl <- subset(visits, visits$Age>=18&visits$BMI_use>=25)

# These are patients that meet age and BMI inclusion criteria, relies on visits data frame
print(str_c("Patients that meet inclusion criteria (Age ≥18 years, BMI ≥ 25 kg/m2): ",
            length(unique(sub_Incl$Arb_PersonId))))
print(str_c("Encounters: ", length(sub_Incl$Arb_PersonId)))
print(str_c("Cohort 1: ", length(unique(sub_Incl$Arb_PersonId[sub_Incl$Cohort=="Cohort1"]))))
print(str_c("Encounters: ", length(sub_Incl$Arb_PersonId[sub_Incl$Cohort=="Cohort1"])))
print(str_c("Cohort 2: ", length(unique(sub_Incl$Arb_PersonId[sub_Incl$Cohort=="Cohort2"]))))
print(str_c("Encounters: ", length(sub_Incl$Arb_PersonId[sub_Incl$Cohort=="Cohort2"])))
print(str_c("Cohort 3: ", length(unique(sub_Incl$Arb_PersonId[sub_Incl$Cohort=="Cohort3"]))))
print(str_c("Encounters: ", length(sub_Incl$Arb_PersonId[sub_Incl$Cohort=="Cohort3"])))

# These are patients who meet age, BMI, and WPV criteria, uses visits_post_id data frame
print(str_c("Patients with a weight-prioritized visit: ",
            length(unique(visits_post_id$Arb_PersonId))))
print(str_c("Encounters: ", length(visits_post_id$Arb_PersonId)))
print(str_c("Cohort 1: ", length(unique(visits_post_id$Arb_PersonId[visits_post_id$Cohort=="Cohort1"]))))
print(str_c("Encounters: ", length(visits_post_id$Arb_PersonId[visits_post_id$Cohort=="Cohort1"])))
print(str_c("Cohort 2: ", length(unique(visits_post_id$Arb_PersonId[visits_post_id$Cohort=="Cohort2"]))))
print(str_c("Encounters: ", length(visits_post_id$Arb_PersonId[visits_post_id$Cohort=="Cohort2"])))
print(str_c("Cohort 3: ", length(unique(visits_post_id$Arb_PersonId[visits_post_id$Cohort=="Cohort3"]))))
print(str_c("Encounters: ", length(visits_post_id$Arb_PersonId[visits_post_id$Cohort=="Cohort3"])))

#sink()