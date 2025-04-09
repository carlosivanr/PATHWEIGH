README

FILE DESCRIPTIONS:



# -----------------------------------------------------------------------------
comorbidity_names_2024917.RDS - ???


# Economic Analysis files -----------------------------------------------------
econ_20240917 - Data for Mark's economic analysis


# Output from the main processing pipeline ------------------------------------
all_visits_2024091.RData - These represent all of the visits processed up to
	the point of capturing the ee and ene patients. These could be used to 
	capture visits before a patients index date, or to capture visits from
	those that were not eligible AND not enrolled.

ee_ene_20240917 - These are data that contain all of the visits after the index
	date for patients that were eligible and enrolled and the patients that 
	were eligible but not enrolled

mod_data_ee_20240917 - These are data where the patients have at least one
	followup in w phase. These are just for the ee patients. These data do not
	have the index visit because, the weight from the index visit is used as
	the baseline value.

mod_data_full_20240917 - These are data were there are two lists, one for ee
	and one for ene, same as previous.

mod_data_w_ind_20240917 - Same as mod_data_ee_20240917 except the index is
	included

sot_20240917 - A specific version of data for the safety officer table

visits_post_id_20240917 - These are data for all the EE patients regardless of
	whether or not they have a followup visit. All visits after the index date.

processed_ee_ene_20240917 - These data are the data processed for identifying
	labs, procedures, medications, EOSS, and comorbidities. If the pipeline
	undergoes a modification, and this modification does not influence the 
	index visits, then this file can be recycled to pull in the labs procedures
	and other processed data. Processing this step of the pipeline takes the
	longest, close to an hour running in parallel with 4 cores. Rather than
	processing the data each time theres a change to the pipeline, this step
	will save the output on the first run of the latest delivery. If/when the
	pipeline needs to be modified, then this allows the pipeline to load 
	processed data and then merge in. The intention is to avoid spending the
	time to re-process the data. 

new_col_names_20240917 - As the data are processed new column names are created.
	Some column-selecting functions are design to use the new columns names to 
	exclude/drop those columns from other procedures such as the safety officer
	table.


# Aim 1A ----------------------------------------------------------------------
- pp_data_20240917.Rdata: These are data for the 9,358 patients in the Aim1A
	analyses and includes all encounters

- pp_mod_data_20240917.Rdata: These are data for the 9,358 patients in the Aim1A
	analyses, but formatted for statistical analyses.


# Aim 1B ----------------------------------------------------------------------
data_aim1b.csv - A .csv file use to give to Qing to conduct the imputation.
	These are a subset of columns from the pp_data_20240917.RData file and
	include only the patient id, intervention phase, months after index date,
	and the Weight in kilograms

quantiles_aim1b.csv - A file showing the quantiles of the N_months_post_id
	variable. These quantiles are used in the imputation section.

imputation_control.csv - A .csv file with the imputed values from the control
	phase of the intervention. Imputed values are from 6, 12, and 18 months
	after the index date.

imputation_intervention - same as above except for the intervention phase


# Quality Assurance -----------------------------------------------------------
Files used to conduct QA explorations
processed_ee_ene_20240917_2024-03-26 and processed_ee_ene_20240917_2024-09-17
	These data were used to see why the number of patients identified from aim
	1 a dropped. There was a loss of patients, and at the same time a gain of
	new patients. Part of the loss of patients was to the implementation of
	a cut off date for enrollment. It was initially not specific enough and was
	filtering out subjects who should not have. In addition, there was an issue
	where some previously captured patients were not in the data. This issue
	was brought up to COMPASS that turned out to be a coding error in part of
	their data processing updates upstream from Krista Miller.


# Aim 2 -----------------------------------------------------------------------
aim1a_provider_npis.csv - All of the NPIs associated with the encounters analyzed
	in the Aim 1A paper. The sample is 9,358 patients, but 844 NPIs. These were
	used to give to Jake to merge with additional features from the NPPES data.
	The data are for the providers in the analyzed data set from Aim 1a, but the
	data will be for the aim 2 analyses. The naming describes the data not the
	purpose

aim2_data_20240917.csv - These are the data for the analyses of Aim 2 where the
	unique providers have had their FTE calculated, determined primary specialty,
	NPPES features, clinic level variables such as volume and % medicaid, and
	patient level characteristics as well