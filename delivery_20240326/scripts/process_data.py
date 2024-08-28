# %% IMPORT PACKAGES------------------------------------------------------------------
import polars as pl
import warnings
import pandas as pd
import numpy as np
import time

# %% Calculate elapsed time
# tic = time.time()

# %%  SET PARAMETERS -----------------------------------------------------------------
#  Parameters 
delivery = "20240326"
#print(delivery)

# Set data delivery root
data_root = "D:/PATHWEIGH/data_raw"

# Concatenate root and delivery
data_path = data_root + "/" + delivery
# print(data_path)


# %% ENCOUNTER -----------------------------------------------------------------------
# Set the path to the encounter table
tab_path = data_path + "/C2976_Table2_Encounter_" + delivery + ".csv"

 # read the encounter table
encounter = pl.read_csv(tab_path, null_values = ["*Unspecified", "-999", "*Restricted", ""])

# Remove duplicated encounters
encounter = (encounter
    .sort(["Arb_EncounterId", "Smoking_Status"])
    .group_by("Arb_EncounterId")
    .head(1)
)

# Define a value recode mapper/dict
mapper = {
    "CU - UA NET": "Commercial",
    "Managed Care": "Commercial", 
    "Special Accounts": "Commercial", 
    "Tricare": "Commercial", 
    "UCHEALTH EMPLOYEE PLAN": "Commercial", 
    "Worker's Comp": "Commercial",
    "Managed Medicaid": "Medicaid", 
    "Colorado Medicaid": "Medicaid", 
    "Out of State Medicaid": "Medicaid",
    "Managed Medicare": "Medicare", 
    "Medicare": "Medicare", 
    "Managed Care": "Medicare",
    "Indigent Care": "Self-Pay", 
}

# Replace values in financial class with values in mapper and save as insurance
encounter = encounter.with_columns(
    pl.col("FinancialClass")
    .replace(mapper)
    .alias("Insurance")
)

# Ensure that those with "Commercial" in FinancialClass are not set to None
encounter.filter(pl.col("FinancialClass") == 'Commercial').select(["FinancialClass", "Insurance"])

# Define a mapper to recode Smoking_status
mapper = {
    "Every Day": "Current",
    "Some Days": "Current",
    "Heavy Smoker": "Current",
    "Light Smoker": "Current",
    "Never": "Never", 
    "Passive Smoke Exposure - Never Smoker": "Never",
    "Smoker, Current Status Unknown": "Unknown", 
    "Unknown": "Unknown",
    "Never Assessed": "Unknown",
    "Former": "Former"
}

# Replace values in smoking status with mapper and save as smoking
encounter = encounter.with_columns(
    pl.col("Smoking_Status").
    replace(mapper)
    .alias("Smoking")
    )

# Convert string columns to numeric
cols_to_mod =["Systolic_blood_pressure", "Diastolic_blood_pressure", "HeartRate", "Respiratoryrate", "Temperature", "Weight","BMI"]
encounter = encounter.with_columns(
    pl.col(cols_to_mod)
    .cast(pl.Float32, strict=False)
    )

# Create height cm
encounter = encounter.with_columns(
    (pl.col("Height") * 2.54)
    .round(2)
    .alias("Height_cm")
    )

# Create Weight_kg
encounter = encounter.with_columns(
    (pl.col("Weight") * .0283495)
    .alias("Weight_kg"))

# Compute BMI
encounter = encounter.with_columns(
    (pl.col("BMI")
    .truediv(pl.col("Height_cm"))
    .truediv(pl.col("Height_cm")) * 10000)
    .alias("BMI_comp")
)

# Coalese BMI
encounter = encounter.with_columns(
    pl.coalesce(["BMI", "BMI_comp"])
    .alias("BMI_coa")
)

# Drop BMI, BMI_comp, Smoking_status and rename BMI_coa to BMI and Smoke to Smoking_Status
encounter = encounter.drop(["BMI", "BMI_comp", "Smoking_Status"])

# Possible way to clean up the code to rename variables
encounter = encounter.rename({"BMI_coa": "BMI",
                              "Smoking": "Smoking_Status"})

# *** Reasonable ranges for Heart Rate, Respiratory Rate, Blood pressure, temperature
# REASONABLE RANGES NEED TO BE MODIFIED, BUT PERHAPS PLACE AFTER FILTERING TO EE & ENE


# %% PATIENT -------------------------------------------------------------------------
# Set the path to the patient table
tab_path = data_path + "/C2976_Table1_Patient_" + delivery + ".csv"

 # read the patient table
patient = pl.read_csv(tab_path, null_values = ["*Unspecified", "-999", "*Restricted", "", "X"])

# Convert missing values to unknown
patient = patient.with_columns(
    pl.col("Sex")
    .replace({None: "Unknown"})
    )

# Check to see if there are multiple values in Sex to potentially use data from one row to fill another
# unique number of values per grouping variable
if (patient
    .select(["Arb_PersonId", "Sex"])
    .group_by("Arb_PersonId")
    .n_unique()
    .filter(pl.col("Sex") > 1)
    .shape
    )[0] != 0:
    warnings.warn("Values for sex can be filled. Consider revising code")

# Create a combined Race_Ethnicity column
# If the patient selected Hispanic, then set it to "Hispanic", otherwise set it
# to the value in the Race column
patient = patient.with_columns(
    pl.when(pl.col("Ethnicity") == "Hispanic, Latino/a, or Spanish Origin")
    .then(pl.lit("Hispanic"))
    .otherwise(pl.col("Race"))
    .alias("Race_Ethnicity")
    )

# If the new Race_Ethnicity value is in the supplied list, then set it to "Other", 
# otherwise set it to what ever value was in Race_Ethnicity
patient = patient.with_columns(
    pl.when(pl.col("Race_Ethnicity").is_in(["American Indian or Alaska Native", "Multiple Race",
                                            "Native Hawaiian and Other Pacific Islander", "Native Hawaiian",
                                            "Other Pacific Islander", "Guamanian or Chamorro", "Samoan"]))
    .then(pl.lit("Other"))
    .otherwise(pl.col("Race_Ethnicity"))
    .alias("Race_Ethnicity")
)

# If the new Race_Ethnicity value is in the supplied list, then set it to "Unknown"
patient = patient.with_columns(
    pl.col("Race_Ethnicity")
    .replace({"Patient Declined": "Unknown", 
               None: "Unknown"})
               )

# Test to ensure that the number of unique Race_Ethnicity values is 6, otherwise raise a warning
if (patient.select("Race_Ethnicity").unique("Race_Ethnicity").n_unique()) != 6:
    warnings.warn("The number of pre-defined race and ethnic categories is not correct. Consider reviewing code.")


# %% Prep Visits df
# Merge Encounter and Patient tables
visits = encounter.join(patient, on="Arb_PersonId", how="left")

# Merge in the GroupIDs for each clinic and modify Greeley IM
clinic = pl.read_csv("D:/PATHWEIGH/working_files/ClinicEpicID_by_group.csv")
visits = visits.join(clinic, on=["DepartmentEpicId", "DepartmentExternalName"], how="left")

# Greeley IM had two separate EpicIds, due to moving locations
visits = visits.with_columns(pl.col("DepartmentEpicId").replace({10981033: 10981004}))

# Move Greeley IM to Group 1, with the other DeptEpicIds
visits = visits.with_columns(
  pl.when(pl.col("DepartmentExternalName") == "UCHealth Internal Medicine Clinic - Greeley")
  .then(pl.lit(1))
  .otherwise(pl.col("GroupID"))
  .alias("GroupID")
  )

# Convert EncounterDate to date format
# visits = visits.with_columns(pl.col("EncounterDate").str.strptime(pl.Date, "%Y-%m-%d"))

# Set Intervention variable
visits = visits.with_columns(
    Intervention = pl.when(
        ((pl.col("EncounterDate") < "2023-03-17") & (pl.col("GroupID")==3)) | 
        ((pl.col("EncounterDate") < "2022-03-17") & (pl.col("GroupID")==2)) |
        ((pl.col("EncounterDate") < "2021-03-17") & (pl.col("GroupID")==1))
    )
    .then(pl.lit("Control"))
    .otherwise(pl.lit("Intervention"))
    )

# Cast EncounterDate and Birthdate as date
visits = visits.with_columns(
    pl.col(["EncounterDate", "BirthDate"])
    .str.to_datetime("%Y-%m-%d")
    )

# Create Age
# First create a pandas data frame to compute age at the time of encounter in year
# because figuring out how to implement in Polars was taking too long
age_enc_pd = visits.select(["Arb_EncounterId", "EncounterDate", "BirthDate"]).to_pandas()

# Calculate age at the time of encounter
age_enc_pd["Age_at_enc"] = (((age_enc_pd["EncounterDate"] - age_enc_pd["BirthDate"])/365.25).dt.days)

# Create a polars df of the age at encounter
age_enc_pl = pl.from_pandas(age_enc_pd[["Arb_EncounterId", "Age_at_enc"]])

# Merge age on Arb_EncounterId
visits = visits.join(age_enc_pl, on="Arb_EncounterId", how="left")

# Create IndexVisitEligible based on age at encounter, BMI, Provider NPI, and Weight values
visits = visits.with_columns(
    IndexVisitEligible = pl.when(
        ((pl.col("Age_at_enc") >= 18) & 
        (pl.col("BMI")>=25) & 
        (pl.col("ProviderNpi").is_not_null()) & 
        (pl.col("Weight_kg").is_not_null()))
    )
    .then(1)
    .otherwise(0)
    )


# %% Make WPV variables
# WPV CC
# Get the Arb_EncounterIds that contain the keywords
# keywords are ["UCH AMB WEIGHT CHECK", "WEIGHT CHANGE", "WEIGHT CONSULT", "OBESITY", "WEIGHT MANAGEMENT", "WEIGHT PROBLEM", "WEIGHT GAIN" ]
# but values must be modified because of inadequate weight gain, and weight loss consult.
wpv_cc_ids = (
visits
    .select("Arb_EncounterId", "EncounterChiefComplaints")
    .filter(pl.col("EncounterChiefComplaints").is_not_null()).
    unique()
    # .filter(pl.col("Arb_EncounterId") == 183835364536)
    .with_columns(pl.col("EncounterChiefComplaints").str.split(";").alias("Complaints"))
    .explode("Complaints")
    .select("Arb_EncounterId", "Complaints")
    .with_columns(pl.col("Complaints").str.strip_chars_start().replace({"WEIGHT LOSS CONSULT": "WEIGHT CONSULT"}))    
    .filter(pl.col("Complaints").str.contains("WEIGHT|OBES"))
    .filter(pl.col("Complaints").str.contains("WEIGHT LOSS|INADEQUATE").not_())
    .select("Arb_EncounterId")
)

# Create a data frame to merge in the binary values
wpv_cc_ids = wpv_cc_ids.with_columns(
    WPV_CC = 1
)

# Merge in the values
visits = visits.join(wpv_cc_ids, on="Arb_EncounterId", how="left")

# Replace null values with 0
visits = visits.with_columns(
    pl.col("WPV_CC")
    .replace({None: 0})
    )


# %% WPV ICD ---
tab_path = data_path + "/C2976_Table6_DX_" + delivery + ".csv"

 # read the patient table
dx = pl.read_csv(tab_path, null_values = ["*Unspecified", "-999", "*Restricted", "", "X"])

# Create a list of E66 Codes as a series in pandas
# using pandas was more elegant and succinct than polars
e_suffix = pd.Series(["01", "09", "1", "2", "3", "8", "9"])
e66_codes = pd.Series(["E66."]*e_suffix.size).str.cat(e_suffix)

# Create a list of Z68 Codes as a series
z_suffix = pd.Series(range(25, 45), dtype="string")
z68_codes = pd.Series(["Z68."]*z_suffix.size).str.cat(z_suffix)

# Combine the two series then combine them with | as a separater
all_codes = pd.concat([e66_codes, z68_codes], axis = 0).str.cat(sep = "|")

# Capture all of the encounter ids where the diagnosis code contains one of the
# values in the concatenated codes
wpv_icd_ids = (dx
    .select("Arb_EncounterId", "DiagnosisCode")
    .filter(pl.col("DiagnosisCode").str.contains(all_codes))
    .select("Arb_EncounterId")
    .unique()
)

# Create a data frame to merge in the binary values
wpv_icd_ids = wpv_icd_ids.with_columns(
    WPV_ICD = 1
)

# Merge in the values
visits = visits.join(wpv_icd_ids, on="Arb_EncounterId", how="left")

# Replace null values with 0
visits = visits.with_columns(
    pl.col("WPV_ICD")
    .replace({None: 0}))
# *** Consider converting all None to 0 at the end after all WPV columns have been created to stay DRY


# %%
# WPV Flowsheets
# Flowsheets represent identifiers to either entire questionnaires or individual questions from questionnaires
# WPV identified from the questions in the PW WMQ as pw_flow or the OBHPI. WPVs also identified by the
# entire WMQ questionnaire as PW WMQ. The general approach is filter the flowsheets table to identify
# encounters that match one of the FlowSheetEpicIds or TemplateEpicIds, pull the EncounterIds, and set 
# aside. Then in visits df, set each WPV column if the EncounterId is in its respective list of encounters
# pulled from the flowsheets table.

# Set the path to the patient table
tab_path = data_path + "/C2976_Table8_Flowsheet_" + delivery + ".csv"

 # read the patient table
flowsheets = pl.read_csv(tab_path, null_values = ["*Unspecified", "-999", "*Restricted", "", "X"])

# Load the csv file with the flowsheet ids
flowsheet_ids = pl.read_csv("D:/PATHWEIGH/working_files/FlowsheetIDs_Obesity_Brief_HPI_PW.csv")

# Prep the flowhsheet_ids dataframe
# Define WPV_WMQ WPV_OBHPI WPV_PW_flow
# Get the flowsheet ids associated with PW flow
pw_flow_ids = pl.Series(
    flowsheet_ids
    .filter(pl.col("PATHWEIGH") == "X")
    .select("Flowsheet_RowID")
    ).to_list()

# Get the encounter ids in the flowsheets table that satisfy the pw_flow_ids
pw_flow_encs = pl.Series(
    flowsheets
    .filter(
    pl.col("FlowsheetRowEpicId")
    .is_in(pw_flow_ids)
    ).select("Arb_EncounterId")
    ).to_list()


# If the Encounter id is in the list, set to 1 otherwise 0
vists = visits.with_columns(
    pl.when(pl.col("Arb_EncounterId").is_in(pw_flow_encs))
    .then(pl.lit(1))
    .otherwise(0)
    .alias("WPV_PW_flow")
)

wmq_encs = pl.Series(
flowsheets.filter(
    pl.col("FlowsheetTemplateEpicId")
    .is_in([2108002828, 21080028316])
    ).select("Arb_EncounterId")
).to_list()

# If the Encounter id is in the list, set to 1 otherwise 0
visits = visits.with_columns(
    pl.when(pl.col("Arb_EncounterId").is_in(wmq_encs))
    .then(pl.lit(1))
    .otherwise(0)
    .alias("WPV_WMQ")
)

# Get the flowsheet ids associated OBHPI
obhpi_ids = pl.Series(
    flowsheet_ids
    .filter(pl.col("obesity_brief_HPI") == "X")
    .select("Flowsheet_RowID")
).to_list()

# Get the encounter ids associated with OBHPI
obhpi_encs = pl.Series(
    flowsheets
    .filter(
    pl.col("FlowsheetRowEpicId")
    .is_in(obhpi_ids)
    ).select("Arb_EncounterId")
    ).to_list()

# If the Encounter id is in the list, set to 1 otherwise 0
vists = visits.with_columns(
    pl.when(pl.col("Arb_EncounterId").is_in(obhpi_encs))
    .then(pl.lit(1))
    .otherwise(0)
    .alias("WPV_OBHPI")
)

# %% WPV Visit Type


# %%
# WPV Smart
# WPV na weight
# WPV Row Sums


# %% Calculate elapsed time
# toc = time.time()
# print(toc-tic, 'Sec Elapsed')
