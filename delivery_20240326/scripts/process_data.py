# %% IMPORT PACKAGES------------------------------------------------------------------
import polars as pl
import warnings
# from datetime import date
import pandas as pd
import time

tic = time.time()

# %%  SET PARAMETERS -----------------------------------------------------------------
#  Parameters 
delivery = "20240326"
#print(delivery)

# Set data delivery root
data_root = "D:/PATHWEIGH/data_raw"

# Concatenate root and delivery
data_path = data_root + "/" + delivery
# print(data_path)

# Define a function
# def function_name(df):
#     return df
# function_name(df)

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

# Replace values in financial class with mapper and save as insurance
encounter = encounter.with_columns(
    pl.col("FinancialClass").replace(mapper).alias("Insurance")
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
    pl.col("Smoking_Status").replace(mapper).alias("Smoking")
)

# Convert string columns to numeric
cols_to_mod =["Systolic_blood_pressure", "Diastolic_blood_pressure", "HeartRate", "Respiratoryrate", "Temperature", "Weight","BMI"]
encounter = encounter.with_columns(pl.col(cols_to_mod).cast(pl.Float32, strict=False))

# Create height cm
encounter = encounter.with_columns((pl.col("Height") * 2.54).round(2).alias("Height_cm"))

# Create Weight_kg
encounter = encounter.with_columns((pl.col("Weight") * .0283495).alias("Weight_kg"))

# Compute BMI
encounter = encounter.with_columns(
    (pl.col("BMI").truediv(pl.col("Height_cm")).truediv(pl.col("Height_cm")) * 10000).alias("BMI_comp")
)

# Coalese BMI
encounter = encounter.with_columns(
    pl.coalesce(["BMI", "BMI_comp"]).alias("BMI_coa")
)

# Drop BMI, BMI_comp, Smoking_status and rename BMI_coa to BMI and Smoke to Smoking_Status
encounter = encounter.drop(["BMI", "BMI_comp", "Smoking_Status"])

# Possible way to clean up the code to rename variables
encounter = encounter.rename({"BMI_coa": "BMI",
                  "Smoking": "Smoking_Status"})

# *** Reasonable ranges for Heart Rate, Respiratory Rate, Blood pressure, temperature

# %% PATIENT -------------------------------------------------------------------------
# Set the path to the patient table
tab_path = data_path + "/C2976_Table1_Patient_" + delivery + ".csv"

 # read the patient table
patient = pl.read_csv(tab_path, null_values = ["*Unspecified", "-999", "*Restricted", "", "X"])

# Convert missing values to unknown
patient = patient.with_columns(
    pl.col("Sex").replace({None: "Unknown"})
)

# Check to see if there are multiple values in Sex to potentially use data from one row to fill another
# unique number of value per grouping variable
# patient.group_by("Arb_PersonId").agg(pl.struct("Sex")).n_unique()
if (patient
    .select(["Arb_PersonId", "Sex"])
    .group_by("Arb_PersonId")
    .n_unique()
    .filter(pl.col("Sex") > 1)
    .shape
    # .select(pl.len())
    # .is_empty()
)[0] != 0:
    warnings.warn("Values for sex can be filled. Consider revising code")

# Create a combined Race_Ethnicity column
# If the patient selected Hispanic, then set it to "Hispanic", otherwise set it
# to the race values
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
    pl.col("Race_Ethnicity").replace({"Patient Declined": "Unknown", None: "Unknown"})
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
    pl.col(["EncounterDate", "BirthDate"]).str.to_datetime("%Y-%m-%d"))

# Create Age
# First create a pandas data frame, compute age at the time of encounter in year
age_enc_pd = visits.select(["Arb_EncounterId", "EncounterDate", "BirthDate"]).to_pandas()

# Computer age
age_enc_pd["Age_at_enc"] = (((age_enc_pd["EncounterDate"] - age_enc_pd["BirthDate"])/365.25).dt.days)

# Create a polars df of the age at encounter
age_enc_pl = pl.from_pandas(age_enc_pd[["Arb_EncounterId", "Age_at_enc"]])

# Merge age on Arb_EncounterId
visits = visits.join(age_enc_pl, on="Arb_EncounterId", how="left")

# Create IndexVisitEligible
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
df = pl.DataFrame({"EncounterChiefComplaints": ["ANNUAL EXAM; ORDERS", "HIP PAIN; ANNUAL EXAM", "ANNUAL EXAM; MEDICATION MANAGEMENT"]})

desired = pl.DataFrame({"EncounterChiefComplaints": ["ANNUAL EXAM", "ORDERS", "HIP PAIN", "ANNUAL EXAM", "ANNUAL EXAM", "MEDICATION MANAGEMENT"]})

df_pd = df.to_pandas()

str_split = (
df.with_columns(
    pl.col("EncounterChiefComplaints").str.split(";").alias("str_split"))
    .explode("str_split")
    .select("str_split")
    .unique() 
)


str_split.with_columns(
    match = pl.col("str_split").str.contains("EXAM|ORDERS")
)

(
visits
    .select("Arb_EncounterId", "EncounterChiefComplaints")
    .filter(pl.col("EncounterChiefComplaints").is_not_null())
    # .filter(pl.col("Arb_EncounterId") == 183835364536)
    .with_columns(pl.col("EncounterChiefComplaints").str.split(";").alias("Complaints"))
    .explode("Complaints")
    .select("Arb_EncounterId", "Complaints")
    .unique()
    # .filter(pl.col("Complaints").str.contains("WEIGHT|OBES"))
    .filter(pl.col("Complaints").str.contains("WEIGHT LOSS"))
    .select("Complaints")
    .unique()
)

# WPV ICD
# WPV Flowsheets
# WPV DX
# WPV Visit Type
# WPV Smart
# WPV na weight
# WPV Row Sums

# %%
toc = time.time()
print(toc-tic, 'Sec Elapsed')
