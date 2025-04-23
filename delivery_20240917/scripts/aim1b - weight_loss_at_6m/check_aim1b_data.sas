******************************************************************************;
* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
  12/18/2024
  
  Script to test that output from R to a .csv file will open and function
  properly in SAS.

;
******************************************************************************;


* Import time to first relapse file;
proc import datafile = 'D:/PATHWEIGH/delivery_20240917/data/data_aim1b.csv'
    out = work.data
    dbms = csv
    replace;
run;

/* The only thing that doesn't work is that the Intervention variable lists
    Interve instead of Intervention because of the character limit */