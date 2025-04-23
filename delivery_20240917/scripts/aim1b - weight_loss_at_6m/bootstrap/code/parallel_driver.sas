/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
   Parallel program driver.

   DESCRIPTION:
   The purpose of this program is to launch 4 separate programs using the
   systask command function to conduct the boot strap in parallel. Uses 4 CPUs
   because only 6 are available on the machine in use. Leaves 2 CPUs for other
   work-related tasks.

   The 1st serial run was used to generate the following bootstrapped/imputed
   data sets. All others were generated from previous runs/testing of the
   macros that Q.Pan wrote in serial.
   109-200
   202-300
   302-400
   501-600

   2nd run will need to have the following:
   601-700
   701-800
   801-900
   901-1000

   USAGE:
   - First, edit the files program1-program2
   - Ensure they have the correct libname
   - Ensure that each file has the correct prog_num set
   - Each file has the non-overlapping series of bootstrapped dataset to write
     i.e. start and num_bootstrap values which represent start and stop values.
*/
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */



%let folder = D:\PATHWEIGH\delivery_20240917\scripts\aim1b\bootstrap\code;
%let prog1 = program1.sas;
%let prog2 = program2.sas;
%let prog3 = program3.sas;
%let prog4 = program4.sas;

systask command """C:\Program Files\SASHome\SASFoundation\9.4\sas.exe""
                   -noterminal -nosplash -sysin ""&folder\&prog1""
                   -log ""&folder\program1_log.txt"""
          taskname=task1 status=rc1;

systask command """C:\Program Files\SASHome\SASFoundation\9.4\sas.exe"" 
                   -noterminal -nosplash -sysin ""&folder\&prog2"" 
                   -log ""&folder\program2_log.txt""" 
          taskname=task2 status=rc2;

systask command """C:\Program Files\SASHome\SASFoundation\9.4\sas.exe""
                   -noterminal -nosplash -sysin ""&folder\&prog3""
                   -log ""&folder\program3_log.txt"""
          taskname=task3 status=rc3;

systask command """C:\Program Files\SASHome\SASFoundation\9.4\sas.exe"" 
                   -noterminal -nosplash -sysin ""&folder\&prog4"" 
                   -log ""&folder\program4_log.txt""" 
          taskname=task4 status=rc4;

* Wait for both programs to finish before proceeding;
waitfor _all_ task1 task2 task3 task4;

%put Task 1 Return Code = &rc1;
%put Task 2 Return Code = &rc2;
%put Task 3 Return Code = &rc3;
%put Task 4 Return Code = &rc4;