%let folder = D:\PATHWEIGH\delivery_20240917\scripts\aim1b\bootstrap_test\code;
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