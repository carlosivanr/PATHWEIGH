options nofmterr nonotes;

/* libname PW "C:\Users\bsc-default\Documents\bcs\PathWeigh"; */
libname PW "D:\PATHWEIGH\delivery_20240917\scripts\aim1b\bootstrap";

*read in the CSV data with n=9358*2;

/* Set the start of the bootstrap iteration to break up the processing */
%let start = 201;

/* Set the number of bootstrap samples, or the stop */
%let num_bootstrap = 300;


/* Set the output templog */
proc printto log="templog_&start-&num_bootstrap..log" new;
run;


/* Input dataset name */
%let input_dataset = pw.analysis;

/* Output dataset for bootstrap samples */
%let output_dataset = bootstrap_samples;

/* Unique subject identifier variable */
%let id_var = id;

/* Create a dataset to hold the bootstrapped samples */
data &output_dataset.;
    set &input_dataset.;
    bootstrap_iter = 0; /* This creates an identical dataset with the same structure as the input */
	newid=.;
run;

/* Get unique subject IDs */
proc sql;
    create table unique_ids as
    select distinct &id_var
    from &input_dataset.;
quit;

%macro bootstrap;
    %do i = &start %to &num_bootstrap;
        /* Resample unique subjects with replacement */
        proc surveyselect data=unique_ids
                          out=resampled_ids&i
                          method=urs /* Unrestricted random sampling with replacement */
                          samprate=1 
                          outhits; /* Allows duplicate rows for bootstrapping */
        run;
		data resampled_ids&i;
            set resampled_ids&i;
			newid=_n_; drop numberhits;
        run;        /* Merge resampled subject IDs back with the original data */
        proc sql;
            create table sample&i as
            select a.*, b.*
            from &input_dataset. as a
            inner join resampled_ids&i as b
            on a.&id_var = b.&id_var;
        quit;

        /* Add a variable to indicate the bootstrap iteration */
        data sample&i;
            set sample&i;
            bootstrap_iter = &i; /* Indicates which bootstrap iteration */
        run;

        /* Append the bootstrapped sample to the output dataset */
        proc append base=&output_dataset.
                     data=sample&i
                     force; /* Ensure structure matches even if issues arise */
        run;

        /* Delete the temporary sample dataset to save space (optional) 
        proc datasets lib=work nolist;
            delete sample&i resampled_ids&i;
        quit;*/
    %end;
%mend bootstrap;

/* Execute the macro */
%bootstrap;

data pw.bootstrap_samples; set bootstrap_samples; run;

/* Tabulations of GBD regions and 10-year calendar year periods 
proc freq data = pw.analysis;
table intervention;
run;
data pw.analysis; set pw.analysis; rename arb_personID=ID n_months_post_id=month; run;
proc sort data=pw.analysis; by intervention id month; run;
/************* Dividing the combined dataset into sub datasets by GBD super region *************/
%macro imputation;
   %do i = &start %to &num_bootstrap;
data intervention;
set pw.bootstrap_samples;
where intervention eq "Interve" & bootstrap_iter = &i;
run;

data control;
set pw.bootstrap_samples;
where intervention eq "Control" & bootstrap_iter = &i;
run;

/************************************************************************************************************/
/************ Stratified imputation(by phase) using restricted cubic splines *********************/
/************************************************************************************************************/

/*************************************************/
/********** control **********/
/************************************
proc univariate data = control;		
var month;		
output out = temp pctlpre=P_ pctlpts=25,50,75;		
run;	
proc print data = temp; run;*************/
proc hpmixed data= control;
class newid; 		
effect spl = spline(month / details naturalcubic basis=tpf(noint) knotmethod=list(1,4,10)); 		
model weight = spl / ddfm=residual solution;		
random int spl/ type=un subject=newid solution; 		
ods output ParameterEstimates=sf_1(keep=effect estimate 		
rename=(estimate=overall));		
ods output solutionr=sr_1(keep=effect spl Subject estimate 		
                             rename=(estimate=ssdev));		
run; 


*preparing to merge datasets with solutions for fixed and random effects into one dataset; 		
data sf;		
	set sf_1;	
	splnum=_n_-1;	
	if effect='Intercept' then Param='Intercept';	
	else if effect='spl' then Param=cats(effect,splnum); 	
	drop effect;	
run;		
		
data sr;		
	set sr_1;	
	if effect='Intercept' then Param='Intercept'; 	
	else if effect='spl' then Param=cats(effect,spl);	
	newid = Subject; 	
	drop effect;	
run; 		
		
proc sort data=sf; by Param; run;		
proc sort data=sr; by Param; run;		
		
*merging the datasets and calculating subject specific coefficients; 		
data final;		
merge sf sr;		
by Param;		
sscoeff = overall + ssdev; 		
run; 		
		
proc sort data=final; by newid Param; run; 		
		
*transposing data to put subjects in rows; 		
proc transpose data=final out=splcoeff prefix=_; 		
by newid;		
id Param;		
var sscoeff; 		
run; 		

data splcoeff_6;
set splcoeff;

drop _name_;

k_n = 10;		
k_n_1 = 4;		
k_i2_1 = 1;	
k_1 = 1;	

ga_impute = 6;		
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);				

weight_imputed_6 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep newid weight_imputed_6;
label weight_imputed_6 = "Imputed weight at 6 months after index visit";
run;

data splcoeff_12;
set splcoeff;

drop _name_;

k_n = 10;		
k_n_1 = 4;		
k_i2_1 = 1;	
k_1 = 1;

ga_impute = 12;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);		

weight_imputed_12 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep newid weight_imputed_12;
label weight_imputed_12 = "Imputed weight at 12 months after index visit";
run;	

data splcoeff_18;
set splcoeff;

drop _name_;

k_n = 10;		
k_n_1 = 4;		
k_i2_1 = 1;	
k_1 = 1;

ga_impute = 18;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);		

weight_imputed_18 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep newid weight_imputed_18;
label weight_imputed_18 = "Imputed weight at 18 months after index visit";
run;	

/* Merging the imputed weights */			
proc sort data = splcoeff_6; by newid; run;
proc sort data = splcoeff_12; by newid; run;
proc sort data = splcoeff_18; by newid; run;
data pw.control&i;
merge splcoeff_6 splcoeff_12 splcoeff_18;				
by newid;	
keep newid weight_imputed_6 weight_imputed_12 weight_imputed_18;
run;	

        /* Delete the temporary sample dataset to save space (optional) */
        proc datasets lib=work nolist;
            delete sf_1 sr_1 final sf sr splcoeff splcoeff_6 splcoeff_12 splcoeff_18;
        quit;
/*************************************************/
/********** intervention **********/
/***********************************
proc univariate data = intervention;		
var month;		
output out = temp pctlpre=P_ pctlpts=25,50,75;		
run;	
proc print data = temp; run;**************/
proc hpmixed data= intervention;
class newid; 		
effect spl = spline(month / details naturalcubic basis=tpf(noint) knotmethod=list(1,5,11)); 		
model weight = spl / ddfm=residual solution;		
random int spl/ type=un subject=newid solution; 		
ods output ParameterEstimates=sf_1(keep=effect estimate 		
rename=(estimate=overall));		
ods output solutionr=sr_1(keep=effect spl Subject estimate 		
                             rename=(estimate=ssdev));		
run; 


*preparing to merge datasets with solutions for fixed and random effects into one dataset; 		
data sf;		
	set sf_1;	
	splnum=_n_-1;	
	if effect='Intercept' then Param='Intercept';	
	else if effect='spl' then Param=cats(effect,splnum); 	
	drop effect;	
run;		
		
data sr;		
	set sr_1;	
	if effect='Intercept' then Param='Intercept'; 	
	else if effect='spl' then Param=cats(effect,spl);	
	newid = Subject; 	
	drop effect;	
run; 		
		
proc sort data=sf; by Param; run;		
proc sort data=sr; by Param; run;		
		
*merging the datasets and calculating subject specific coefficients; 		
data final;		
merge sf sr;		
by Param;		
sscoeff = overall + ssdev; 		
run; 		
		
proc sort data=final; by newid Param; run; 		
		
*transposing data to put subjects in rows; 		
proc transpose data=final out=splcoeff prefix=_; 		
by newid;		
id Param;		
var sscoeff; 		
run; 		

data splcoeff_6;
set splcoeff;

drop _name_;

k_n = 11;		
k_n_1 = 5;		
k_i2_1 = 1;	
k_1 = 1;	

ga_impute = 6;		
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);				

weight_imputed_6 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep newid weight_imputed_6;
label weight_imputed_6 = "Imputed weight at 6 months after index visit";
run;

data splcoeff_12;
set splcoeff;

drop _name_;

k_n = 11;		
k_n_1 = 5;		
k_i2_1 = 1;	
k_1 = 1;

ga_impute = 12;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);		

weight_imputed_12 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep newid weight_imputed_12;
label weight_imputed_12 = "Imputed weight at 12 months after index visit";
run;	

data splcoeff_18;
set splcoeff;

drop _name_;

k_n = 11;		
k_n_1 = 5;		
k_i2_1 = 1;	
k_1 = 1;

ga_impute = 18;	
		
u_n = (max((ga_impute - k_n),0))**3;		
u_i2_1 = (max((ga_impute - k_i2_1),0))**3;
u_n_1 = (max((ga_impute - k_n_1),0))**3;		
		
ga_impute_spl2 = (u_i2_1 - u_n_1*(k_n-k_i2_1)/(k_n-k_n_1) + u_n*(k_n_1-k_i2_1)/(k_n-k_n_1))/((k_n-k_1)**2);		

weight_imputed_18 = round(_Intercept + (_spl1*ga_impute) + (_spl2*ga_impute_spl2), 0.01); 

keep newid weight_imputed_18;
label weight_imputed_18 = "Imputed weight at 18 months after index visit";
run;	

/* Merging the imputed weights */			
proc sort data = splcoeff_6; by newid; run;
proc sort data = splcoeff_12; by newid; run;
proc sort data = splcoeff_18; by newid; run;
data pw.intervention&i;
merge splcoeff_6 splcoeff_12 splcoeff_18;				
by newid;	
keep newid weight_imputed_6 weight_imputed_12 weight_imputed_18;
run;	
        /* Delete the temporary sample dataset to save space (optional) */
        proc datasets lib=work nolist;
            delete sf_1 sr_1 final sf sr splcoeff splcoeff_6 splcoeff_12 splcoeff_18;
        quit;
		proc sort data=control nodupkey; by newid; run;
		data control&i; set pw.control&i; newidextra=substr(newid,7); drop newid; run;
		data control&i; set control&i; newid=newidextra*1; drop newidextra;run;
		proc sort data=control&i; by newid; run;
		data pw.control&i; merge control&i(in=in) control; by newid; if in; run;

		proc sort data=intervention nodupkey; by newid; run;
		data intervention&i; set pw.intervention&i; newidextra=substr(newid,7); drop newid; run;
		data intervention&i; set intervention&i; newid=newidextra*1; drop newidextra;run;
		proc sort data=intervention&i; by newid; run;
		data pw.intervention&i; merge intervention&i(in=in) intervention; by newid; if in; run;

    %end;
%mend imputation;

/* Execute the macro */
%imputation;
 
