/*
Author: Xiangji Ying
Date: 2025-08-06

This document is to help researchers calculate sample sizes for external pilot randomized controlled 
trials using SAS. It serves as a companion application to the forthcoming article titled "Determining 
sample size for pilot trials: A tutorial" (Ying X, et al. BMJ 2025;390:e083405. doi:10.1136/bmj-2024-083405). 
For details on the methods, examples, and sample size tables, please refer to the paper. Unless otherwise 
specified, all calculations refer to the total number of participants that will need to be enrolled in the pilot trial.
*/

/**************************************************************
 * Box 1 Example 1
  **************************************************************/
 
%macro propci_wilson(proportion, width, level=95, onesided=0);
/*
proportion: The estimated proportion (e.g., 0.5 for 50%)
width: The desired confidence interval width
level (optional, default=95): The confidence level percentage
onesided (optional, default=0): Set to 1 for a one-sided CI, otherwise 0 (default is two-sided)
*/

%let p = &proportion;
%let desired_width = &width;
%let alpha = %sysevalf((100 - &level) / 100);

%if &onesided = 1 %then %do;
    %let z = %sysfunc(probit(1 - &alpha));
    %let ci_type = one-sided;
%end;
%else %do;
    %let z = %sysfunc(probit(1 - &alpha / 2));
    %let ci_type = two-sided;
%end;

%let temp1 = %sysevalf(&p * (1 - &p));
%let temp2 = %sysevalf(&z * %sysfunc(sqrt(&temp1)));
%let temp3 = %sysevalf(&desired_width / 2);
%let temp4 = %sysevalf(&temp2 / &temp3);
%let n = %sysevalf(&temp4 ** 2);

%let tolerance = 0.0000000001;
%let max_iter = 100;
%let iter = 0;
%let converged = 0;

%do %while(&iter < &max_iter and &converged = 0);
    %let p1 = %sysevalf(&p + (&z**2) / (2 * &n));
    %let temp_sqrt = %sysevalf((&p * (1 - &p) / &n) + (&z**2) / (4 * &n**2));
    %let p2 = %sysevalf(%sysfunc(sqrt(&temp_sqrt)));
    %let denom = %sysevalf(1 + &z**2 / &n);
    %let current_width = %sysevalf(2 * &z * &p2 / &denom);

    %if %sysevalf(%sysfunc(abs(&current_width - &desired_width)) < &tolerance) %then %do;
        %let converged = 1;
    %end;
    %else %do;
        %let n = %sysevalf(&n * (&current_width / &desired_width)**2);
    %end;

    %let iter = %eval(&iter + 1);
%end;

data propci_wilson_results;
    format Expected_Proportion 8.4 Desired_CI_Width 8.4 Confidence_Level 8.2 CI_Type $10. Required_Sample_Size 8.4 Achieved_CI_Width 8.4;
    Expected_Proportion = &p;
    Desired_CI_Width = &desired_width;
    Confidence_Level = &level;
    CI_Type = "&ci_type";
    Required_Sample_Size = &n;
    Achieved_CI_Width = &current_width;
run;

%if &converged %then %do;
    proc print data=propci_wilson_results noobs; 
        title "Box 1 Example 1: Sample Size Based on Precision for One-sample Proportion (Wilson CI)";
    run;
%end;
%else %do;
    %put ERROR: No optimal sample size found within the range.;
%end;
%mend propci_wilson;

%propci_wilson(0.1, 0.2, level=95, onesided=1);




/**************************************************************
 * Box 1 Example 2
  **************************************************************/

*// * Score method * //;

%macro poissonci_score(rate=, width=, level=95, onesided=0);
    /* Parameters:
       rate      = Expected Poisson rate (events per time unit)
       width     = Desired total CI width
       level     = Confidence level (default 95%)
       onesided  = One-sided CI if 1, two-sided if 0 (default)
    */
    
    /* Adjust alpha for one-sided or two-sided CI */
    %if &onesided = 1 %then %do;
        %let alpha = %sysevalf((100-&level)/100);
        %let ci_type = One-sided;
    %end;
    %else %do;
        %let alpha = %sysevalf((100-&level)/200); /* Split alpha for two-sided CI */
        %let ci_type = Two-sided;
    %end;
    
    /* Convert confidence level to chi-square value */
    %let chi2 = %sysfunc(cinv(1-&alpha*2, 1));
    
    /* Initialize search for required time units */
    /* Start with rough normal approximation as initial guess */
    %let z = %sysfunc(probit(1-&alpha));
    %let T = %sysevalf((2*&z/&width)**2 * &rate);
    
    /* Iterative solution for T using score interval */
    %let done = 0;
    %let iter = 0;
    %let maxiter = 100;
    %let tolerance = 0.0000000001;
    
    %do %while(&done = 0 and &iter < &maxiter);
        /* Calculate score interval bounds for current T */
        %let lambda = %sysevalf(&rate*&T);
        %let L = %sysevalf((&lambda + &chi2/2 - %sysfunc(sqrt(&chi2**2/4 + &chi2*&lambda)))/&T);
        %let U = %sysevalf((&lambda + &chi2/2 + %sysfunc(sqrt(&chi2**2/4 + &chi2*&lambda)))/&T);
        
        /* Check if width matches desired width */
        %let current_width = %sysevalf(&U - &L);
        %let diff = %sysevalf(%sysfunc(abs(&current_width - &width)));
        
        %if &diff < &tolerance %then %do;
            %let done = 1;
        %end;
        %else %do;
            /* Adjust T proportionally */
            %let T = %sysevalf(&T * (&current_width/&width));
        %end;
        %let iter = %eval(&iter + 1);
    %end;
    
    /* Create a dataset to display results */
    data poisson_results;
        length CI_Type $10;
        rate = &rate;
        width = &width;
        level = &level;
        T = &T;
        achieved_width = &current_width;
        CI_Type = "&ci_type";
        if &done = 1 then Status = "Optimal";
        else Status = "Not Found";
    run;
    
    /* Display results in the results window */
    title "Box 1 Example 2: Sample Size Based on Precision for One-sample Poisson Rate (Score CI)";
    proc print data=poisson_results noobs;
        var rate width level T achieved_width CI_Type Status;
    run;
    title;
%mend poissonci_score;

/* Example call */
%poissonci_score(rate=10, width=6, level=95, onesided=0);


*// * Exact method * //;

%macro poissonci_exact(rate=, width=, level=95);
/* Parameters:
   rate = Expected Poisson rate (events per time unit)
   width = Desired CI width
   level = Confidence level (default 95%)
*/

/* Initialize variables */
%let optimal_T = -1;
%let min_diff = .;
%let optimal_width = .;
%let alpha = %sysevalf((100-&level)/100);

/* Create a dataset to store results */
data poissonciresults;
    length T 8 Events 8 LowerCI 8 UpperCI 8 Width 8 Diff 8;
    
    /* Loop over potential values for T */
    do i = 1 to 100;
        /* Use a data step loop instead of a macro loop for non-integer increments */
        T = 0.1 * i; /* This gives increments of 0.1 from 0.1 to 100 */
        
        /* Calculate expected events */
        Events = round(&rate * T, 1);
        
        /* Calculate exact Poisson confidence interval */
        if Events > 0 then do;
            /* Lower bound */
            LowerCI = cinv(&alpha/2, 2*Events)/2/T;
            /* Upper bound */
            UpperCI = cinv(1-&alpha/2, 2*(Events+1))/2/T;
        end;
        else do;
            /* If events = 0 */
            LowerCI = 0;
            UpperCI = cinv(1-&alpha/2, 2)/2/T;
        end;
        
        Width = UpperCI - LowerCI;
        Diff = abs(Width - &width);
        
        /* Only keep results that are close to the desired width */
        if Width <= &width then output;
    end;
run;

/* Find the optimal T with the smallest difference */
proc sort data=poissonciresults;
    by Diff;
run;

/* Get the best result and save to permanent dataset */
data work.poissonci_optimal;
    set poissonciresults(obs=1);
    Rate = &rate;
    DesiredWidth = &width;
    ConfidenceLevel = &level;
    format Rate 6.3 DesiredWidth 6.3 T 6.1 LowerCI UpperCI 12.6 Width 6.3;
    keep Rate DesiredWidth ConfidenceLevel T Events LowerCI UpperCI Width;
run;

/* Create a macro variable to check if we found a solution */
data _null_;
    set poissonciresults(obs=1);
    call symput('optimal_T', put(T, 6.1));
run;

/* Display results in a report */
proc print data=work.poissonci_optimal noobs label;
    title "Box 1 Example 2: Sample Size Based on Precision for One-sample Poisson Rate (Exact CI)";
    label Rate = "Expected Rate (events per unit)"
          DesiredWidth = "Desired CI Width"
          ConfidenceLevel = "Confidence Level (%)"
          T = "Required Units of Observation"
          Events = "Expected Number of Events"
          LowerCI = "Lower Bound of CI"
          UpperCI = "Upper Bound of CI"
          Width = "Achieved CI Width";
run;
title;

/* If no solution was found, print an error message */
%if &optimal_T = . or &optimal_T = -1 %then %do;
    data work.poissonci_optimal;
        Error = "No optimal units found within the range.";
    run;
    
    proc print data=work.poissonci_optimal noobs;
        title "Poisson CI Calculation Error";
    run;
    title;
%end;

%mend poissonci_exact;

/* Example usage */
%poissonci_exact(rate=10, width=6, level=95);

/**************************************************************
 * Box 2 Example 1
  **************************************************************/
title "Box 2 Example 1: Sample Size for Testing Feasibility Progression Criteria (Proportion)";
proc power;
	onesamplefreq test=adjz method=normal
	nullproportion = 0.5
	proportion = 0.2
	sides = l
	ntotal = .
	power = .95
	alpha = 0.05;
run;

/**************************************************************
 * Box 2 Example 2
  **************************************************************/
 
%macro poisson_exact(lambda1=, lambda0=, alpha=0.05, beta=0.1);
    /* Initialize parameters */
    %let d = 1;
    %let found = 0;

    /* Loop until we find valid n */
    %do %while (&found = 0);
        /* Calculate bounds using chi-square distribution in a data step */
        data _null_;
            lower = quantile('CHISQ', 1 - &beta, 2 * &d) / (2 * &lambda1);
            upper = quantile('CHISQ', &alpha, 2 * &d) / (2 * &lambda0);
            call symputx('lower', lower);
            call symputx('upper', upper);
        run;

        /* Check if interval contains at least one integer */
        %if %sysfunc(ceil(&lower)) <= %sysfunc(floor(&upper)) %then %do;
            %let n = &lower;
            %let found = 1;
        %end;
        %else %do;
            %let d = %eval(&d + 1);
        %end;

        /* Add safety break to prevent infinite loop */
        %if &d > 1000 %then %do;
            %put ERROR: No solution found within 1000 iterations;
            %abort;
        %end;
    %end;

    /* Calculate the ceiling of n */
    %let n_ceil = %sysfunc(ceil(&n));

    /* Create a dataset to store results */
    data poisson_results;
        length Parameter $32 Value 8;
        Parameter = "Required number of observation units (n)"; Value = &n_ceil; output;
        Parameter = "Degree of freedom (d)"; Value = &d; output;
        Parameter = "Lower bound"; Value = &lower; output;
        Parameter = "Upper bound"; Value = &upper; output;
    run;

    /* Display results in the Results Viewer using PROC REPORT */
    title "Box 2 Example 2: Sample Size for Testing Feasibility Progression Criteria (Poisson rate)";
    proc report data=poisson_results nowd;
        column Parameter Value;
        define Parameter / display "Parameter";
        define Value / display "Value";
    run;

    /* Clear the title */
    title;
%mend poisson_exact;

/* Example usage */
%poisson_exact(lambda1=10, lambda0=6, alpha=0.05, beta=0.1)


/**************************************************************
 * Box 3
  **************************************************************/
 
/* Set parameters */
%let gamma = 0.95; /* Confidence level */
%let pi = 0.1;    /* Anticipated probability of the event of interest */

/* Calculate sample size */
data sample_size;
  sample_size = ceil(log(1 - &gamma) / log(1 - &pi));
run;

/* Display in results window */
proc print data=sample_size noobs;
  title "Box 3: Sample Size for Detecting a Feasibility Problem";
run;

/**************************************************************
 * Box 4 NCT Method for Optimal Pilot Sample Size Calculation
  **************************************************************/

/* Define a macro to calculate the sample size based on the NCT method */
%macro optPilotSizeNCT(d=, alpha=0.05, beta=0.2, min_np=2, max_np=10000);
    /* Step 1: Validate inputs */
    %if %sysevalf(&d < 0) or %sysevalf(&d > 1) or %sysevalf(%superq(d) =) %then %do;
        %put ERROR: d must be between 0 and 1 and cannot be missing.;
        %return;
    %end;

    %if %sysevalf(&alpha <= 0) or %sysevalf(&alpha >= 1) or %sysevalf(%superq(alpha) =) %then %do;
        %put ERROR: alpha must be between 0 and 1 and cannot be missing.;
        %return;
    %end;

    %if %sysevalf(&beta <= 0) or %sysevalf(&beta >= 1) or %sysevalf(%superq(beta) =) %then %do;
        %put ERROR: beta must be between 0 and 1 and cannot be missing.;
        %return;
    %end;

    %if %sysevalf(&min_np < 2) or %sysevalf(&max_np < 2) %then %do;
        %put ERROR: The minimum allowed value for min_np and max_np is 2.;
        %return;
    %end;

    /* Step 2: Calculate definitive trial sample size per group (n_m) */
    /* Suppress the output of PROC POWER */
    ods exclude all;
    proc power;
        twosamplemeans test=diff
        	nfractional
            meandiff = &d
            stddev = 1
            alpha = &alpha
            power = %sysevalf(1 - &beta)
            npergroup = .
            sides = 2;
        ods output output = power_results;
    run;
    /* Restore the default ODS behavior */
    ods exclude none;
    
    data _null_;
        set power_results;
        call symputx('n_m', put(FractionalN, best12.));
    run;

    /* Step 3: Calculate overall sample size for each pilot sample size */
    data nct_results;
        do n_p = &min_np to &max_np;
            /* Calculate the inflated definitive trial sample size per group */
            /* Compute the non-centrality parameter */
		    ncp = quantile('T', 1 - &alpha / 2, 2 * &n_m - 2);
		
		    /* Compute the quantile of the non-central t-distribution */
		    t_quantile = quantile('T', 1 - &beta, 2 * n_p - 2, ncp);

    		/* Perform the final calculation */
            infl_n_m = 2 * (t_quantile)**2 / &d**2;
            
            /* Calculate the overall sample size per group */
            N = n_p + infl_n_m;
            output;
        end;
    run;

    /* Step 4: Find the pilot sample size that minimizes the overall sample size */
    proc sort data=nct_results;
        by N;
    run;

    data optimal_results;
        set nct_results (obs=1); /* Keep only the first row (minimum N) */
        n_pilot = n_p * 2;       /* Pilot sample size for both arms */
        n_full_scale = infl_n_m * 2; /* Inflated definitive trial sample size for both arms */
        n_total = N * 2;          /* Total sample size for both arms */
        /* Add calculation parameters */
        effect_size = &d;
        alpha_level = &alpha;
        beta_level = &beta;
        power = 1-&beta;
    run;
    /* Step 5: Display the results */
    proc report data=optimal_results nowd;
        column ("Input Parameters" effect_size alpha_level beta_level power) 
               ("Sample Size Results" n_pilot n_full_scale n_total);
        define effect_size / "Effect Size" format=8.3;
        define alpha_level / "Alpha" format=8.3;
        define beta_level / "Beta" format=8.3;
        define power / "Power" format=8.3;
        define n_pilot / "Pilot Study n (Total)" format=8.0;
        define n_full_scale / "Definitive Study n (Total)" format=8.2;
        define n_total / "Total Sample Size (Both Phases)" format=8.2;
        title "Box 4: Sample Size for Minimizing Total Sample Size (NCT Method)";
    run;
%mend optPilotSizeNCT;

/* Example usage */
%optPilotSizeNCT(d=0.4, alpha=0.05, beta=0.2, min_np=2, max_np=1000);


/**************************************************************
 * Box 4 UCL Method for Optimal Pilot Sample Size Calculation
 **************************************************************/

%macro optPilotSizeUCL(d=, alpha=0.05, beta=0.2, std_conf_level=0.8, 
                       min_np=2, max_np=10000);
    
    /* Create a dataset with possible values of pilot sample size per group */
    data n_pilot;
        do n_p = &min_np to &max_np;
            output;
        end;
    run;

    /* Calculate definitive trial sample size per group using PROC POWER */
    /* Suppress the output of PROC POWER */
    ods exclude all;
    proc power;
        twosamplemeans test=diff
        	nfractional
            meandiff = &d
            stddev = 1
            alpha = &alpha
            power = %sysevalf(1 - &beta)
            npergroup = .
            sides = 2;
        ods output output = power_results;
    run;
    /* Restore the default ODS behavior */
    ods exclude none;
    
    /* Extract the sample size from PROC POWER results */
    data _null_;
        set power_results;
        call symputx('n_m', put(FractionalN, best12.));
    run;

    /* Calculate overall sample size for each potential pilot sample size */
    data results;
        set n_pilot;
        n_m = &n_m;
        
        /* Calculate inflated definitive trial sample size per group */
        full_scale_size = n_m * (2 * n_p - 2) / cinv(1 - &std_conf_level, 2 * n_p - 2);
        
        /* Calculate overall sample size per group */
        overall_size = n_p + full_scale_size;
        
        /* Store total sample sizes (both arms) */
        n_pilot_total = n_p * 2;
        n_full_scale_total = full_scale_size * 2;
        n_overall_total = overall_size * 2;
    run;

    /* Find the pilot sample size that minimizes the overall sample size */
    proc sort data=results;
        by overall_size;
    run;

    /* Create output dataset with the optimal results */
    data optimal_results;
        set results(obs=1);

        /* Add calculation parameters */
        effect_size = &d;
        alpha_level = &alpha;
        beta_level = &beta;
        power = 1-&beta;
        std_conf_level = &std_conf_level;
        
        /* Keep the original variables plus the new ones */
        n_pilot = n_pilot_total;
        n_full_scale = n_full_scale_total;
        n_total = n_overall_total;
    run;
    /* Print the results */
    proc report data=optimal_results nowd;
        column ("Input Parameters" effect_size alpha_level beta_level power std_conf_level) 
               ("Sample Size Results" n_pilot n_full_scale n_total);
        define effect_size / "Effect Size" format=8.3;
        define alpha_level / "Alpha" format=8.3;
        define beta_level / "Beta" format=8.3;
        define power / "Power" format=8.3;
        define std_conf_level / "Std Dev Conf Level" format=8.3;
        define n_pilot / "Pilot Study n (Total)" format=8.0;
        define n_full_scale / "Definitive Study n (Total)" format=8.2;
        define n_total / "Total Sample Size (Both Phases)" format=8.2;
        title "Box 4: Sample Size for Minimizing Total Sample Size (UCL Method)";
    run;
    title;
%mend optPilotSizeUCL;

/* Run the examples */
%optPilotSizeUCL(d=0.4, alpha=0.05, beta=0.2, std_conf_level=0.8, min_np=2, max_np=1000);


/*-----------------------------------------------------------------------
 * Box 5 **Continuous Outcomes**
 * Calculates sample sizes for confidence intervals for mean differences
 *-----------------------------------------------------------------------*/

%macro size_ci_stdmean2(alpha, d, w, R);
    data results;
        z = probit(1 - &alpha/2);
        n11 = ceil((&d**2*(1 + &R)/(2*&R) + 4*(1 + &R)/&R)*(z/&w)**2);
        n12 = ceil(&R * n11);

        /* Create output dataset */
         n1 = n11; n2 = n12; output;

        keep n1 n2;
    run;

    /* Print results */
    proc print data=results noobs;
        var n1 n2;
        title "Box 5: Sample Size for Excluding Interventions (Continuous Outcome)";
    run;
%mend size_ci_stdmean2;

/* Example usage */
%size_ci_stdmean2(0.4, 0, 0.6, 1);


/*-----------------------------------------------------------------------
 * Box 5 **Binary Outcomes**
 * Calculates sample sizes for confidence intervals for proportion differences
 *-----------------------------------------------------------------------*/

 * 1. Newcombe CI;
%macro propdiffci_newcombe(p1=, p2=, conf_width=, level=);
    /* Convert confidence level to alpha */
    %let alpha = %sysevalf(1 - &level);
    %let z = %sysfunc(probit(1 - &alpha/2));
    
    /* Create dataset to store results of the search */
    data search_results;
        p1 = &p1;
        p2 = &p2;
        conf_width = &conf_width;
        z = &z;
        
        min_diff = .;
        optimal_n1 = .;
        
        /* Loop over potential values for n1 */
        do n1 = 1 to 1000;
            /* Set n2 equal to n1 */
            n2 = n1;
            
            /* Calculate Wilson confidence intervals for p1 */
            ci1_lwr = (p1 + z**2/(2*n1) - z*sqrt((p1*(1-p1)/n1) + (z**2/(4*n1**2)))) / (1 + z**2/n1);
            ci1_upr = (p1 + z**2/(2*n1) + z*sqrt((p1*(1-p1)/n1) + (z**2/(4*n1**2)))) / (1 + z**2/n1);
            
            /* Calculate Wilson confidence intervals for p2 */
            ci2_lwr = (p2 + z**2/(2*n2) - z*sqrt((p2*(1-p2)/n2) + (z**2/(4*n2**2)))) / (1 + z**2/n2);
            ci2_upr = (p2 + z**2/(2*n2) + z*sqrt((p2*(1-p2)/n2) + (z**2/(4*n2**2)))) / (1 + z**2/n2);
            
            /* Calculate confidence width using Newcombe's method */
            cw = sqrt((p1 - ci1_lwr)**2 + (ci2_upr - p2)**2) + sqrt((ci1_upr - p1)**2 + (p2 - ci2_lwr)**2);
            
            /* Calculate the difference between cw and conf_width */
            diff = abs(conf_width - cw);
            
            /* Check if this is a viable solution (cw < conf_width) and has smallest difference */
            if cw < conf_width then do;
                if min_diff = . or diff < min_diff then do;
                    min_diff = diff;
                    optimal_n1 = n1;
                    output;
                end;
            end;
        end;
    run;
    
    /* Get the optimal n1 */
    proc sql noprint;
        select optimal_n1 into :opt_n1 
        from search_results
        having min_diff = min(min_diff);
    quit;
    
    /* Create a dataset with the final result for display */
    data final_result;
        length Parameter $40 Value 8;
        
        Parameter = "Proportion 1 (p1)";
        Value = &p1;
        output;
        
        Parameter = "Proportion 2 (p2)";
        Value = &p2;
        output;
        
        Parameter = "Confidence Level";
        Value = &level;
        output;
        
        Parameter = "Desired CI Width";
        Value = &conf_width;
        output;
        
        %if %length(&opt_n1) > 0 %then %do;
            Parameter = "Optimal Sample Size per Group";
            Value = &opt_n1;
            output;
            
            /* Calculate the actual confidence width at the optimal n1 */
            n1 = &opt_n1;
            n2 = &opt_n1;
            z = &z;
            p1 = &p1;
            p2 = &p2;
            
            /* Wilson CI for p1 */
            ci1_lwr = (p1 + z**2/(2*n1) - z*sqrt((p1*(1-p1)/n1) + (z**2/(4*n1**2)))) / (1 + z**2/n1);
            ci1_upr = (p1 + z**2/(2*n1) + z*sqrt((p1*(1-p1)/n1) + (z**2/(4*n1**2)))) / (1 + z**2/n1);
            
            /* Wilson CI for p2 */
            ci2_lwr = (p2 + z**2/(2*n2) - z*sqrt((p2*(1-p2)/n2) + (z**2/(4*n2**2)))) / (1 + z**2/n2);
            ci2_upr = (p2 + z**2/(2*n2) + z*sqrt((p2*(1-p2)/n2) + (z**2/(4*n2**2)))) / (1 + z**2/n2);
            
            /* Actual CI width */
            actual_width = sqrt((p1 - ci1_lwr)**2 + (ci2_upr - p2)**2) + sqrt((ci1_upr - p1)**2 + (p2 - ci2_lwr)**2);
            
            Parameter = "Actual CI Width";
            Value = actual_width;
            output;
            
            Parameter = "Total Sample Size";
            Value = 2 * &opt_n1;
            output;
        %end;
        %else %do;
            Parameter = "Optimal Sample Size per Group";
            Value = .;
            output;
            
            Parameter = "Note";
            Value = .;
            output;
        %end;
    run;
    
    /* Display the results in the output window with nice formatting */
    title "Box 5: Sample Size for Excluding Interventions (Binary Outcome, Newcombe Method)";
    *title2 "Results for p1=&p1, p2=&p2, conf_width=&conf_width, level=&level";
    
    proc print data=final_result noobs label;
        var Parameter Value;
        label Parameter = "Parameter" 
              Value = "Value";
        %if %length(&opt_n1) = 0 %then %do;
            format Value 8.;
            footnote "No optimal sample size found within the search range (1-1000).";
        %end;
        %else %do;
            format Value 8.4;
        %end;
    run;
    
    title;
    footnote;
    
    /* Return the optimal n1 as a global macro variable */
    %global optimal_n1;
    %let optimal_n1 = &opt_n1;
%mend propdiffci_newcombe;

/* Example usage */
%propdiffci_newcombe(p1=0.5, p2=0.5, conf_width=0.2, level=0.6);
%put Optimal sample size: &optimal_n1;



 * Wald CI;
data sample_size;
    /* Set parameters */
    oneside_ci_level = 0.8; /* Confidence level */
    alpha = (1 - oneside_ci_level) * 2; /* Alpha corresponding to the confidence limit */
    p1 = 0.5; /* Proportion of the participants with the outcome in control group */
    p2 = 0.5; /* Proportion of the participants with the outcome in treatment group */
    w = 0.1 * 2; /* Width of the confidence interval or twice the clinically meaningful difference */

    /* Calculate pilot trial sample size per group using the Wald test */
    Pilot_Size_Per_Group = 4 * (p1 * (1 - p1) + p2 * (1 - p2)) * (probit(alpha / 2))**2 / (w**2);

    /* Output results */
    output;
run;

/* Print results */
proc print data=sample_size noobs;
    *var Pilot_Size_Per_Group;
    var oneside_ci_level alpha p1 p2 w Pilot_Size_Per_Group;
    title "Box 5: Sample Size for Excluding Interventions (Binary Outcome, Wald Method)";
run;


