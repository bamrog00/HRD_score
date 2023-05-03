# HRD_score

## Python
### format_files
A jupyter notebook which was used to format the allelic specific copy number files for the pan-cancer cohort.
The missing part was the ploidy but at the current time it is not factored in.

### find_missing_files
Because GDC has a limit of 10'000 files that can be downloaded in one go. Because the pan-cancer cohort contains over 11'000 cases, we had to check which files
were not downloaded in the first batch. It also creates the manifest file needed to download the missing files.

### check_data
This jupyter notebook can be used to get information about the pan-cancer cohort, such as number of different cancer types or number of primary, recurrent or metastatic samples.

### annotate_results
In this jupyter notebook which can be used to annotated the results from the HRDscar algorthim with the metadata.

### correlation_summary_plot
What to do with this? Is not used and needed anymore

## R

### HRDscore
R script which combutes the HRD scores (LOH, LST, TAI, HRD sum) from the formated files.

### correlation_HRD_scores
R script which creates correlation plots per cancer type comparing the three scores LOH, LST, TAI.
The script can also produce a summary plot containing all the correlation plots in one plot.

### plot_HRD_scores
R script for plotting the results from HRDscar.
Some of the plots can be either labeled with the Project ID (better more readable) or the study names (less readable). Plot possible with the Project ID are
labeled (PID) and those possible with study names (SN)

Possible plots are:
- Boxplot for each score containing all cancer types (PID/SN)
- Boxplot for each score containing all cancer types but split into the three sample types (Primary, Recurrent, Metastatic) (PID/SN)
- Boxplot per cancer type containg all four scores (LST, LOH, TAI, HRD sum) (PID)
- Summary plot containing the per cancer type boxplots (PID)
- HRDsum distribution plot per cancer (PID)
- Summary plot containing the per cancer type distribution plot (PID)
- HRDsum distribtuion plot containing all cancer types (PID)

### validation_SARC_scores
R script used for validation of the used HRDscar algorthim using the SARC cohort as a control (Results where given).
The script first calculates the scores using HRDscar for the TCGA-SARC cohort and then compares them to older results.
5 plots can be obtained: Histogram plot of the differences in TAI and a frequency plot for each score compairing the old and new results.

