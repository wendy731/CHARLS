Joint Impact of Sleep and Physical Activity on Hypertension - Replication Code
Project Overview
This repository contains complete replication code for the study Joint Impact of Sleep and Physical Activity on Hypertension among middle-aged Chinese women.
Using longitudinal data from the China Health and Retirement Longitudinal Study (CHARLS), this analysis evaluates:
Independent effects of sleep duration and physical activity (PA) on hypertension risk
Joint effects across 6 combined sleep–PA groups
Non-linear associations using restricted cubic splines (RCS)
Kaplan–Meier survival curves
Subgroup, sensitivity, and interaction analyses
Participant selection flow matching the final cohort of 2,097 women
All code is debugged, reproducible, and ready for peer review.
File Name	Description
Hypertension.R	Main debugged replication script (all analyses, tables, figures)
code2.R	Original unmodified script (reference only; deprecated functions)
Data_Cleaning_and_Longitudinal_Analysis.R	Participant selection & cohort derivation code (replicates N=2,097 sample)
Joint Impact CHARLS.xlsx	Cleaned analysis dataset (required for full replication)
README.md	This setup & execution guide
## Joint Impact CHARLS.xlsx (Cleaned Dataset)
- File: `Joint Impact CHARLS.xlsx`
- Sample: 2,097 normotensive women aged 40–60 at CHARLS 2011 baseline
- Source: Derived from CHARLS 2011, 2015, 2019 waves
- Format: One sheet (`CleanedData`) with 2,097 rows × 24 variables
- Key variables: Age, BMI, residence, menopause, sleep duration (hours/category), physical activity (MET-min/category), 6 joint sleep–PA groups, baseline SBP/DBP, glucose, cholesterol, smoking, alcohol, education, income, kidney disease, diabetes, incident hypertension, follow-up years
### Participant Flow (Final Cohort)
- Initial CHARLS 2011: 17,708
- Women 40–60: 6,458
- Exclude baseline HTN/CVD: 3,114 excluded
- Exclude missing data: 1,247 excluded
- Final sample: 2,097
This Excel file is required to run `Hypertension.R` and `Data_Cleaning_and_Longitudinal_Analysis.R`.
Variable Name	Type	Description
ID	Character	Unique participant ID (ID1–ID2097)
age	Numeric	Baseline age (years, 40–60)
bmi	Numeric	Baseline BMI (kg/m²)
residence	Binary	0=Rural, 1=Urban
menopause	Binary	0=Pre-menopausal, 1=Post-menopausal
sleep_duration	Numeric	Self-reported nightly sleep (hours)
sleep_cat	Factor	Short (<8h) / Adequate (≥8h)
PA_MET	Numeric	Weekly physical activity (MET-min)
PA_cat	Factor	Low (<600) / Moderate (600–3000) / High (>3000)
Joint_Group	Factor	6 groups: Adequate/High, Adequate/Mod, Adequate/Low, Short/High, Short/Mod, Short/Low
SBP_base	Numeric	Baseline third-measure SBP (mmHg)
DBP_base	Numeric	Baseline third-measure DBP (mmHg)
fasting_glucose	Numeric	Fasting blood glucose (mmol/L)
total_cholesterol	Numeric	Total cholesterol (mg/dL)
smoking	Binary	0=Non-smoker, 1=Smoker
alcohol	Binary	0=Non-drinker, 1=Drinker
education	Categorical	1=Primary or less, 2=Middle school, 3=High school+, 4=College+
income	Numeric	Annual household income (CNY)
kidney_disease	Binary	0=No, 1=Yes
diabetes	Binary	0=No, 1=Yes
Incident_HTN	Binary	0=No hypertension, 1=New hypertension during follow-up (2011–2019)
FollowUp_Years	Numeric	Follow-up time (years, 1–8)
Prerequisites
Software
Ensure you have R (version ≥ 4.0.0) and RStudio installed (recommended for ease of use). Download R from CRAN.
Required R Packages
The following packages are used in the replication script. The script includes code to install them (uncomment if needed):required_pkgs <- c( 
  "readxl", "dplyr", "tidyr", "survival", "survminer", "ggplot2", 
  "rms", "mice", "broom", "gtsummary", "gt", "gridExtra", 
  "scales", "lmtest", "car", "purrr", "flextable", "officer", "writexl" )
Setup Instructions
Download Files
1. Clone this GitHub repository (or download all files manually).
2. Place all files in a dedicated folder (e.g., C:/Users/Administrator/Desktop/hypertension/).
Update File Path (Critical!)
In both R scripts, locate the file_path variable (under the "1. READ DATA" section) and update it to match the location of your Excel dataset:
# Example (update this line to your actual file path)
file_path <- "C:/Users/Administrator/Desktop/hypertension/Joint Impact CHARLS.xlsx" # Path to your cleaned Excel dataset
How to Run the Analysis
Use the debugged script (Hypertension.R) for full replication (recommended). The original script (code2.R) is provided for reference only.
1. Open Hypertension.R in RStudio.
2. Run the script line-by-line (or use "Run All" Ctrl+Shift+Enter) to execute all analyses.
3. If prompted to install missing packages, uncomment the install.packages() lines at the top of the script and run them once.
Key Notes for Running the Script
- Multiple Imputation (MICE): The script uses 5 imputations to handle missing data. This may take a few minutes to run.
- Figure/Table Outputs: All figures (PNG format) and tables (HTML/Excel/Word) will be saved to the same folder as the script. Ensure you have write permissions for the folder.
- Deprecated Functions: The debugged script fixes deprecated functions (e.g., replacing aov with oneway.test in gtsummary) to avoid errors.
- Xfun Conflict: If you encounter errors when printing tables/figures, update the xfun package (run install.packages("xfun")) and restart R.
Outputs Generated
Figures (PNG)
- Figure2_RCS_PhysicalActivity.png: Non-linear association between physical activity (MET-min/week) and hypertension (restricted cubic splines).
- Figure3_RCS_SleepDuration_CleanU.png: U-shaped association between sleep duration and hypertension (restricted cubic splines).
- Figure4_ForestPlot_JointHR.png: Forest plot of hazard ratios for joint sleep-PA groups (fully adjusted model).
- Figure4a_Sensitivity_Age_BMI.png: Sensitivity analysis forest plot (short sleep/low PA by age and BMI).
- Figure5_KaplanMeier.png: Kaplan-Meier cumulative incidence curves for hypertension by joint sleep-PA group.
- Figure7_SubgroupForest.png: Subgroup analysis forest plot (short sleep/low PA vs. reference group).
- Figure8_InteractionPvalues.png: Interaction p-values for joint sleep-PA effects by modifiers (age, BMI, etc.).
Tables
- Table1_Baseline.html: Baseline characteristics by joint sleep-PA group (formatted HTML).
- Hypertension_Tables.docx: Word document with Table 1 (baseline) and Table 2 (hazard ratios), plus key figures.
- Hypertension_Tables.xlsx: Excel file with two sheets (Table 1 and Table 2) for easy manipulation.
Analysis Overview
The debugged script includes the following key analyses (aligned with the original study):
- Data Preparation: Recoding variables, creating sleep/PA subgroups, and handling missing data via MICE.
- Baseline Characteristics (Table 1): Summary statistics by joint sleep-PA group, with ANOVA/chi-square p-values.
- Independent Cox Regression: Effects of sleep (short vs. adequate) and physical activity (low/moderate/high) on hypertension risk.
- Joint Effect Cox Regression (Table 2): Hazard ratios for incident hypertension across 6 joint sleep-PA groups (reference: Adequate Sleep / High PA).
- Non-linear Analyses: Restricted cubic splines (RCS) for continuous sleep duration and physical activity.
- Survival Analyses: Kaplan-Meier curves and log-rank tests.
- Subgroup & Sensitivity Analyses: Subgroup analyses by age, BMI, menopausal status, etc.; interaction tests; complete-case checks; and pooled MICE estimates.
- Proportional Hazards Assumption: Schoenfeld residuals test for Cox model validity.
Troubleshooting
- File Path Errors: Double-check the file_path variable to ensure it matches the location of your Excel dataset. Use forward slashes (/) or double backslashes (\\) in the path.
- Missing Packages: If you get "package not found" errors, uncomment and run the install.packages() lines at the top of the script.
- Memory Issues: If the script crashes, close other R sessions or reduce the number of MICE imputations (change m = 5 to m = 3 in the MICE section).
- Figure/Table Saving Errors: Ensure you have write permissions for the folder where the script is saved. Avoid special characters in the folder path.
Reference
Replication of: Joint Impact of Sleep and Physical Activity on Hypertension.
## DOIReplication materials are archived on Zenodo with DOI: 10.5281/zenodo.19597111Citation: wendy731. (2026). wendy731/CHARLS: First release for hypertension study replication materials (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.19597111
Data Source: CHARLS (China Health and Retirement Longitudinal Study) - Joint Impact CHARLS.xlsx (cleaned dataset, shared as part of replication materials).
Contact
For questions or issues with the replication code, please open an issue in this GitHub repository.
