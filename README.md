Joint Impact of Sleep and Physical Activity on Hypertension - Replication Code
Project Overview
This repository contains replication code for the study Joint Impact of Sleep and Physical Activity on Hypertension. The analysis uses data from the China Health and Retirement Longitudinal Study (CHARLS) to explore how sleep duration and physical activity (PA) jointly influence the risk of incident hypertension, alongside independent effects, subgroup analyses, and sensitivity checks.
File Structure
The repository includes the following files (upload all to your GitHub repository):
- Joint_Impact_Hypertension_Analysis.R: Full replication script (debugged version) with all analyses, figures, and tables.
- Original_Analysis_Script.R: Original unmodified script (for reference, contains deprecated functions and potential errors).
- README.md: This documentation file (guide to setup and run the code).
Prerequisites
1. Software
Ensure you have R (version ≥ 4.0.0) and RStudio installed (recommended for ease of use). Download R fromCRAN.
2. Required R Packages
The following packages are used in the replication script. The script includes code to install them (uncomment if needed):
# List of required packages
required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "survival", "survminer",
  "ggplot2", "rms", "mice", "broom", "gtsummary",
  "gt", "gridExtra", "scales", "lmtest", "car", "purrr",
  "flextable", "officer", "writexl"
)
Setup Instructions
1. Download Files
1. Clone this GitHub repository (or download all files manually).
2. Place all files in a dedicated folder (e.g., C:/Users/Administrator/Desktop/hypertension/).
2. Update File Path (Critical!)
In both R scripts, locate the file_path variable (under the "1. READ DATA" section) and update it to match the location of your Excel dataset:
How to Run the Analysis
Use the debugged script (Joint_Impact_Hypertension_Analysis.R) for full replication (recommended). The original script is provided for reference only.
1. Open Joint_Impact_Hypertension_Analysis.R in RStudio.
2. Run the script line-by-line (or use "Run All" Ctrl+Shift+Enter) to execute all analyses.
3. If prompted to install missing packages, uncomment theinstall.packages() lines at the top of the script and run them once.
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
1. Data Preparation: Recoding variables, creating sleep/PA subgroups, and handling missing data via MICE.
2. Baseline Characteristics (Table 1): Summary statistics by joint sleep-PA group, with ANOVA/chi-square p-values.
3. Independent Cox Regression: Effects of sleep (short vs. adequate) and physical activity (low/moderate/high) on hypertension risk.
4. Joint Effect Cox Regression (Table 2): Hazard ratios for incident hypertension across 6 joint sleep-PA groups (reference: Adequate Sleep / High PA).
5. Non-linear Analyses: Restricted cubic splines (RCS) for continuous sleep duration and physical activity.
6. Survival Analyses: Kaplan-Meier curves and log-rank tests.
7. Subgroup & Sensitivity Analyses: Subgroup analyses by age, BMI, menopausal status, etc.; interaction tests; complete-case checks; and pooled MICE estimates.
8. Proportional Hazards Assumption: Schoenfeld residuals test for Cox model validity.
Troubleshooting
- File Path Errors: Double-check the file_path variable to ensure it matches the location of your Excel dataset. Use forward slashes (/) or double backslashes (\\) in the path.
- Missing Packages: If you get "package not found" errors, uncomment and run the install.packages() lines at the top of the script.
- Memory Issues: If the script crashes, close other R sessions or reduce the number of MICE imputations (change m = 5 to m = 3 in the MICE section).
- Figure/Table Saving Errors: Ensure you have write permissions for the folder where the script is saved. Avoid special characters in the folder path.
Reference
Replication of: Joint Impact of Sleep and Physical Activity on Hypertension.

    Data Source: CHARLS (China Health and Retirement Longitudinal Study) - Joint Impact CHARLS.xlsx (cleaned dataset, shared as part of replication materials).
Contact
For questions or issues with the replication code, please open an issue in this GitHub repository.
