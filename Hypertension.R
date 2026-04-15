# =============================================================================
# REPLICATION OF: Joint Impact of Sleep and Physical Activity on Hypertension
# Data: Joint Impact CHARLS.xlsx
# =============================================================================

# Install required packages (uncomment if needed)
# install.packages(c("readxl", "dplyr", "survival", "survminer", "table1", 
#                    "ggplot2", "rms", "car", "broom"))

# Load libraries
library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(table1)      # For nice baseline table
library(ggplot2)
library(rms)         # For restricted cubic splines (if continuous data available)
library(car)         # For ANOVA type tests
library(broom)       # For tidy model outputs

# ---------------------------
# 1. READ DATA
# ---------------------------
file_path <- "C:/Users/Administrator/Desktop/hypertension/Joint Impact CHARLS.xlsx"
df_raw <- read_excel(file_path)

# Create a copy
df <- df_raw
head(df)
describe(df)
tail(df)
# ---------------------------
# 2. DATA PREPARATION
# ---------------------------

# Parse Joint_Group into separate Sleep and PA factors
# Expected patterns: "Adequate Sleep / High PA", "Short Sleep / Low PA", etc.
df <- df %>%
  mutate(
    Sleep = ifelse(grepl("Adequate", Joint_Group), "Adequate", "Short"),
    PA = case_when(
      grepl("High", Joint_Group) ~ "High",
      grepl("Moderate", Joint_Group) ~ "Moderate",
      grepl("Low", Joint_Group) ~ "Low",
      TRUE ~ NA_character_
    ),
    # Convert to factors with explicit levels
    Sleep = factor(Sleep, levels = c("Adequate", "Short")),
    PA = factor(PA, levels = c("Low", "Moderate", "High")),  # Low as reference for independent PA model
    # Joint group as factor with reference = "Adequate Sleep / High PA"
    Joint_Group = factor(Joint_Group, 
                         levels = c("Adequate Sleep / High PA",
                                    "Adequate Sleep / Moderate PA",
                                    "Adequate Sleep / Low PA",
                                    "Short Sleep / High PA",
                                    "Short Sleep / Moderate PA",
                                    "Short Sleep / Low PA"))
  )

# Recode binary variables
df <- df %>%
  mutate(
    Residence_bin = ifelse(Residence == "Urban", 1, 0),
    Education_secondary = ifelse(Education %in% c("Secondary", "Secondary+"), 1, 0),
    Smoking_bin = ifelse(Smoking == "Yes", 1, 0),
    Drinking_bin = ifelse(Drinking == "Yes", 1, 0),
    Diabetes_bin = ifelse(Diabetes == "Yes", 1, 0),
    Kidney_bin = ifelse(Kidney_Dis == "Yes", 1, 0),
    Menopause_bin = ifelse(Menopause == "Yes", 1, 0),
    # For subgroup analyses
    Age_grp = ifelse(Age >= 60, ">=60", "<60"),
    BMI_grp = ifelse(BMI >= 24, ">=24", "<24")
  )

# ---------------------------
# 3. TABLE 1: BASELINE CHARACTERISTICS BY JOINT GROUP
# ---------------------------
# Define variables to summarize
vars <- c("Age", "BMI", "Residence", "Education_secondary", "Income", 
          "Smoking_bin", "Drinking_bin", "Diabetes_bin", "Kidney_bin",
          "Menopause_bin", "Cholesterol", "Glucose", "Baseline_SBP", "Baseline_DBP")

# Labels for table1
label(df$Age) <- "Age (years)"
label(df$BMI) <- "BMI (kg/m²)"
label(df$Residence) <- "Residence"
label(df$Education_secondary) <- "Secondary education or higher"
label(df$Income) <- "Annual household income (Yuan)"
label(df$Smoking_bin) <- "Current smoking"
label(df$Drinking_bin) <- "Current drinking"
label(df$Diabetes_bin) <- "Diabetes"
label(df$Kidney_bin) <- "Kidney disease"
label(df$Menopause_bin) <- "Post-menopausal"
label(df$Cholesterol) <- "Total cholesterol (mg/dL)"
label(df$Glucose) <- "Fasting plasma glucose (mmol/L)"
label(df$Baseline_SBP) <- "Baseline SBP (mmHg)"
label(df$Baseline_DBP) <- "Baseline DBP (mmHg)"

my_pvalue <- function(x, g, ...) {
  # x: variable, g: grouping variable (Joint_Group)
  if (is.numeric(x)) {
    p <- summary(aov(x ~ g))[[1]]$`Pr(>F)`[1]
  } else {
    tbl <- table(x, g)
    p <- chisq.test(tbl)$p.value
  }
  return(signif(p, 3))
}

table1_obj <- table1(~ Age + BMI + Residence + Education_secondary + Income + 
                       Smoking_bin + Drinking_bin + Diabetes_bin + Kidney_bin + 
                       Menopause_bin + Cholesterol + Glucose + Baseline_SBP + Baseline_DBP 
                     | Joint_Group, data = df, 
                     overall = "Overall", 
                     extra.col = list(`P-value` = my_pvalue))


# For ANOVA/chi-square p-values (replicating paper's Table 1), run separately:
# Continuous variables: ANOVA
cont_vars <- c("Age", "BMI", "Income", "Cholesterol", "Glucose", "Baseline_SBP", "Baseline_DBP")
p_cont <- sapply(cont_vars, function(v) {
  f <- as.formula(paste(v, "~ Joint_Group"))
  summary(aov(f, data = df))[[1]]$`Pr(>F)`[1]
})

# Categorical variables: chi-square
cat_vars <- c("Residence", "Education_secondary", "Smoking_bin", "Drinking_bin", 
              "Diabetes_bin", "Kidney_bin", "Menopause_bin")
p_cat <- sapply(cat_vars, function(v) {
  tbl <- table(df[[v]], df$Joint_Group)
  chisq.test(tbl)$p.value
})

# Combine p-values
p_table <- data.frame(Variable = c(cont_vars, cat_vars), 
                      P_value = round(c(p_cont, p_cat), 4))
print(p_table)

# ---------------------------
# 4. COX REGRESSION: INDEPENDENT EFFECTS OF SLEEP AND PA
# ---------------------------
# Model 1: Unadjusted
# Model 2: Age-adjusted
# Model 3: Fully adjusted (age, BMI, education, income, smoking, kidney, diabetes, menopause)

covariates_full <- c("Age", "BMI", "Education_secondary", "Income", 
                     "Smoking_bin", "Kidney_bin", "Diabetes_bin", "Menopause_bin")

# Independent effect of Sleep (short vs adequate)
# Using subset without PA adjustment? We'll adjust for PA as well to get independent effect.
# Paper says: "both behaviors were significant independent predictors" in fully adjusted model.
# So we include both in same model.

cox_sleep_pa <- coxph(Surv(FollowUp_Time, Incident_HTN) ~ Sleep + PA + 
                        Age + BMI + Education_secondary + Income + 
                        Smoking_bin + Kidney_bin + Diabetes_bin + Menopause_bin,
                      data = df)

summary(cox_sleep_pa)
# HR for Sleep (Short vs Adequate) = exp(coef) -> should be ~1.24
# HR for PA (Moderate vs Low, High vs Low)

# For independent effect of sleep alone (without PA adjustment) - as mentioned in paper's first paragraph:
cox_sleep_only <- coxph(Surv(FollowUp_Time, Incident_HTN) ~ Sleep + 
                          Age + BMI + Education_secondary + Income + 
                          Smoking_bin + Kidney_bin + Diabetes_bin + Menopause_bin,
                        data = df)
summary(cox_sleep_only)  # HR for Short sleep ~ 1.24

# Independent effect of PA alone (with sleep adjustment)
cox_pa_only <- coxph(Surv(FollowUp_Time, Incident_HTN) ~ PA + Sleep +
                       Age + BMI + Education_secondary + Income + 
                       Smoking_bin + Kidney_bin + Diabetes_bin + Menopause_bin,
                     data = df)
summary(cox_pa_only)  # HR for High vs Low PA ~ 0.81

# ---------------------------
# 5. JOINT EFFECTS (TABLE 2)
# ---------------------------
# Model 3 fully adjusted with Joint_Group (6 levels)
cox_joint <- coxph(Surv(FollowUp_Time, Incident_HTN) ~ Joint_Group + 
                     Age + BMI + Education_secondary + Income + 
                     Smoking_bin + Kidney_bin + Diabetes_bin + Menopause_bin,
                   data = df)

# Extract HR, CI, p-values
joint_results <- tidy(cox_joint, exponentiate = TRUE, conf.int = TRUE)
print(joint_results)

# Calculate incidence rates per 1000 person-years
incidence <- df %>%
  group_by(Joint_Group) %>%
  summarise(
    Events = sum(Incident_HTN),
    PY = sum(FollowUp_Time),
    Incidence_rate = (Events / PY) * 1000
  )
print(incidence)

# Merge with HR from model
table2 <- merge(incidence, joint_results[joint_results$term != "Joint_GroupAdequate Sleep / High PA", 
                                         c("term", "estimate", "conf.low", "conf.high", "p.value")],
                by.x = "Joint_Group", by.y = "term", all.x = TRUE)
print(table2)

# ---------------------------
# 6. FOREST PLOT FOR JOINT HRs (FIGURE 4)
# ---------------------------
# Prepare data for forest plot
hr_data <- joint_results %>% 
  filter(term != "Joint_GroupAdequate Sleep / High PA") %>%
  mutate(
    Group = gsub("Joint_Group", "", term),
    Group = factor(Group, levels = levels(df$Joint_Group)[-1])
  )

ggplot(hr_data, aes(x = estimate, y = Group)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
  geom_point(size = 3, color = "darkred") +
  scale_x_log10(name = "Hazard Ratio (95% CI)") +
  ylab("Joint Behavioral Group") +
  ggtitle("Forest Plot of Hazard Ratios (Model 3)") +
  theme_minimal()

# ---------------------------
# 7. KAPLAN-MEIER CURVES (FIGURE 5)
# ---------------------------
# Survival object
fit_km <- survfit(Surv(FollowUp_Time, Incident_HTN) ~ Joint_Group, data = df)

# Plot
ggsurvplot(fit_km, 
           data = df,
           risk.table = TRUE,
           pval = TRUE,  # Log-rank p-value
           pval.method = TRUE,
           conf.int = FALSE,
           xlab = "Follow-up time (years)",
           ylab = "Hypertension-free survival probability",
           legend.title = "Joint Group",
           legend.labs = levels(df$Joint_Group),
           palette = "hue",
           ggtheme = theme_minimal())

# Log-rank test explicitly
survdiff(Surv(FollowUp_Time, Incident_HTN) ~ Joint_Group, data = df)

# ---------------------------
# 8. RESTRICTED CUBIC SPLINES (RCS)
# NOTE: This requires continuous PA (MET-min/week) and sleep duration (hours).
# If you have those variables, uncomment and adapt the code below.
# ---------------------------
# Example if you have columns: `PA_continuous` (MET-min/week) and `Sleep_hours`
# df$PA_continuous <- ...   # add from your data
# df$Sleep_hours <- ...
# 
# # For PA
# dd <- datadist(df)
# options(datadist = "dd")
# cox_rcs_pa <- cph(Surv(FollowUp_Time, Incident_HTN) ~ rcs(PA_continuous, 3) + 
#                     Age + BMI + Education_secondary + Income + Smoking_bin + 
#                     Kidney_bin + Diabetes_bin + Menopause_bin,
#                   data = df, x = TRUE, y = TRUE)
# 
# # Plot
# ggplot(Predict(cox_rcs_pa, PA_continuous, fun = exp)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#   labs(y = "Hazard Ratio", x = "Physical activity (MET-min/week)") +
#   geom_hline(yintercept = 1, linetype = "dashed")
# 
# # Similarly for sleep duration (U-shaped)
# cox_rcs_sleep <- cph(Surv(FollowUp_Time, Incident_HTN) ~ rcs(Sleep_hours, 4) + 
#                        Age + BMI + Education_secondary + Income + Smoking_bin + 
#                        Kidney_bin + Diabetes_bin + Menopause_bin,
#                      data = df, x = TRUE, y = TRUE)

# ---------------------------
# 9. SUBGROUP AND SENSITIVITY ANALYSES (FIGURES 6, 7, 8)
# ---------------------------
# Define function to run joint model within a subset
run_subgroup <- function(data, subgroup_name) {
  cox_sub <- coxph(Surv(FollowUp_Time, Incident_HTN) ~ Joint_Group + 
                     Age + BMI + Education_secondary + Income + 
                     Smoking_bin + Kidney_bin + Diabetes_bin + Menopause_bin,
                   data = data)
  hr <- exp(coef(cox_sub)["Joint_GroupShort Sleep / Low PA"])
  ci <- exp(confint(cox_sub)["Joint_GroupShort Sleep / Low PA", ])
  return(c(HR = hr, lower = ci[1], upper = ci[2]))
}

# Subgroups
subgroups <- list(
  Age_lt60 = filter(df, Age < 60),
  Age_ge60 = filter(df, Age >= 60),
  BMI_lt24 = filter(df, BMI < 24),
  BMI_ge24 = filter(df, BMI >= 24),
  Pre_meno = filter(df, Menopause_bin == 0),
  Post_meno = filter(df, Menopause_bin == 1),
  Rural = filter(df, Residence == "Rural"),
  Urban = filter(df, Residence == "Urban"),
  No_DM = filter(df, Diabetes_bin == 0),
  With_DM = filter(df, Diabetes_bin == 1)
)

# Run for each
sub_results <- lapply(subgroups, run_subgroup)
sub_df <- do.call(rbind, sub_results)
sub_df <- as.data.frame(sub_df)
sub_df$Subgroup <- names(subgroups)
print(sub_df)

# Forest plot for subgroups (Figure 7)
ggplot(sub_df, aes(x = HR, y = Subgroup)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_point(size = 3, color = "blue") +
  scale_x_log10() +
  labs(x = "Hazard Ratio (Short Sleep/Low PA vs Reference)", 
       y = "Subgroup", 
       title = "Subgroup Analysis: High-Risk Behavioral Pattern") +
  theme_minimal()

# Interaction tests (Figure 8)
# Add interaction terms to full model
# Example for menopausal status
df$Menopause_bin <- as.numeric(df$Menopause_bin)
cox_int_meno <- coxph(Surv(FollowUp_Time, Incident_HTN) ~ Joint_Group * Menopause_bin + 
                        Age + BMI + Education_secondary + Income + Smoking_bin + 
                        Kidney_bin + Diabetes_bin,
                      data = df)
# Test interaction via likelihood ratio test
anova(cox_int_meno, test = "Chisq")  # Look at Joint_Group:Menopause_bin term

# Similarly for BMI group (categorical)
df$BMI_grp <- factor(df$BMI_grp, levels = c("<24", ">=24"))
cox_int_bmi <- coxph(Surv(FollowUp_Time, Incident_HTN) ~ Joint_Group * BMI_grp + 
                       Age + Education_secondary + Income + Smoking_bin + 
                       Kidney_bin + Diabetes_bin + Menopause_bin,
                     data = df)
anova(cox_int_bmi)

# ---------------------------
# 10. PROPORTIONAL HAZARDS ASSUMPTION CHECK
# ---------------------------
# Schoenfeld residuals for final joint model
test_ph <- cox.zph(cox_joint)
print(test_ph)
plot(test_ph)  # Should show non-significant trends (p > 0.05)

# ---------------------------
# END OF SCRIPT
# ---------------------------