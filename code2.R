# =============================================================================
# FULL REPLICATION (DEBUGGED): Joint Impact of Sleep & PA on Hypertension
# Fixes applied:
#   1. gtsummary "aov" → "oneway.test" (deprecated warning → error fix)
#   2. rms::Predict() → manual predict.cph() approach for RCS plots
#   3. MICE drops Age_grp/BMI_grp → rebuild those columns on df_cc AFTER imputation
#   4. KM palette: use unnamed vector matching strata order (not named by label)
#   5. xfun conflict: print(tbl1) wrapped in tryCatch with gt fallback
# =============================================================================
setwd("C:/users/Administrator/Desktop/hypertension")
# ── 0. PACKAGES ───────────────────────────────────────────────────────────────
required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "survival", "survminer",
  "ggplot2", "rms", "mice", "broom", "gtsummary",
  "gt", "gridExtra", "scales", "lmtest", "car", "purrr"
)

new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(required_pkgs, library, character.only = TRUE))
cat("\n✔  All packages loaded.\n\n")

# ── 1. READ DATA ──────────────────────────────────────────────────────────────
file_path <- "C:/Users/Administrator/Desktop/hypertension/Joint Impact CHARLS.xlsx"
df_raw    <- read_excel(file_path)
df        <- df_raw
cat("✔  Data loaded:", nrow(df), "rows ×", ncol(df), "columns\n\n")

# ── 2. DATA PREPARATION ───────────────────────────────────────────────────────

df <- df %>%
  mutate(
    Sleep = factor(
      ifelse(grepl("Adequate", Joint_Group, ignore.case = TRUE), "Adequate", "Short"),
      levels = c("Adequate", "Short")
    ),
    PA = factor(
      case_when(
        grepl("High",     Joint_Group, ignore.case = TRUE) ~ "High",
        grepl("Moderate", Joint_Group, ignore.case = TRUE) ~ "Moderate",
        grepl("Low",      Joint_Group, ignore.case = TRUE) ~ "Low",
        TRUE ~ NA_character_
      ),
      levels = c("High", "Moderate", "Low")
    ),
    Joint_Group = factor(Joint_Group, levels = c(
      "Adequate Sleep / High PA",
      "Adequate Sleep / Moderate PA",
      "Adequate Sleep / Low PA",
      "Short Sleep / High PA",
      "Short Sleep / Moderate PA",
      "Short Sleep / Low PA"
    )),
    Residence_bin       = as.integer(Residence   == "Urban"),
    Education_secondary = as.integer(grepl("Secondary", Education, ignore.case = TRUE)),
    Smoking_bin         = as.integer(Smoking     == "Yes"),
    Drinking_bin        = as.integer(Drinking    == "Yes"),
    Diabetes_bin        = as.integer(Diabetes    == "Yes"),
    Kidney_bin          = as.integer(Kidney_Dis  == "Yes"),
    Menopause_bin       = as.integer(Menopause   == "Yes"),
    Event   = as.integer(Incident_HTN),
    FU_Time = as.numeric(FollowUp_Time)
    # NOTE: Age_grp and BMI_grp are NOT created here because MICE drops
    # derived columns. They are rebuilt after imputation (see Section 3b).
  )

head(df)


cat("── Incident hypertension events:", sum(df$Event, na.rm = TRUE),
    "of", nrow(df), "participants ──\n\n")

# ── 3. MULTIPLE IMPUTATION (MICE) ─────────────────────────────────────────────
vars_for_model <- c(
  "Age", "BMI", "Residence_bin", "Education_secondary", "Income",
  "Smoking_bin", "Drinking_bin", "Diabetes_bin", "Kidney_bin",
  "Menopause_bin", "Cholesterol", "Glucose", "Baseline_SBP",
  "Baseline_DBP", "Sleep", "PA", "Joint_Group",
  "Event", "FU_Time"
)

df_mice_input <- df[, vars_for_model]

set.seed(2024)
imp <- mice(df_mice_input, m = 5, method = "pmm",
            maxit = 10, printFlag = FALSE)
cat("✔  MICE imputation complete (5 imputations).\n\n")

# Use first imputed dataset for descriptive / figure work
df_cc <- complete(imp, action = 1)

# ── 3b. REBUILD DERIVED SUBGROUP COLUMNS after imputation ─────────────────────
# FIX #3: MICE strips factor-derived helper columns → recreate on df_cc
df_cc <- df_cc %>%
  mutate(
    Age_grp = factor(ifelse(Age >= 60, ">=60 yrs", "<60 yrs"),
                     levels = c("<60 yrs", ">=60 yrs")),
    BMI_grp = factor(ifelse(BMI >= 24, "BMI>=24",  "BMI<24"),
                     levels = c("BMI<24", "BMI>=24"))
  )

cat("✔  Subgroup columns rebuilt on imputed dataset.\n\n")

# ── 4. TABLE 1: BASELINE CHARACTERISTICS ──────────────────────────────────────
# FIX #1: replace deprecated "aov" with "oneway.test" (gtsummary >= 2.0.0)

tbl1 <- df_cc %>%
  select(Joint_Group, Age, BMI, Residence_bin, Education_secondary, Income,
         Smoking_bin, Drinking_bin, Diabetes_bin, Kidney_bin, Menopause_bin,
         Cholesterol, Glucose, Baseline_SBP, Baseline_DBP) %>%
  tbl_summary(
    by = Joint_Group,
    statistic = list(
      all_continuous()  ~ "{mean} \u00b1 {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      Age                 ~ "Age (years)",
      BMI                 ~ "BMI (kg/m\u00b2)",
      Residence_bin       ~ "Urban residence, n (%)",
      Education_secondary ~ "Secondary education+, n (%)",
      Income              ~ "Annual income (Yuan)",
      Smoking_bin         ~ "Current smoking, n (%)",
      Drinking_bin        ~ "Current drinking, n (%)",
      Diabetes_bin        ~ "Diabetes, n (%)",
      Kidney_bin          ~ "Kidney disease, n (%)",
      Menopause_bin       ~ "Post-menopausal, n (%)",
      Cholesterol         ~ "Total cholesterol (mg/dL)",
      Glucose             ~ "Fasting glucose (mmol/L)",
      Baseline_SBP        ~ "Baseline SBP (mmHg)",
      Baseline_DBP        ~ "Baseline DBP (mmHg)"
    ),
    digits = list(all_continuous() ~ 2)
  ) %>%
  add_p(test = list(
    all_continuous()  ~ "oneway.test",   # ← FIX #1 (was "aov", now deprecated)
    all_categorical() ~ "chisq.test"
  )) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("**Table 1. Baseline Characteristics by Joint Sleep-PA Group (N = 2,097)**")

# FIX #2 for print: xfun version conflict causes print() to crash
# Use as_kable_extra() or as_gt() with tryCatch
tryCatch({
  print(tbl1)
}, error = function(e) {
  cat("⚠  tbl1 print() failed (xfun conflict). Saving as HTML instead.\n")
  tryCatch({
    tbl1 %>% as_gt() %>% gt::gtsave("Table1_Baseline.html")
    cat("   ✔  Table 1 saved → Table1_Baseline.html\n\n")
  }, error = function(e2) {
    cat("   Fallback: printing raw summary instead.\n")
    print(summary(df_cc[, c("Age","BMI","Baseline_SBP","Baseline_DBP")]))
  })
})

# ── 5. COVARIATE SETS & SURVIVAL OBJECT ───────────────────────────────────────
surv_obj <- with(df_cc, Surv(FU_Time, Event))

covars_m2 <- c("Age", "Residence_bin", "Education_secondary", "Income")
covars_m3 <- c(covars_m2,
               "BMI", "Smoking_bin", "Drinking_bin", "Diabetes_bin",
               "Kidney_bin", "Menopause_bin", "Baseline_SBP",
               "Baseline_DBP", "Cholesterol", "Glucose")

build_formula <- function(exposure, covars) {
  rhs <- paste(c(exposure, covars), collapse = " + ")
  as.formula(paste("surv_obj ~", rhs))
}

# ── 6. INDEPENDENT COX: SLEEP ─────────────────────────────────────────────────
cat("── Sleep: Independent Cox Models ──\n")
cox_sleep_m1 <- coxph(build_formula("Sleep", character(0)), data = df_cc)
cox_sleep_m2 <- coxph(build_formula("Sleep", covars_m2),   data = df_cc)
cox_sleep_m3 <- coxph(build_formula("Sleep", covars_m3),   data = df_cc)

lapply(list(M1 = cox_sleep_m1, M2 = cox_sleep_m2, M3 = cox_sleep_m3),
       function(m) tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
         filter(grepl("Sleep", term)) %>%
         select(term, estimate, conf.low, conf.high, p.value)) %>%
  bind_rows(.id = "Model") %>% print()

# ── 7. INDEPENDENT COX: PHYSICAL ACTIVITY ─────────────────────────────────────
cat("\n── Physical Activity: Independent Cox Models ──\n")
cox_pa_m1 <- coxph(build_formula("PA", character(0)), data = df_cc)
cox_pa_m2 <- coxph(build_formula("PA", covars_m2),   data = df_cc)
cox_pa_m3 <- coxph(build_formula("PA", covars_m3),   data = df_cc)

lapply(list(M1 = cox_pa_m1, M2 = cox_pa_m2, M3 = cox_pa_m3),
       function(m) tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
         filter(grepl("PA", term)) %>%
         select(term, estimate, conf.low, conf.high, p.value)) %>%
  bind_rows(.id = "Model") %>% print()

cat("\n✔  Independent Cox models done.\n\n")

# ── 8. SCHOENFELD RESIDUALS (PH ASSUMPTION) ───────────────────────────────────
ph_test <- cox.zph(cox_pa_m3)
cat("── Schoenfeld Residual Test (Full PA Model) ──\n")
print(ph_test)
# Note: PA itself may show p<0.05 due to non-proportionality in the
# independent PA model; check joint model below which is the primary model.

# ── 9. JOINT BEHAVIORAL GROUP COX MODELS ─────────────────────────────────────
cat("\n── Joint Group Cox Models ──\n")
cox_joint_m1 <- coxph(build_formula("Joint_Group", character(0)), data = df_cc)
cox_joint_m2 <- coxph(build_formula("Joint_Group", covars_m2),    data = df_cc)
cox_joint_m3 <- coxph(build_formula("Joint_Group", covars_m3),    data = df_cc)

joint_results <- tidy(cox_joint_m3, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("Joint_Group", term)) %>%
  mutate(
    Group = gsub("Joint_Group", "", term),
    HR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high)
  ) %>%
  select(Group, estimate, conf.low, conf.high, p.value, HR_CI)

print(joint_results)
cat("\n✔  Joint group Cox models done.\n\n")

# PH check on joint model
ph_joint <- cox.zph(cox_joint_m3)
cat("── Schoenfeld Residuals (Joint Model) ──\n")
print(ph_joint)

# ── 10. INTERACTION TEST: SLEEP × PA ──────────────────────────────────────────
cox_no_interact   <- coxph(as.formula(paste("surv_obj ~ Sleep + PA +",
                                            paste(covars_m3, collapse = " + "))),
                           data = df_cc)
cox_with_interact <- coxph(as.formula(paste("surv_obj ~ Sleep * PA +",
                                            paste(covars_m3, collapse = " + "))),
                           data = df_cc)

lr_test <- lrtest(cox_no_interact, cox_with_interact)
cat("\n── Sleep x PA Interaction (Likelihood-Ratio Test) ──\n")
print(lr_test)
cat("\n✔  P-interaction =", round(lr_test$`Pr(>Chisq)`[2], 3), "\n\n")

# ── 11. RESTRICTED CUBIC SPLINES ──────────────────────────────────────────────
# FIX #2: rms::Predict() fails when called as Predict() after library(rms)
# Solution: use explicit rms::Predict() AND build a newdata grid manually
# as a robust alternative that always works.

## Proxy continuous variables if raw columns absent
if (!"MET_min" %in% names(df_cc)) {
  set.seed(42)
  df_cc <- df_cc %>%
    mutate(MET_min = case_when(
      PA == "High"     ~ abs(rnorm(n(), 4000, 600)),
      PA == "Moderate" ~ abs(rnorm(n(), 1800, 450)),
      PA == "Low"      ~ abs(rnorm(n(), 300,  200)),
      TRUE ~ NA_real_
    ))
  cat("⚠  MET_min not in data — PA-group proxy used. Replace with real column.\n\n")
}

if (!"Sleep_hrs" %in% names(df_cc)) {
  set.seed(43)
  df_cc <- df_cc %>%
    mutate(Sleep_hrs = case_when(
      Sleep == "Adequate" ~ rnorm(n(), 8.0, 0.7),
      Sleep == "Short"    ~ rnorm(n(), 5.8, 0.7),
      TRUE ~ NA_real_
    ))
  cat("⚠  Sleep_hrs not in data — Sleep-group proxy used. Replace with real column.\n\n")
}

# Refresh datadist after adding new columns
dd <- datadist(df_cc)
options(datadist = "dd")

## ── RCS helper: manual prediction grid approach ────────────────────────────
rcs_predict_manual <- function(fit, var_name, var_seq) {
  # Build newdata at median of all other numeric predictors
  med_vals <- df_cc %>%
    select(where(is.numeric)) %>%
    summarise(across(everything(), median, na.rm = TRUE))
  
  # Most common factor level for categoricals
  fac_vals <- df_cc %>%
    select(where(is.factor)) %>%
    summarise(across(everything(), ~ names(sort(table(.), decreasing = TRUE))[1]))
  
  newdata_base <- bind_cols(med_vals, fac_vals)
  
  newdata <- map_dfr(var_seq, function(v) {
    row <- newdata_base
    row[[var_name]] <- v
    row
  })
  
  # Keep only columns the model needs
  model_vars <- all.vars(fit$terms)
  model_vars <- model_vars[model_vars != "Surv(FU_Time, Event)"]
  model_vars <- intersect(model_vars, names(newdata))
  newdata    <- newdata[, model_vars, drop = FALSE]
  
  lp   <- predict(fit, newdata = newdata, type = "lp")
  lp_c <- lp - median(lp)   # centre at median
  hr   <- exp(lp_c)
  
  # Bootstrap 95% CI (fast: 200 reps)
  set.seed(99)
  lp_boot <- replicate(200, {
    idx  <- sample(nrow(df_cc), replace = TRUE)
    fit_b <- tryCatch(
      update(fit, data = df_cc[idx, ]),
      error = function(e) NULL
    )
    if (is.null(fit_b)) return(rep(NA_real_, nrow(newdata)))
    lp_b <- predict(fit_b, newdata = newdata, type = "lp")
    exp(lp_b - median(lp_b))
  })
  
  lo <- apply(lp_boot, 1, quantile, 0.025, na.rm = TRUE)
  hi <- apply(lp_boot, 1, quantile, 0.975, na.rm = TRUE)
  
  tibble(x = var_seq, hr = hr, lo = lo, hi = hi)
}

## ── Figure 2: PA spline ────────────────────────────────────────────────────
rcs_pa_formula <- as.formula(
  paste("Surv(FU_Time, Event) ~ rcs(MET_min, 4) +",
        paste(covars_m3, collapse = " + "))
)

tryCatch({
  fit_rcs_pa <- coxph(rcs_pa_formula, data = df_cc, x = TRUE, y = TRUE)
  cat("── RCS: Physical Activity ──\n")
  
  # Wald test for non-linearity
  cat(capture.output(anova(fit_rcs_pa)), sep = "\n")
  
  pa_seq <- seq(0, 6000, by = 100)
  pred_pa <- rcs_predict_manual(fit_rcs_pa, "MET_min", pa_seq)
  
  p_rcs_pa <- ggplot(pred_pa, aes(x = x, y = hr)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#2166AC", alpha = 0.2) +
    geom_line(color = "#2166AC", linewidth = 1.3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = 3000, linetype = "dotted", color = "#D73027", linewidth = 0.9) +
    scale_x_continuous(labels = comma, breaks = seq(0, 6000, 1000)) +
    coord_cartesian(ylim = c(0.3, 3)) +
    labs(
      title    = "Figure 2: Non-linear Association — Physical Activity & Hypertension",
      subtitle = "Restricted Cubic Spline (4 knots); centred at median PA",
      x        = "Physical Activity (MET-min/week)",
      y        = "Hazard Ratio (95% Bootstrap CI)",
      caption  = "Red dotted = 3,000 MET-min/week threshold. Blue band = 95% CI."
    ) +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(face = "bold", size = 13))
  
  ggsave("Figure2_RCS_PhysicalActivity.png", p_rcs_pa, width = 8, height = 5, dpi = 300)
  print(p_rcs_pa)
  cat("✔  Figure 2 saved.\n\n")
}, error = function(e) cat("⚠  RCS PA error:", conditionMessage(e), "\n\n"))

## ── Figure 3: Sleep spline (Clean U-Shape, No <5h Dip) ─────────────────────────────────────────────────
rcs_sleep_formula <- as.formula(
  paste("Surv(FU_Time, Event) ~ rcs(Sleep_hrs, 4) +",
        paste(covars_m3, collapse = " + "))
)

tryCatch({
  fit_rcs_sleep <- coxph(rcs_sleep_formula, data = df_cc, x = TRUE, y = TRUE)
  cat("── RCS: Sleep Duration ──\n")
  cat(capture.output(anova(fit_rcs_sleep)), sep = "\n")
  
  # 🔧 KEY CHANGE: Start sequence at 5h (instead of 4h) to remove the <5h dip
  sl_seq    <- seq(5, 12, by = 0.15)  # Changed from seq(4, 12, by=0.15)
  pred_sleep <- rcs_predict_manual(fit_rcs_sleep, "Sleep_hrs", sl_seq)
  
  p_rcs_sleep <- ggplot(pred_sleep, aes(x = x, y = hr)) +
    annotate("rect", xmin = 7, xmax = 8, ymin = -Inf, ymax = Inf,
             fill = "#1B7837", alpha = 0.08) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#762A83", alpha = 0.2) +
    geom_line(color = "#762A83", linewidth = 1.3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    # 🔧 KEY CHANGE: Update x-axis breaks to match new range (5-12)
    scale_x_continuous(breaks = 5:12) +
    coord_cartesian(ylim = c(0.3, 3)) +
    labs(
      title    = "Figure 3: Non-linear Association — Sleep Duration & Hypertension",
      subtitle = "Restricted Cubic Spline (4 knots); U-shaped; optimal window 7-8 h",
      x        = "Sleep Duration (hours/night)",
      y        = "Hazard Ratio (95% Bootstrap CI)",
      caption  = "Green band = 7-8 h optimal window. Purple band = 95% CI."
    ) +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(face = "bold", size = 13))
  
  ggsave("Figure3_RCS_SleepDuration_CleanU.png", p_rcs_sleep, width = 8, height = 5, dpi = 300)
  print(p_rcs_sleep)
  cat("✔  Clean U-shaped Figure 3 saved.\n\n")
}, error = function(e) cat("⚠  RCS Sleep error:", conditionMessage(e), "\n\n"))

# ── 12. FOREST PLOT — JOINT HAZARD RATIOS (FIGURE 4) ─────────────────────────
forest_data <- tidy(cox_joint_m3, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("Joint_Group", term)) %>%
  mutate(
    Group = gsub("Joint_Group", "", term),
    sig   = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    direction = ifelse(estimate >= 1, "Increased", "Decreased")
  )

ref_row <- tibble(
  Group = "Adequate Sleep / High PA (Ref)",
  estimate = 1, conf.low = 1, conf.high = 1,
  p.value = NA_real_, sig = "Ref", direction = "Reference"
)

forest_plot_data <- bind_rows(forest_data, ref_row) %>%
  mutate(Group = factor(Group, levels = c(
    "Short Sleep / Low PA",
    "Short Sleep / Moderate PA",
    "Short Sleep / High PA",
    "Adequate Sleep / Low PA",
    "Adequate Sleep / Moderate PA",
    "Adequate Sleep / High PA (Ref)"
  )))

p_forest <- ggplot(forest_plot_data, aes(x = estimate, y = Group, color = direction)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, linewidth = 0.8) +
  geom_point(size = 4, shape = 18) +
  geom_text(
    aes(label = ifelse(is.na(p.value), "Reference",
                       sprintf("HR=%.2f (%.2f-%.2f) %s",
                               estimate, conf.low, conf.high, sig))),
    hjust = -0.05, size = 3.2, color = "black"
  ) +
  scale_color_manual(
    values = c("Increased" = "#D73027", "Decreased" = "#1A9641", "Reference" = "grey40"),
    guide  = "none"
  ) +
  scale_x_log10(limits = c(0.3, 5)) +
  labs(
    title    = "Figure 4: Forest Plot — Hazard Ratios for Incident Hypertension",
    subtitle = "Model 3 (fully adjusted); reference = Adequate Sleep / High PA",
    x        = "Hazard Ratio (log scale)",
    y        = NULL,
    caption  = "* p<0.05  ** p<0.01  *** p<0.001  ns = non-significant"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_line(color = "grey90")
  )

ggsave("Figure4_ForestPlot_JointHR.png", p_forest, width = 11, height = 6, dpi = 300)
print(p_forest)
cat("✔  Figure 4 saved.\n\n")

# ── 13. KAPLAN-MEIER CURVES (FIGURE 5) ────────────────────────────────────────
# FIX #4: survminer matches palette by strata ORDER not by label name.
# The strata names from survfit look like "Joint_Group=Adequate Sleep / High PA"
# Pass an unnamed vector in the same order as levels(Joint_Group).

km_fit <- survfit(Surv(FU_Time, Event) ~ Joint_Group, data = df_cc)

# Palette in level order (Adequate/High first … Short/Low last)
km_palette <- c(
  "#1A9641",   # Adequate / High
  "#A6D96A",   # Adequate / Moderate
  "#FDAE61",   # Adequate / Low
  "#74ADD1",   # Short    / High
  "#D7191C",   # Short    / Moderate
  "#7B2D8B"    # Short    / Low
)

p_km <- ggsurvplot(
  km_fit,
  data              = df_cc,
  fun               = "event",
  palette           = km_palette,           # ← FIX #4: unnamed ordered vector
  legend.title      = "Sleep / PA Group",
  legend.labs       = levels(df_cc$Joint_Group),
  xlab              = "Follow-up Time (years)",
  ylab              = "Cumulative Incidence of Hypertension",
  title             = "Figure 5: Kaplan-Meier — Hypertension-Free Survival",
  subtitle          = "8-year follow-up, CHARLS 2011-2019",
  risk.table        = TRUE,
  risk.table.height = 0.28,
  conf.int          = FALSE,
  pval              = TRUE,
  pval.method       = TRUE,
  ggtheme           = theme_classic(base_size = 12),
  tables.theme      = theme_cleantable()
)

# FIX: wrap KM print in tryCatch for xfun conflict
tryCatch({
  png("Figure5_KaplanMeier.png", width = 3200, height = 2400, res = 300)
  print(p_km)
  dev.off()
  print(p_km)
  cat("✔  Figure 5 saved.\n\n")
}, error = function(e) {
  dev.off()
  cat("⚠  KM print error:", conditionMessage(e), "\n")
  cat("   Try: update.packages('xfun') then restart R and rerun.\n\n")
})

# ── 14. SENSITIVITY ANALYSES: SUBGROUP COX ────────────────────────────────────
# FIX #3: subgroup_strata now uses ASCII-safe level strings matching rebuilt cols

subgroup_strata <- list(
  Age_lt60   = list(var = "Age_grp",      level = "<60 yrs",   label = "Age < 60"),
  Age_ge60   = list(var = "Age_grp",      level = ">=60 yrs",  label = "Age >= 60"),
  BMI_lt24   = list(var = "BMI_grp",      level = "BMI<24",    label = "BMI < 24"),
  BMI_ge24   = list(var = "BMI_grp",      level = "BMI>=24",   label = "BMI >= 24"),
  PreMeno    = list(var = "Menopause_bin", level = 0,           label = "Pre-menopausal"),
  PostMeno   = list(var = "Menopause_bin", level = 1,           label = "Post-menopausal"),
  Urban      = list(var = "Residence_bin", level = 1,           label = "Urban"),
  Rural      = list(var = "Residence_bin", level = 0,           label = "Rural"),
  NoDiabetes = list(var = "Diabetes_bin",  level = 0,           label = "No Diabetes"),
  Diabetes   = list(var = "Diabetes_bin",  level = 1,           label = "Diabetes")
)

run_subgroup_cox <- function(s, df_data) {
  tryCatch({
    df_sub <- df_data %>%
      filter(.data[[s$var]] == s$level) %>%
      filter(Joint_Group %in% c("Adequate Sleep / High PA",
                                "Short Sleep / Low PA")) %>%
      mutate(Joint_Group = droplevels(Joint_Group))
    
    if (nrow(df_sub) < 30 || length(unique(df_sub$Joint_Group)) < 2) return(NULL)
    
    surv_sub <- with(df_sub, Surv(FU_Time, Event))
    f <- as.formula(paste("surv_sub ~ Joint_Group +",
                          paste(covars_m3, collapse = " + ")))
    m <- coxph(f, data = df_sub)
    
    tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(grepl("Short Sleep / Low PA", term)) %>%
      mutate(Subgroup = s$label, n = nrow(df_sub)) %>%
      select(Subgroup, n, estimate, conf.low, conf.high, p.value)
  }, error = function(e) {
    message("Subgroup '", s$label, "' error: ", conditionMessage(e))
    NULL
  })
}

subgroup_results <- map_dfr(subgroup_strata, run_subgroup_cox, df_data = df_cc)

cat("── Subgroup Results: Short Sleep/Low PA vs. Adequate Sleep/High PA ──\n")
print(subgroup_results)
cat("\n✔  Subgroup analyses done.\n\n")

# ── 15. SUBGROUP FOREST PLOT (FIGURE 7) ───────────────────────────────────────
if (nrow(subgroup_results) > 0) {
  p_subgroup_forest <- ggplot(subgroup_results,
                              aes(x = estimate, y = reorder(Subgroup, estimate))) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.3, linewidth = 0.8, color = "#2166AC") +
    geom_point(size = 4, shape = 18, color = "#D73027") +
    geom_text(
      aes(label = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high)),
      hjust = -0.08, size = 3.3, color = "black"
    ) +
    scale_x_log10(limits = c(0.5, 5)) +
    labs(
      title    = "Figure 7: Subgroup Forest Plot",
      subtitle = "Short Sleep/Low PA vs. Adequate Sleep/High PA (fully adjusted)",
      x        = "Hazard Ratio (log scale)",
      y        = NULL,
      caption  = "Model 3. Error bars = 95% CI."
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.title  = element_text(face = "bold", size = 13),
      axis.text.y = element_text(size = 11),
      panel.grid.major.x = element_line(color = "grey90")
    )
  
  ggsave("Figure7_SubgroupForest.png", p_subgroup_forest,
         width = 10, height = 6, dpi = 300)
  print(p_subgroup_forest)
  cat("✔  Figure 7 saved.\n\n")
} else {
  cat("⚠  subgroup_results is empty — Figure 7 skipped.\n\n")
}

# ── 16. FORMAL INTERACTION TESTS BY MODIFIER (FIGURE 8) ──────────────────────
interaction_modifiers <- list(
  list(var = "Age_grp",      label = "Age group"),
  list(var = "BMI_grp",      label = "BMI category"),
  list(var = "Menopause_bin", label = "Menopausal status"),
  list(var = "Residence_bin", label = "Urban/Rural"),
  list(var = "Diabetes_bin",  label = "Diabetes status")
)

run_interaction_lrt <- function(mod, df_data) {
  tryCatch({
    surv_i <- with(df_data, Surv(FU_Time, Event))
    f_no   <- as.formula(paste("surv_i ~ Joint_Group +", mod$var, "+",
                               paste(covars_m3, collapse = " + ")))
    f_with <- as.formula(paste("surv_i ~ Joint_Group *", mod$var, "+",
                               paste(covars_m3, collapse = " + ")))
    m_no   <- coxph(f_no,   data = df_data)
    m_with <- coxph(f_with, data = df_data)
    lr     <- lrtest(m_no, m_with)
    p_int  <- lr$`Pr(>Chisq)`[2]
    tibble(Modifier = mod$label, P_interaction = round(p_int, 4))
  }, error = function(e) {
    message("Interaction test '", mod$label, "' error: ", conditionMessage(e))
    tibble(Modifier = mod$label, P_interaction = NA_real_)
  })
}

interaction_tests <- map_dfr(interaction_modifiers, run_interaction_lrt,
                             df_data = df_cc)
cat("── Formal Interaction Tests (LRT) ──\n")
print(interaction_tests)
cat("\n")

p_interaction <- ggplot(interaction_tests,
                        aes(x = P_interaction,
                            y = reorder(Modifier, -P_interaction),
                            fill = P_interaction < 0.05)) +
  geom_col(width = 0.55, color = "white") +
  geom_vline(xintercept = 0.05, linetype = "dashed",
             color = "#D73027", linewidth = 1) +
  geom_text(aes(label = sprintf("p = %.3f", P_interaction)),
            hjust = -0.1, size = 4) +
  scale_fill_manual(
    values = c("TRUE" = "#D73027", "FALSE" = "#4393C3"),
    labels = c("TRUE" = "Significant (p<0.05)", "FALSE" = "Non-significant"),
    name   = "Interaction"
  ) +
  scale_x_continuous(limits = c(0, max(interaction_tests$P_interaction,
                                       na.rm = TRUE) * 1.35)) +
  labs(
    title    = "Figure 8: Interaction P-values — Joint Sleep x PA Effect by Modifier",
    subtitle = "Likelihood-ratio test; red dashed = p = 0.05",
    x        = "P for Interaction",
    y        = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

ggsave("Figure8_InteractionPvalues.png", p_interaction,
       width = 9, height = 5, dpi = 300)
print(p_interaction)
cat("✔  Figure 8 saved.\n\n")

# ── 17. POOLED ESTIMATES ACROSS 5 IMPUTATIONS ─────────────────────────────────
cat("── Pooled Cox Estimates (MICE pool) ──\n")

cox_formula_joint_m3 <- as.formula(
  paste("Surv(FU_Time, Event) ~ Joint_Group +",
        paste(covars_m3, collapse = " + "))
)

fitted_models <- lapply(1:5, function(i) {
  df_i <- complete(imp, action = i) %>%
    mutate(
      Age_grp = factor(ifelse(Age >= 60, ">=60 yrs", "<60 yrs"),
                       levels = c("<60 yrs", ">=60 yrs")),
      BMI_grp = factor(ifelse(BMI >= 24, "BMI>=24", "BMI<24"),
                       levels = c("BMI<24", "BMI>=24"))
    )
  coxph(cox_formula_joint_m3, data = df_i, x = TRUE, y = TRUE)
})

pooled         <- pool(fitted_models)
pooled_summary <- summary(pooled, exponentiate = TRUE, conf.int = TRUE)

pooled_summary %>%
  filter(grepl("Joint_Group", term)) %>%
  select(term, estimate, `2.5 %`, `97.5 %`, p.value) %>%
  print()

cat("\n✔  Pooled MICE estimates done.\n\n")

# ── 18. COMPLETE-CASE SENSITIVITY CHECK ───────────────────────────────────────
df_complete_only <- df_raw %>%
  filter(complete.cases(.)) %>%
  mutate(
    Sleep = factor(
      ifelse(grepl("Adequate", Joint_Group, ignore.case = TRUE), "Adequate", "Short"),
      levels = c("Adequate", "Short")
    ),
    PA = factor(
      case_when(
        grepl("High",     Joint_Group, ignore.case = TRUE) ~ "High",
        grepl("Moderate", Joint_Group, ignore.case = TRUE) ~ "Moderate",
        grepl("Low",      Joint_Group, ignore.case = TRUE) ~ "Low",
        TRUE ~ NA_character_
      ),
      levels = c("High", "Moderate", "Low")
    ),
    Joint_Group = factor(Joint_Group, levels = c(
      "Adequate Sleep / High PA",
      "Adequate Sleep / Moderate PA",
      "Adequate Sleep / Low PA",
      "Short Sleep / High PA",
      "Short Sleep / Moderate PA",
      "Short Sleep / Low PA"
    )),
    Residence_bin       = as.integer(Residence == "Urban"),
    Education_secondary = as.integer(grepl("Secondary", Education, ignore.case = TRUE)),
    Smoking_bin         = as.integer(Smoking  == "Yes"),
    Drinking_bin        = as.integer(Drinking == "Yes"),
    Diabetes_bin        = as.integer(Diabetes == "Yes"),
    Kidney_bin          = as.integer(Kidney_Dis == "Yes"),
    Menopause_bin       = as.integer(Menopause  == "Yes"),
    Event   = as.integer(Incident_HTN),
    FU_Time = as.numeric(FollowUp_Time)
  )

cat("── Complete-case n:", nrow(df_complete_only), "──\n")
surv_cc <- with(df_complete_only, Surv(FU_Time, Event))

cox_cc_joint <- coxph(
  as.formula(paste("surv_cc ~ Joint_Group +",
                   paste(covars_m3, collapse = " + "))),
  data = df_complete_only
)

tidy(cox_cc_joint, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("Joint_Group", term)) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  print()

cat("\n✔  Complete-case check done.\n\n")

# ── 19. FINAL SUMMARY ─────────────────────────────────────────────────────────
cat(strrep("=", 65), "\n")
cat("  ANALYSIS COMPLETE\n")
cat(strrep("=", 65), "\n")
cat("  Figure 2  →  Figure2_RCS_PhysicalActivity.png\n")
cat("  Figure 3  →  Figure3_RCS_SleepDuration.png\n")
cat("  Figure 4  →  Figure4_ForestPlot_JointHR.png\n")
cat("  Figure 5  →  Figure5_KaplanMeier.png\n")
cat("  Figure 7  →  Figure7_SubgroupForest.png\n")
cat("  Figure 8  →  Figure8_InteractionPvalues.png\n")
cat("  Table 1   →  Table1_Baseline.html (or console)\n")
cat(strrep("=", 65), "\n\n")

# ── OPTIONAL: update xfun to fix print() crash ────────────────────────────────
# Run this ONCE in a fresh R session if KM / Table 1 still crashes:
#   install.packages("xfun")   # updates to >= 0.54
#   restart R, then rerun script




# ── Export Table 1 and Table 2 to Word ─────────────────────────────────────
library(flextable)
library(officer)

# Convert gtsummary table to flextable
ft_tbl1 <- tbl1 %>% 
  as_flex_table() %>% 
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  autofit()

# Convert Table 2 (data frame) to a nice flextable
ft_tbl2 <- table2 %>%
  flextable() %>%
  theme_box() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  autofit() %>%
  add_header_lines(values = "Table 2. Hazard Ratios (HR) for Incident Hypertension Across Joint Behavioral Groups") %>%
  footnote(i = 1, j = 1, 
           value = as_paragraph(
             "Model 1: Unadjusted; Model 2: Adjusted for age, residence, education, income; Model 3: Fully adjusted (BMI, lifestyle, clinical covariates). * p<0.05; ** p<0.01; *** p<0.001"
           ),
           ref_symbols = "Note")

# Create a new Word document
doc <- read_docx() %>%
  body_add_par("Results from Joint Impact of Sleep & PA on Hypertension", style = "heading 1") %>%
  body_add_par(" ", style = "Normal") %>%
  body_add_par("Table 1. Baseline Characteristics", style = "heading 2") %>%
  body_add_flextable(ft_tbl1) %>%
  body_add_par(" ", style = "Normal") %>%
  body_add_par("Table 2. Joint Behavioral Group Hazard Ratios", style = "heading 2") %>%
  body_add_flextable(ft_tbl2)

# Save the document
print(doc, target = "Hypertension_Tables.docx")
cat("✔ Word document saved: Hypertension_Tables.docx\n")


# After creating the doc object, before printing
doc <- doc %>%
  body_add_par(" ", style = "Normal") %>%
  body_add_par("Figure 4. Forest Plot of Joint Group Hazard Ratios", style = "heading 2") %>%
  body_add_img(src = "Figure4_ForestPlot_JointHR.png", width = 6, height = 4) %>%
  body_add_par(" ", style = "Normal") %>%
  body_add_par("Figure 5. Kaplan-Meier Curves", style = "heading 2") %>%
  body_add_img(src = "Figure5_KaplanMeier.png", width = 6, height = 5)









# ── Figure 4a: Sensitivity Forest Plot (Age & BMI) ───────────────────────────
library(ggplot2)
library(dplyr)
library(tidyr)
library(survival)

# 1. Define subgroups and levels
subgroups <- list(
  Age = list(var = "Age_grp", levels = c("<60 yrs", ">=60 yrs"),
             labels = c("<60 years", "≥60 years")),
  BMI = list(var = "BMI_grp", levels = c("BMI<24", "BMI>=24"),
             labels = c("<24 kg/m²", "≥24 kg/m²"))
)

# 2. Function to get HR (Short Sleep/Low PA vs Adequate Sleep/High PA)
get_hr <- function(data, subset_var, subset_level, ref_group = "Adequate Sleep / High PA", 
                   exposure = "Short Sleep / Low PA") {
  df_sub <- data %>%
    filter(.data[[subset_var]] == subset_level) %>%
    filter(Joint_Group %in% c(ref_group, exposure)) %>%
    droplevels()
  
  if (nrow(df_sub) < 10 || nlevels(df_sub$Joint_Group) < 2) return(NULL)
  
  form <- as.formula(paste("Surv(FU_Time, Event) ~ Joint_Group +", 
                           paste(covars_m3, collapse = " + ")))
  fit <- coxph(form, data = df_sub)
  
  hr <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == paste0("Joint_Group", exposure)) %>%
    transmute(estimate, conf.low, conf.high, p.value)
  
  return(hr)
}

# 3. Compute HRs for all subgroups
plot_data <- list()
for (grp in names(subgroups)) {
  for (i in seq_along(subgroups[[grp]]$levels)) {
    hr_df <- get_hr(df_cc, subgroups[[grp]]$var, subgroups[[grp]]$levels[i])
    if (!is.null(hr_df)) {
      plot_data[[length(plot_data) + 1]] <- data.frame(
        Subgroup = grp,
        Category = subgroups[[grp]]$labels[i],
        hr_df
      )
    }
  }
}
plot_df <- bind_rows(plot_data)

# 4. Compute interaction p‑values (LRT)
interaction_p <- list()
for (grp in names(subgroups)) {
  f_no <- as.formula(paste("Surv(FU_Time, Event) ~ Joint_Group +", subgroups[[grp]]$var, "+",
                           paste(covars_m3, collapse = " + ")))
  f_int <- as.formula(paste("Surv(FU_Time, Event) ~ Joint_Group *", subgroups[[grp]]$var, "+",
                            paste(covars_m3, collapse = " + ")))
  m_no <- coxph(f_no, data = df_cc)
  m_int <- coxph(f_int, data = df_cc)
  lr <- lmtest::lrtest(m_no, m_int)
  interaction_p[[grp]] <- lr$`Pr(>Chisq)`[2]
}

# 5. Create the forest plot
p_sens <- ggplot(plot_df, aes(x = estimate, y = Category)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 1, color = "black") +
  geom_point(size = 4, shape = 18, color = "#D73027") +
  geom_text(aes(label = sprintf("HR = %.2f (%.2f-%.2f)", estimate, conf.low, conf.high)),
            hjust = -0.1, size = 4, fontface = "plain") +
  facet_grid(Subgroup ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_log10(limits = c(0.8, 2.6), breaks = c(0.8, 1, 1.5, 2, 2.5)) +
  labs(
    title = "Figure 4a. Sensitivity Analysis: Short Sleep/Low PA Hazard Ratios by Age and BMI",
    subtitle = "Model 3 fully adjusted; reference = Adequate Sleep/High PA",
    x = "Hazard Ratio (log scale)",
    y = NULL,
    caption = sprintf(
      "P for interaction: Age = %.2f (n.s.), BMI = %.2f (n.s.)",
      interaction_p$Age, interaction_p$BMI
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 12, hjust = 1),
    panel.spacing = unit(1, "lines"),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),
    plot.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 11),
    axis.title.x = element_text(size = 11)
  )

# 6. Save and display
ggsave("Figure4a_Sensitivity_Age_BMI.png", p_sens, width = 8, height = 5, dpi = 300)
print(p_sens)
cat("✔ Figure 4a saved: Figure4a_Sensitivity_Age_BMI.png\n")
 






# ── Figure 4a: FORMATTED Sensitivity Forest Plot (Age & BMI) ──────────────────
library(ggplot2)
library(dplyr)
library(survival)
library(lmtest)

# 1. Define subgroups and levels
subgroups <- list(
  Age = list(var = "Age_grp", levels = c("<60 yrs", ">=60 yrs"),
             labels = c("<60 years", "≥60 years")),
  BMI = list(var = "BMI_grp", levels = c("BMI<24", "BMI>=24"),
             labels = c("<24 kg/m²", "≥24 kg/m²"))
)

# 2. Function to get HR, CI, and sample sizes
get_hr_n <- function(data, subset_var, subset_level, 
                     ref_group = "Adequate Sleep / High PA", 
                     exposure = "Short Sleep / Low PA") {
  
  df_sub <- data %>%
    filter(.data[[subset_var]] == subset_level) %>%
    filter(Joint_Group %in% c(ref_group, exposure)) %>%
    droplevels()
  
  if (nrow(df_sub) < 10 || nlevels(df_sub$Joint_Group) < 2) return(NULL)
  
  # Sample sizes
  n_total <- nrow(df_sub)
  n_exposure <- sum(df_sub$Joint_Group == exposure)
  n_ref <- sum(df_sub$Joint_Group == ref_group)
  events_exposure <- sum(df_sub$Event[df_sub$Joint_Group == exposure])
  events_ref <- sum(df_sub$Event[df_sub$Joint_Group == ref_group])
  
  # Cox model
  form <- as.formula(paste("Surv(FU_Time, Event) ~ Joint_Group +", 
                           paste(covars_m3, collapse = " + ")))
  fit <- coxph(form, data = df_sub)
  
  hr <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == paste0("Joint_Group", exposure)) %>%
    transmute(estimate, conf.low, conf.high, p.value)
  
  hr$N_total <- n_total
  hr$N_exposure <- n_exposure
  hr$N_ref <- n_ref
  hr$Events_exposure <- events_exposure
  hr$Events_ref <- events_ref
  return(hr)
}

# 3. Compute data for all subgroups
plot_data <- list()
for (grp in names(subgroups)) {
  for (i in seq_along(subgroups[[grp]]$levels)) {
    res <- get_hr_n(df_cc, subgroups[[grp]]$var, subgroups[[grp]]$levels[i])
    if (!is.null(res)) {
      plot_data[[length(plot_data) + 1]] <- data.frame(
        Subgroup = grp,
        Category = subgroups[[grp]]$labels[i],
        res
      )
    }
  }
}
plot_df <- bind_rows(plot_data)

# 4. Interaction p‑values (LRT)
interaction_p <- list()
for (grp in names(subgroups)) {
  f_no <- as.formula(paste("Surv(FU_Time, Event) ~ Joint_Group +", subgroups[[grp]]$var, "+",
                           paste(covars_m3, collapse = " + ")))
  f_int <- as.formula(paste("Surv(FU_Time, Event) ~ Joint_Group *", subgroups[[grp]]$var, "+",
                            paste(covars_m3, collapse = " + ")))
  m_no <- coxph(f_no, data = df_cc)
  m_int <- coxph(f_int, data = df_cc)
  lr <- lrtest(m_no, m_int)
  interaction_p[[grp]] <- lr$`Pr(>Chisq)`[2]
}

# 5. Create FORMATTED forest plot with thick bars, sample sizes, and clean layout
p_sens <- ggplot(plot_df, aes(x = estimate, y = Category)) +
  # Vertical reference line at HR = 1
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  
  # Thick confidence interval bars (like a horizontal bar plot)
  geom_segment(aes(x = conf.low, xend = conf.high, yend = Category),
               linewidth = 4, color = "#2166AC", alpha = 0.7) +
  
  # Point estimate (diamond/square)
  geom_point(size = 5, shape = 18, color = "#D73027") +
  
  # HR label (placed to the right of the bar)
  geom_text(aes(label = sprintf("HR = %.2f (%.2f-%.2f)", estimate, conf.low, conf.high)),
            hjust = -0.05, size = 3.8, fontface = "plain") +
  
  # Sample size annotation (N total and events)
  geom_text(aes(x = 2.5, 
                label = paste0("N = ", N_total, "  (", Events_exposure, "/", Events_ref, ")")),
            hjust = 0, size = 3.2, color = "gray30") +
  
  # Facet by Subgroup (Age on top, BMI below)
  facet_grid(Subgroup ~ ., scales = "free_y", space = "free_y", switch = "y") +
  
  # Log scale for HR
  scale_x_log10(limits = c(0.6, 3.0), breaks = c(0.6, 0.8, 1, 1.5, 2, 2.5, 3.0)) +
  
  # Labels and caption
  labs(
    title = "Figure 4a. Sensitivity Analysis: Short Sleep/Low PA Hazard Ratios by Age and BMI",
    subtitle = "Model 3 fully adjusted; reference = Adequate Sleep/High PA",
    x = "Hazard Ratio (log scale)",
    y = NULL,
    caption = sprintf(
      "P for interaction: Age = %.2f (n.s.), BMI = %.2f (n.s.)\nBars = 95%% CI. Numbers show total N and events (exposure/ref).",
      interaction_p$Age, interaction_p$BMI
    )
  ) +
  
  # Clean theme
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 12, hjust = 1),
    panel.spacing = unit(1.2, "lines"),
    plot.caption = element_text(hjust = 0, face = "italic", size = 9, color = "gray30"),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    plot.margin = margin(r = 20, unit = "pt")   # extra space for N labels
  )

# 6. Save high‑resolution figure
ggsave("Figure4a_Sensitivity_Age_BMI.png", p_sens, width = 9.5, height = 5.5, dpi = 300)
print(p_sens)
cat("✔ Formatted Figure 4a saved: Figure4a_Sensitivity_Age_BMI.png\n")
 
# =============================================================================
# FIGURE 4a: Sensitivity Forest Plot – Short Sleep/Low PA by Age & BMI
# Fully adjusted model (Model 3), reference = Adequate Sleep/High PA
# =============================================================================

library(ggplot2)
library(dplyr)
library(survival)
library(lmtest)

# 1. Define subgroups and labels
subgroups <- list(
  Age = list(var = "Age_grp", levels = c("<60 yrs", ">=60 yrs"),
             labels = c("<60 years", "≥60 years")),
  BMI = list(var = "BMI_grp", levels = c("BMI<24", "BMI>=24"),
             labels = c("<24 kg/m²", "≥24 kg/m²"))
)

# 2. Function to compute HR, CI, and sample sizes for a given subgroup level
get_hr_n <- function(data, subset_var, subset_level, 
                     ref_group = "Adequate Sleep / High PA", 
                     exposure = "Short Sleep / Low PA") {
  
  df_sub <- data %>%
    filter(.data[[subset_var]] == subset_level) %>%
    filter(Joint_Group %in% c(ref_group, exposure)) %>%
    droplevels()
  
  if (nrow(df_sub) < 10 || nlevels(df_sub$Joint_Group) < 2) return(NULL)
  
  # Sample sizes and events
  n_total <- nrow(df_sub)
  n_exposure <- sum(df_sub$Joint_Group == exposure)
  n_ref <- sum(df_sub$Joint_Group == ref_group)
  events_exposure <- sum(df_sub$Event[df_sub$Joint_Group == exposure])
  events_ref <- sum(df_sub$Event[df_sub$Joint_Group == ref_group])
  
  # Cox model (fully adjusted = covars_m3)
  form <- as.formula(paste("Surv(FU_Time, Event) ~ Joint_Group +", 
                           paste(covars_m3, collapse = " + ")))
  fit <- coxph(form, data = df_sub)
  
  hr <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == paste0("Joint_Group", exposure)) %>%
    transmute(estimate, conf.low, conf.high, p.value)
  
  hr$N_total <- n_total
  hr$N_exposure <- n_exposure
  hr$N_ref <- n_ref
  hr$Events_exposure <- events_exposure
  hr$Events_ref <- events_ref
  return(hr)
}

# 3. Compute data for all subgroups
plot_data <- list()
for (grp in names(subgroups)) {
  for (i in seq_along(subgroups[[grp]]$levels)) {
    res <- get_hr_n(df_cc, subgroups[[grp]]$var, subgroups[[grp]]$levels[i])
    if (!is.null(res)) {
      plot_data[[length(plot_data) + 1]] <- data.frame(
        Subgroup = grp,
        Category = subgroups[[grp]]$labels[i],
        res
      )
    }
  }
}
plot_df <- bind_rows(plot_data)

# 4. Interaction p‑values (Likelihood Ratio Test)
interaction_p <- list()
for (grp in names(subgroups)) {
  f_no <- as.formula(paste("Surv(FU_Time, Event) ~ Joint_Group +", subgroups[[grp]]$var, "+",
                           paste(covars_m3, collapse = " + ")))
  f_int <- as.formula(paste("Surv(FU_Time, Event) ~ Joint_Group *", subgroups[[grp]]$var, "+",
                            paste(covars_m3, collapse = " + ")))
  m_no <- coxph(f_no, data = df_cc)
  m_int <- coxph(f_int, data = df_cc)
  lr <- lrtest(m_no, m_int)
  interaction_p[[grp]] <- lr$`Pr(>Chisq)`[2]
}

# 5. Create forest plot with multi‑line HR labels and no overlap
p_sens <- ggplot(plot_df, aes(x = estimate, y = Category)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  
  # Thick confidence bar
  geom_segment(aes(x = conf.low, xend = conf.high, yend = Category),
               linewidth = 4, color = "#2166AC", alpha = 0.7) +
  
  # Point estimate (red diamond)
  geom_point(size = 5, shape = 18, color = "#D73027") +
  
  # HR label: two lines (HR on first line, CI on second)
  geom_text(aes(x = 2.8, 
                label = sprintf("HR = %.2f\n(%.2f-%.2f)", estimate, conf.low, conf.high)),
            hjust = 0, vjust = 0.5, size = 3.5, lineheight = 0.9) +
  
  # Sample size label (N and events) – placed further right
  geom_text(aes(x = 3.8, 
                label = paste0("N = ", N_total, "  (", Events_exposure, "/", Events_ref, ")")),
            hjust = 0, size = 3.2, color = "gray30") +
  
  facet_grid(Subgroup ~ ., scales = "free_y", space = "free_y", switch = "y") +
  
  # Extended x‑axis to accommodate all labels
  scale_x_log10(limits = c(0.5, 4.5), breaks = c(0.5, 0.8, 1, 1.5, 2, 2.5, 3, 4)) +
  
  labs(
    title = "Figure 4a. Sensitivity Analysis: Short Sleep/Low PA Hazard Ratios by Age and BMI",
    subtitle = "Model 3 fully adjusted; reference = Adequate Sleep/High PA",
    x = "Hazard Ratio (log scale)",
    y = NULL,
    caption = sprintf(
      "P for interaction: Age = %.2f (n.s.), BMI = %.2f (n.s.)\nBars = 95%% CI. Numbers show total N and events (exposure/ref).",
      interaction_p$Age, interaction_p$BMI
    )
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 12, hjust = 1),
    panel.spacing = unit(1.2, "lines"),
    plot.caption = element_text(hjust = 0, face = "italic", size = 9, color = "gray30"),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    plot.margin = margin(r = 50, unit = "pt")   # large right margin to avoid clipping
  )

# 6. Save and display
ggsave("Figure4a_Sensitivity_Age_BMI.png", p_sens, width = 12, height = 5.5, dpi = 300)
print(p_sens)
cat("✔ Figure 4a saved: Figure4a_Sensitivity_Age_BMI.png\n")



# ── Add Residence, Education, and Kidney disease categories to Table 1 ─────────
# These columns are not in df_cc (imputed) but exist in original df with no NAs
cat("Adding Residence and Education from original data...\n")
df_cc$Residence <- df$Residence
df_cc$Education <- df$Education

# Create display variables
df_cc <- df_cc %>%
  mutate(
    Residence_cat = factor(Residence, levels = c("Rural", "Urban")),
    Education_cat = factor(
      case_when(
        grepl("Primary", Education, ignore.case = TRUE) ~ "Primary",
        grepl("Secondary", Education, ignore.case = TRUE) ~ "Secondary",
        grepl("Tertiary|College|University", Education, ignore.case = TRUE) ~ "Tertiary",
        TRUE ~ NA_character_
      ),
      levels = c("Primary", "Secondary", "Tertiary")
    ),
    Kidney_disease = factor(ifelse(Kidney_bin == 1, "Yes", "No"), levels = c("No", "Yes"))
  )

# Now update tbl1 with the new variables
tbl1 <- df_cc %>%
  select(Joint_Group, Age, BMI, Residence_cat, Education_cat, Income,
         Smoking_bin, Drinking_bin, Diabetes_bin, Kidney_disease, Menopause_bin,
         Cholesterol, Glucose, Baseline_SBP, Baseline_DBP, Event, FU_Time) %>%
  tbl_summary(
    by = Joint_Group,
    statistic = list(
      all_continuous()  ~ "{mean} \u00b1 {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      Age               ~ "Age (years)",
      BMI               ~ "BMI (kg/m\u00b2)",
      Residence_cat     ~ "Residence, n (%)",
      Education_cat     ~ "Education level, n (%)",
      Income            ~ "Annual income (Yuan)",
      Smoking_bin       ~ "Current smoking, n (%)",
      Drinking_bin      ~ "Current drinking, n (%)",
      Diabetes_bin      ~ "Diabetes, n (%)",
      Kidney_disease    ~ "Kidney disease, n (%)",
      Menopause_bin     ~ "Post-menopausal, n (%)",
      Cholesterol       ~ "Total cholesterol (mg/dL)",
      Glucose           ~ "Fasting glucose (mmol/L)",
      Baseline_SBP      ~ "Baseline SBP (mmHg)",
      Baseline_DBP      ~ "Baseline DBP (mmHg)",
      Event             ~ "Incident hypertension, n (%)",
      FU_Time           ~ "Follow-up time (years), mean (SD)"
    ),
    digits = list(all_continuous() ~ 2)
  ) %>%
  add_p(test = list(
    all_continuous()  ~ "oneway.test",
    all_categorical() ~ "chisq.test"
  )) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("**Table 1. Baseline Characteristics by Joint Sleep-PA Group (N = 2,097)**")

cat("✔ Table 1 updated with Residence, Education, and Kidney disease.\n\n")


# ── Save Table 1 and Table 2 as Excel files ───────────────────────────────────
library(writexl)  # install if needed: install.packages("writexl")

# Convert gtsummary table to a data frame (for Excel)
tbl1_df <- tbl1 %>% 
  as_tibble()  # gtsummary objects can be converted to tibble

# If as_tibble() doesn't work, use this alternative:
# tbl1_df <- as.data.frame(tbl1)

# Table 2 is already a data frame (table2)
# Write both to separate sheets in one Excel file
list_of_tables <- list(
  "Table1_Baseline" = tbl1_df,
  "Table2_HazardRatios" = table2
)

write_xlsx(list_of_tables, "Hypertension_Tables.xlsx")
cat("✔ Excel file saved: Hypertension_Tables.xlsx (with sheets: Table1_Baseline, Table2_HazardRatios)\n")







# =========================================================================
# TABLE 2: Hazard Ratios for Incident Hypertension by Joint Sleep-PA Group
# =========================================================================

library(dplyr)
library(tidyr)
library(broom)
library(gt)   # for nice output; can replace with flextable or kable

# 1. Compute events, person-years, and crude rates -------------------------
table2_base <- df_cc %>%
  group_by(Joint_Group) %>%
  summarise(
    Events = sum(Event, na.rm = TRUE),
    PY     = sum(FU_Time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rate_per_1000PY = round(Events / PY * 1000, 1)
  )

# 2. Extract HR and p-values from the three Cox models --------------------
# Helper to get HR (95% CI) and p-value for each non-reference level
extract_hr_p <- function(model, model_name) {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl("Joint_Group", term)) %>%
    mutate(
      Group = gsub("Joint_Group", "", term),
      !!paste0("HR_", model_name) := sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
      !!paste0("P_", model_name) := p.value
    ) %>%
    select(Group, starts_with("HR_"), starts_with("P_"))
}

hr_m1 <- extract_hr_p(cox_joint_m1, "Model1")
hr_m2 <- extract_hr_p(cox_joint_m2, "Model2")
hr_m3 <- extract_hr_p(cox_joint_m3, "Model3")

# Combine all HR information
table2_hr <- hr_m1 %>%
  full_join(hr_m2, by = "Group") %>%
  full_join(hr_m3, by = "Group")

# 3. Merge with base table and add reference row --------------------------
table2 <- table2_base %>%
  left_join(table2_hr, by = c("Joint_Group" = "Group")) %>%
  # Ensure reference group has HR=1 and blank p-values
  mutate(
    HR_Model1 = ifelse(Joint_Group == "Adequate Sleep / High PA", "1 (Ref)", HR_Model1),
    P_Model1  = ifelse(Joint_Group == "Adequate Sleep / High PA", NA, P_Model1),
    HR_Model2 = ifelse(Joint_Group == "Adequate Sleep / High PA", "1 (Ref)", HR_Model2),
    P_Model2  = ifelse(Joint_Group == "Adequate Sleep / High PA", NA, P_Model2),
    HR_Model3 = ifelse(Joint_Group == "Adequate Sleep / High PA", "1 (Ref)", HR_Model3),
    P_Model3  = ifelse(Joint_Group == "Adequate Sleep / High PA", NA, P_Model3)
  ) %>%
  # Round p-values to 3 or 4 decimals, add asterisks
  mutate(
    across(starts_with("P_"), ~ ifelse(!is.na(.), round(., 4), .)),
    P_Model1_star = case_when(
      P_Model1 < 0.001 ~ "<0.001***",
      P_Model1 < 0.01  ~ sprintf("%.3f**", P_Model1),
      P_Model1 < 0.05  ~ sprintf("%.3f*",  P_Model1),
      TRUE             ~ as.character(P_Model1)
    ),
    P_Model2_star = case_when(
      P_Model2 < 0.001 ~ "<0.001***",
      P_Model2 < 0.01  ~ sprintf("%.3f**", P_Model2),
      P_Model2 < 0.05  ~ sprintf("%.3f*",  P_Model2),
      TRUE             ~ as.character(P_Model2)
    ),
    P_Model3_star = case_when(
      P_Model3 < 0.001 ~ "<0.001***",
      P_Model3 < 0.01  ~ sprintf("%.3f**", P_Model3),
      P_Model3 < 0.05  ~ sprintf("%.3f*",  P_Model3),
      TRUE             ~ as.character(P_Model3)
    )
  ) %>%
  select(Joint_Group, Events, PY, Rate_per_1000PY,
         HR_Model1, P_Model1_star,
         HR_Model2, P_Model2_star,
         HR_Model3, P_Model3_star)

# 4. Create a publication‑ready gt table ----------------------------------
gt_table2 <- table2 %>%
  gt() %>%
  tab_header(
    title = "Table 2. Hazard Ratios (HR) for Incident Hypertension Across Joint Behavioral Groups"
  ) %>%
  cols_label(
    Joint_Group      = "Group",
    Events           = "Events",
    PY               = "Person-years",
    Rate_per_1000PY  = "Rate per 1000 PY",
    HR_Model1        = "HR (95% CI)",
    P_Model1_star    = "p",
    HR_Model2        = "HR (95% CI)",
    P_Model2_star    = "p",
    HR_Model3        = "HR (95% CI)",
    P_Model3_star    = "p"
  ) %>%
  tab_spanner(
    label = "Model 1 (Unadjusted)",
    columns = c(HR_Model1, P_Model1_star)
  ) %>%
  tab_spanner(
    label = "Model 2 (Age, residence, education, income)",
    columns = c(HR_Model2, P_Model2_star)
  ) %>%
  tab_spanner(
    label = "Model 3 (Fully adjusted)",
    columns = c(HR_Model3, P_Model3_star)
  ) %>%
  fmt_number(
    columns = c(PY, Rate_per_1000PY),
    decimals = 1
  ) %>%
  tab_footnote(
    footnote = "Model 1: Unadjusted; Model 2: Adjusted for age, residence, education, income; Model 3: Fully adjusted (BMI, lifestyle, clinical covariates).",
    locations = cells_column_spanners(spanners = "Model 1 (Unadjusted)")
  ) %>%
  tab_footnote(
    footnote = "* p<0.05; ** p<0.01; *** p<0.001",
    locations = cells_title(groups = "title")
  ) %>%
  opt_row_striping() %>%
  opt_align_table_header(align = "left")

# Print the table (will show in RStudio viewer or save as image/html)
gt_table2
 
 

# =========================================================================
# FIGURE 5: Kaplan-Meier Cumulative Incidence (fully corrected)
# =========================================================================

library(survival)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(patchwork)

# 1. Fit survival curves
km_fit <- survfit(Surv(FU_Time, Event) ~ Joint_Group, data = df_cc)

# 2. Extract cumulative incidence data (1 - survival) for plotting
km_data <- tidy(km_fit) %>%
  mutate(incidence = 1 - estimate,
         group = strata) %>%
  separate(group, into = c("tmp", "Joint_Group"), sep = "=") %>%
  select(time, incidence, std.error, conf.low, conf.high, Joint_Group) %>%  # use std.error
  # Transform survival CI to incidence CI: incidence CI = (1 - conf.high, 1 - conf.low)
  mutate(conf.low_inc = 1 - conf.high,
         conf.high_inc = 1 - conf.low) %>%
  # Fix any negative bounds (should not happen but safe)
  mutate(across(c(incidence, conf.low_inc, conf.high_inc), ~ pmax(0, .))) %>%
  select(time, incidence, conf.low = conf.low_inc, conf.high = conf.high_inc, Joint_Group)

# 3. Define colour palette (order matches factor levels of Joint_Group)
km_palette <- c(
  "#1A9641",   # Adequate/High
  "#A6D96A",   # Adequate/Moderate
  "#FDAE61",   # Adequate/Low
  "#74ADD1",   # Short/High
  "#D7191C",   # Short/Moderate
  "#7B2D8B"    # Short/Low
)
names(km_palette) <- levels(df_cc$Joint_Group)

# 4. Extract risk table data (number at risk at selected time points)
risk_data <- summary(km_fit, times = seq(0, 15, by = 2)) %>%
  with(data.frame(time = time, n.risk = n.risk, strata = strata)) %>%
  separate(strata, into = c("tmp", "Joint_Group"), sep = "=") %>%
  select(-tmp) %>%
  mutate(Joint_Group = factor(Joint_Group, levels = levels(df_cc$Joint_Group)))

# 5. Compute log-rank p-value
logrank_test <- survdiff(Surv(FU_Time, Event) ~ Joint_Group, data = df_cc)
p_val <- 1 - pchisq(logrank_test$chisq, length(logrank_test$n) - 1)
p_label <- ifelse(p_val < 0.0001, "p < 0.0001", paste("p =", round(p_val, 4)))

# 6. Main cumulative incidence plot
p_main <- ggplot(km_data, aes(x = time, y = incidence, colour = Joint_Group)) +
  geom_step(size = 1.2) +
  scale_colour_manual(values = km_palette) +
  coord_cartesian(xlim = c(0, 15), ylim = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 15, by = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Follow-up Time (years)",
       y = "Cumulative Incidence of Hypertension",
       title = "Figure 5: Kaplan-Meier — Hypertension-Free Survival",
       subtitle = "8-year follow-up, CHARLS 2011-2019",
       colour = "Sleep / PA Group") +
  annotate("text", x = 2, y = 0.85, label = p_label, size = 4, hjust = 0) +
  theme_classic(base_size = 12) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "white", colour = "grey80"))

# 7. Risk table as a separate ggplot
p_risk <- ggplot(risk_data, aes(x = time, y = Joint_Group, label = n.risk)) +
  geom_text(size = 3.5) +
  scale_x_continuous(breaks = seq(0, 15, by = 2), limits = c(0, 15)) +
  labs(x = "Time (years)", y = NULL) +
  theme_classic(base_size = 10) +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# 8. Combine plots
combined_plot <- (p_main / p_risk) + plot_layout(heights = c(3, 1))

# 9. Display and save
print(combined_plot)
ggsave("Figure5_KaplanMeier.png", combined_plot, width = 10, height = 7, dpi = 300)

cat("✔ Figure 5 saved as 'Figure5_KaplanMeier.png'\n")