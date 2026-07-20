# ============================================================
# 03_regressions_and_plots.R  (copy-paste)
# Changes vs prior:
#   - Split belonging into pb_citizenship and pb_belonging
#   - Run 4 main models (MigBG) + 1 generation model (MigGen)
#   - Export a combined regression table
#   - Produce coefficient plots for EACH model (M1–M5)
# ============================================================

library(readr)
library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)

# -----------------------------
# Load cleaned data
# -----------------------------
ds <- read_csv("data_frost_cleaned.csv", show_col_types = FALSE)

# -----------------------------
# Type handling for clean interpretation
# -----------------------------
ds <- ds %>%
  mutate(
    institution_mean = suppressWarnings(as.numeric(institution_mean)),
    pb_citizenship   = suppressWarnings(as.numeric(pb_citizenship)),
    pb_belonging     = suppressWarnings(as.numeric(pb_belonging)),
    discrim_mean     = suppressWarnings(as.numeric(discrim_mean)),
    age              = suppressWarnings(as.numeric(age)),
    
    german_citizen_binary = factor(german_citizen_binary, levels = c(0, 1), labels = c("No", "Yes")),
    migration_background  = factor(migration_background,  levels = c(0, 1), labels = c("No", "Yes")),
    migration_generation  = factor(migration_generation, levels = c("Non-migrant", "2nd gen", "1st gen")),
    
    gender_binary = factor(gender_binary, levels = c(0, 1), labels = c("Female", "Male")),
    uni_binary    = factor(uni_binary, levels = c(0, 1), labels = c("Non-uni", "Uni"))
  )

# -----------------------------
# Output folders
# -----------------------------
dir.create("Visuals", showWarnings = FALSE)
dir.create("Visuals/Tables", recursive = TRUE, showWarnings = FALSE)
dir.create("Visuals/Figures", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Models (Migration background)
# ============================================================

# M1: baseline interaction + controls
m1 <- lm(institution_mean ~ german_citizen_binary * migration_background +
           age + gender_binary + uni_binary,
         data = ds)

# M2: add belonging (split)
m2 <- lm(institution_mean ~ german_citizen_binary * migration_background +
           age + gender_binary + uni_binary +
           pb_citizenship + pb_belonging,
         data = ds)

# M3: add discrimination
m3 <- lm(institution_mean ~ german_citizen_binary * migration_background +
           age + gender_binary + uni_binary +
           discrim_mean,
         data = ds)

# M4: add belonging (split) + discrimination
m4 <- lm(institution_mean ~ german_citizen_binary * migration_background +
           age + gender_binary + uni_binary +
           pb_citizenship + pb_belonging + discrim_mean,
         data = ds)

# ============================================================
# Robustness / extension (Migration generation)
# (uses generation categories instead of migration_background)
# ============================================================
m5 <- lm(institution_mean ~ german_citizen_binary * migration_generation +
           age + gender_binary + uni_binary +
           pb_citizenship + pb_belonging + discrim_mean,
         data = ds)

models <- list(
  "M1: Cit×MigBG + controls"         = m1,
  "M2: + Belonging (split)"          = m2,
  "M3: + Discrimination"             = m3,
  "M4: + Belonging (split) + Discrim"= m4,
  "M5: Cit×MigGen + mediators"       = m5
)

# -----------------------------
# Export regression table (HTML)
# -----------------------------
modelsummary::modelsummary(
  models,
  output = "Visuals/Tables/regression_models.html",
  stars = TRUE,
  statistic = "({std.error})",
  fmt = 3,
  gof_omit = "AIC|BIC|Log\\.Lik|RMSE"
)

# Also export tidy coefficients for transparency
tidy_all <- bind_rows(lapply(names(models), function(nm) {
  broom::tidy(models[[nm]], conf.int = TRUE) %>%
    mutate(model = nm)
})) %>%
  mutate(
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    std.error = round(std.error, 3),
    p.value = round(p.value, 3)
  )

write_csv(tidy_all, "Visuals/Tables/regression_tidy_all.csv")

# ============================================================
# Coefficient plots (ONE per model)
# Only plot substantively relevant terms to keep slides readable.
# ============================================================

# Terms to plot for MigBG models (M1–M4)
terms_migbg <- c(
  "german_citizen_binaryYes",
  "migration_backgroundYes",
  "german_citizen_binaryYes:migration_backgroundYes",
  "pb_citizenship",
  "pb_belonging",
  "discrim_mean",
  "age",
  "gender_binaryMale",
  "uni_binaryUni"
)

# Terms to plot for MigGen model (M5)
terms_miggen <- c(
  "german_citizen_binaryYes",
  "migration_generation2nd gen",
  "migration_generation1st gen",
  "german_citizen_binaryYes:migration_generation2nd gen",
  "german_citizen_binaryYes:migration_generation1st gen",
  "pb_citizenship",
  "pb_belonging",
  "discrim_mean",
  "age",
  "gender_binaryMale",
  "uni_binaryUni"
)

pretty_term <- function(x) {
  dplyr::recode(x,
                "german_citizen_binaryYes" = "Citizenship (Yes)",
                "migration_backgroundYes" = "Migration background (Yes)",
                "german_citizen_binaryYes:migration_backgroundYes" = "Citizenship × MigBG",
                "migration_generation2nd gen" = "2nd gen (vs Non-migrant)",
                "migration_generation1st gen" = "1st gen (vs Non-migrant)",
                "german_citizen_binaryYes:migration_generation2nd gen" = "Citizenship × 2nd gen",
                "german_citizen_binaryYes:migration_generation1st gen" = "Citizenship × 1st gen",
                "pb_citizenship" = "Belonging: citizenship items",
                "pb_belonging" = "Belonging: belonging items",
                "discrim_mean" = "Discrimination (mean)",
                "age" = "Age",
                "gender_binaryMale" = "Male (vs Female)",
                "uni_binaryUni" = "University (vs non-uni)",
                .default = x
  )
}

plot_model_coefs <- function(mod, model_name, keep_terms, file_out) {
  df <- broom::tidy(mod, conf.int = TRUE) %>%
    filter(term %in% keep_terms) %>%
    mutate(term = pretty_term(term))
  
  ggplot(df, aes(x = estimate, y = reorder(term, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    labs(x = "Coefficient (95% CI)", y = NULL, title = model_name) +
    theme_minimal(base_size = 12)
  
  ggsave(file_out, width = 7.8, height = 4.6, dpi = 300)
}

plot_model_coefs(m1, "Model 1 (Cit×MigBG + controls)", terms_migbg,
                 "Visuals/Figures/coefplot_M1.png")
plot_model_coefs(m2, "Model 2 (+ belonging split)", terms_migbg,
                 "Visuals/Figures/coefplot_M2.png")
plot_model_coefs(m3, "Model 3 (+ discrimination)", terms_migbg,
                 "Visuals/Figures/coefplot_M3.png")
plot_model_coefs(m4, "Model 4 (+ belonging split + discrimination)", terms_migbg,
                 "Visuals/Figures/coefplot_M4.png")
plot_model_coefs(m5, "Model 5 (Cit×Migration generation + mediators)", terms_miggen,
                 "Visuals/Figures/coefplot_M5.png")