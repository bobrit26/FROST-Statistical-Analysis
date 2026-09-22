library(readr)
library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)

# 1. read the cleaned data

ds <- read_csv("data_frost_cleaned.csv", show_col_types = FALSE)

# 2. numeric transformations

ds <- ds %>%
  mutate(
    institution_mean = suppressWarnings(as.numeric(institution_mean)),
    pb_citizenship = suppressWarnings(as.numeric(pb_citizenship)), #see the data_cleaning.r script for reminders on what this is
    pb_belonging = suppressWarnings(as.numeric(pb_belonging)),
    discrim_mean = suppressWarnings(as.numeric(discrim_mean)),
    age = suppressWarnings(as.numeric(age)),

    german_citizen_binary = factor(
      german_citizen_binary,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    migration_background = factor(
      migration_background,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    migration_generation = factor(
      migration_generation,
      levels = c("Non-migrant", "2nd gen", "1st gen")
    ),

    gender_binary = factor(
      gender_binary,
      levels = c(0, 1),
      labels = c("Female", "Male")
    ),
    uni_binary = factor(
      uni_binary,
      levels = c(0, 1),
      labels = c("Non-uni", "Uni")
    )
  )

# 3. output folders for the visuals

dir.create("Regressions", showWarnings = FALSE)
dir.create("Regressions/Tables", recursive = TRUE, showWarnings = FALSE)
dir.create("Regressions/Figures", recursive = TRUE, showWarnings = FALSE)

# 4. regression models

#model 0: minimal baseline model + 3 basic controls (it's 0 because it's below our core theory level)
m0 <- lm(
  institution_mean ~ german_citizen_binary +
    migration_background +
    age + #controls
    gender_binary +
    uni_binary,
  data = ds
)

#model 1A: baseline interaction model + 3 basic controls
m1 <- lm(
  institution_mean ~ german_citizen_binary *
    migration_background +
    age + #controls
    gender_binary +
    uni_binary,
  data = ds
)

# model 1 with alternative trust: model 1 + institution_mean_minimal - institution_mean
# aka our first robustness check model

m1_alt_trust <- lm(
  institution_mean_minimal ~ german_citizen_binary *
    migration_background +
    age + #controls
    gender_binary +
    uni_binary,
  data = ds
)

# model 1 with alternative migration: model 1 + migration_generation - migration_background
# aka our first robustness check model

m1_alt_migration <- lm(
  institution_mean ~ german_citizen_binary *
    migration_generation + #non-migrant vs 2nd gen vs 3rd gen
    age + #controls
    gender_binary +
    uni_binary,
  data = ds
)

#model 2: model 1 + 3 sense of belonging items + 2 citizenship opinion items
m2 <- lm(
  institution_mean ~ german_citizen_binary *
    migration_background +
    age +
    gender_binary +
    uni_binary +
    pb_citizenship + #personal belonging
    pb_belonging,
  data = ds
)

#model 3: model 1 + discrimination frequency
m3 <- lm(
  institution_mean ~ german_citizen_binary *
    migration_background +
    age +
    gender_binary +
    uni_binary +
    discrim_mean, #discrimination frequency
  data = ds
)

#model 4: model 1 + 3 sense of belonging items + 2 citizenship opinion items + discrimination frequency
m4 <- lm(
  institution_mean ~ german_citizen_binary *
    migration_background +
    age +
    gender_binary +
    uni_binary +
    pb_citizenship +
    pb_belonging +
    discrim_mean,
  data = ds
)

#model 4 alternative trust: model 4 + institution_mean_minimal - institution_mean
m4_alt_trust <- lm(
  institution_mean_minimal ~ german_citizen_binary *
    migration_background +
    age +
    gender_binary +
    uni_binary +
    pb_citizenship +
    pb_belonging +
    discrim_mean,
  data = ds
)

#model 4 alternative migration: model 4 + migration_generation - migration_background
m4_alt_migration <- lm(
  institution_mean ~ german_citizen_binary *
    migration_generation + #non-migrant vs 2nd gen vs 3rd gen
    age +
    gender_binary +
    uni_binary +
    pb_citizenship +
    pb_belonging +
    discrim_mean,
  data = ds
)
models <- list(
  "Model 0: Cit + MigBG + controls" = m0,
  "Model 1: Cit×MigBG + controls" = m1,
  "Model 1: Alternative trust - minimal trust scale" = m1_alt_trust,
  "Model 1: Alternative migration - migration generation" = m1_alt_migration,
  "Model 2: + Belonging (split)" = m2,
  "Model 3: + Discrimination" = m3,
  "Model 4: + Belonging (split) + Discrim" = m4,
  "Model 4: Alternative trust - minimal trust scale" = m4_alt_trust,
  "Model 4: Alternative migration - migration generation" = m4_alt_migration
)

# 5. HTML regression export
modelsummary::modelsummary(
  models,
  output = "Regressions/Tables/regression_models.html",
  stars = TRUE,
  statistic = "({std.error})",
  fmt = 3,
  gof_omit = "AIC|BIC|Log\\.Lik|RMSE"
)

# 6. coefficient export
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

write_csv(
  tidy_all,
  "Regressions/Tables/regression_tidy_all.csv"
) # frankly, I really doubt we will make use of this, but who knows

# 7. coefficient plot export

# models using migration background
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

# models using migration generation
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
  dplyr::recode(
    x,
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

  p <- ggplot(df, aes(x = estimate, y = reorder(term, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    labs(x = "Coefficient (95% CI)", y = NULL, title = model_name) +
    theme_minimal(base_size = 12)

  ggsave(file_out, p, width = 7.8, height = 4.6, dpi = 300)
}

plot_model_coefs(
  m1,
  "Model 1 (Cit×MigBG + controls)",
  terms_migbg,
  "Regressions/Figures/coefplot_M1.png"
)

plot_model_coefs(
  m1_alt_trust,
  "Model 1 alternative trust (minimal trust scale)",
  terms_migbg,
  "Regressions/Figures/coefplot_M1_alt_trust.png"
)

plot_model_coefs(
  m1_alt_migration,
  "Model 1 alternative migration (migration generation)",
  terms_miggen,
  "Regressions/Figures/coefplot_M1_alt_migration.png"
)

plot_model_coefs(
  m2,
  "Model 2 (+ belonging split)",
  terms_migbg,
  "Regressions/Figures/coefplot_M2.png"
)

plot_model_coefs(
  m3,
  "Model 3 (+ discrimination)",
  terms_migbg,
  "Regressions/Figures/coefplot_M3.png"
)

plot_model_coefs(
  m4,
  "Model 4 (+ belonging split + discrimination)",
  terms_migbg,
  "Regressions/Figures/coefplot_M4.png"
)

plot_model_coefs(
  m4_alt_trust,
  "Model 4 alternative trust (minimal trust scale)",
  terms_migbg,
  "Regressions/Figures/coefplot_M4_alt_trust.png"
)

plot_model_coefs(
  m4_alt_migration,
  "Model 4 alternative migration (migration generation)",
  terms_miggen,
  "Regressions/Figures/coefplot_M4_alt_migration.png"
)
