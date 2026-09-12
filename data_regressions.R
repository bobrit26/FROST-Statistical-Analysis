library(readr)
library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)


ds <- read_csv("data_frost_cleaned.csv", show_col_types = FALSE)

#numeric transformations
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

#output folders for the visuals
dir.create("Visuals", showWarnings = FALSE)
dir.create("Visuals/Tables", recursive = TRUE, showWarnings = FALSE)
dir.create("Visuals/Figures", recursive = TRUE, showWarnings = FALSE)

#regression models

#model 0: minimal baseline model + 3 basic controls (it's 0 because it's below our core theory level)
m0 <- lm(
  institution_mean ~ german_citizen_binary +
    migration_background +
    age + #controls
    gender_binary +
    uni_binary,
  data = ds
)

#model 1: baseline interaction model + 3 basic controls
m1 <- lm(
  institution_mean ~ german_citizen_binary *
    migration_background +
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

#model 4A: model 1 + 3 sense of belonging items + 2 citizenship opinion items + discrimination frequency
m4A <- lm(
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

#model 4B: model 4A replace migration_background with migration_generation (alternative specification just to see if there is a difference)
m4B <- lm(
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
  "Model 2: + Belonging (split)" = m2,
  "Model 3: + Discrimination" = m3,
  "Model 4A: + Belonging (split) + Discrim" = m4A,
  "Model 4B: Cit×MigGen + mediators" = m4B
)

#HTML regression export
modelsummary::modelsummary(
  models,
  output = "Visuals/Tables/regression_models.html",
  stars = TRUE,
  statistic = "({std.error})",
  fmt = 3,
  gof_omit = "AIC|BIC|Log\\.Lik|RMSE"
)

#coefficient export
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

#coefficient plot export

#models 1 to 4A
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

#model 4B
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

  ggplot(df, aes(x = estimate, y = reorder(term, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    labs(x = "Coefficient (95% CI)", y = NULL, title = model_name) +
    theme_minimal(base_size = 12)

  ggsave(file_out, width = 7.8, height = 4.6, dpi = 300)
}

plot_model_coefs(
  m1,
  "Model 1 (Cit×MigBG + controls)",
  terms_migbg,
  "Visuals/Figures/coefplot_M1.png"
)
plot_model_coefs(
  m2,
  "Model 2 (+ belonging split)",
  terms_migbg,
  "Visuals/Figures/coefplot_M2.png"
)
plot_model_coefs(
  m3,
  "Model 3 (+ discrimination)",
  terms_migbg,
  "Visuals/Figures/coefplot_M3.png"
)
plot_model_coefs(
  m4A,
  "Model 4 (+ belonging split + discrimination)",
  terms_migbg,
  "Visuals/Figures/coefplot_M4A.png"
)
plot_model_coefs(
  m4B,
  "Model 4B (Cit×Migration generation + mediators)",
  terms_miggen,
  "Visuals/Figures/coefplot_M4B.png"
)
