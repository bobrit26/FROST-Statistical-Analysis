library(readr) # read_csv, write_csv
library(dplyr) # mutate, filter, transmute, recode, etc.
library(psych) # corr.test, alpha, fa, fa.parallel
library(ggplot2) # plots + ggsave
library(tidyr) # pivot_longer
library(knitr) # kable
library(kableExtra) # styling + save_kable
library(tibble) # rownames_to_column

# 1. reading the data again

ds_cleaned <- read_csv("data_frost_cleaned.csv", show_col_types = FALSE)

# 2. getting our folders

dir.create("Descriptives", showWarnings = FALSE)

dir.create("Descriptives/Tables", recursive = TRUE, showWarnings = FALSE)
dir.create(
  "Descriptives/Tables/Response rate",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Tables/Correlations",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Tables/Cronbach's alpha",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Tables/EFA",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "Descriptives/Figures",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Figures/Response rate",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Figures/Correlations",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Figures/Cronbach's alpha",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Figures/EFA",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Figures/Misc",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "Descriptives/Text outputs",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Text outputs/Correlations",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Text outputs/Cronbach's alpha",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "Descriptives/Text outputs/EFA",
  recursive = TRUE,
  showWarnings = FALSE
)

# 3. more helper functions

# response rates:
# we get feedback on total rows in dataset, count of non-missing responses per variable
# and % of non-missing responses per variable
response_rates <- function(data, vars) {
  tibble(
    variable = vars,
    n_total = nrow(data),
    n_nonmissing = sapply(data[vars], function(x) sum(!is.na(x))),
    pct_nonmissing = round(100 * n_nonmissing / n_total, 1)
  )
}

# small helper for exporting html tables
save_html_table <- function(tbl, html_file, caption_text) {
  tbl %>%
    kable(format = "html", caption = caption_text) %>%
    kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "condensed")
    ) %>%
    column_spec(1, bold = TRUE) %>%
    save_kable(file = html_file)
}

# helper for significance stars in tables / heatmaps
sig_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ ".", # because why not
    TRUE ~ ""
  )
}

# helper for making a tidy correlation export
make_corr_tidy <- function(corr_obj) {
  r_long <- as.data.frame(corr_obj$r) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "r")

  p_long <- as.data.frame(corr_obj$p) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "p")

  n_long <- as.data.frame(corr_obj$n) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "n")

  r_long %>%
    left_join(p_long, by = c("var1", "var2")) %>%
    left_join(n_long, by = c("var1", "var2")) %>%
    filter(var1 < var2) %>%
    mutate(
      r = round(r, 3),
      p = round(p, 3),
      stars = sig_stars(p),
      r_with_sig = paste0(r, stars)
    ) %>%
    arrange(p)
}

# helper for correlation heatmaps
make_corr_heatmap <- function(corr_obj, title_text) {
  heat_df <- as.data.frame(corr_obj$r) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
    left_join(
      as.data.frame(corr_obj$p) %>%
        rownames_to_column("var1") %>%
        pivot_longer(-var1, names_to = "var2", values_to = "p"),
      by = c("var1", "var2")
    ) %>%
    mutate(
      stars = sig_stars(p),
      label = paste0(round(r, 2), stars),
      var1 = factor(var1, levels = colnames(corr_obj$r)),
      var2 = factor(var2, levels = colnames(corr_obj$r))
    )

  ggplot(heat_df, aes(x = var2, y = var1, fill = r)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = label), size = 3) +
    scale_fill_gradient2(
      low = "#b2182b",
      mid = "white",
      high = "#2166ac",
      limits = c(-1, 1),
      name = "r"
    ) +
    coord_fixed() +
    labs(x = NULL, y = NULL, title = title_text) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

# helper for exporting printed text outputs into plain txt files
save_txt_output <- function(object, file_out) {
  capture.output(object, file = file_out)
}

# helper for saving one plot in multiple colour variants without rewriting the same code 3 times
save_plot_variants <- function(
  plot_default,
  plot_greyscale = NULL,
  plot_bluescale = NULL,
  file_base,
  subfolder = "",
  width = 6,
  height = 4,
  dpi = 300
) {
  prefix <- if (subfolder == "") {
    "Descriptives/Figures"
  } else {
    paste0("Descriptives/Figures/", subfolder)
  }

  # default version
  ggsave(
    paste0(prefix, "/", file_base, ".png"),
    plot_default,
    width = width,
    height = height,
    dpi = dpi
  )

  # greyscale version
  if (!is.null(plot_greyscale)) {
    ggsave(
      paste0(prefix, "/", file_base, "_greyscale.png"),
      plot_greyscale,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  # bluescale version
  if (!is.null(plot_bluescale)) {
    ggsave(
      paste0(prefix, "/", file_base, "_bluescale.png"),
      plot_bluescale,
      width = width,
      height = height,
      dpi = dpi
    )
  }
}

# helper for histogram colour variants
make_hist_variants <- function(base_plot, binwidth = 0.5) {
  list(
    default = base_plot +
      geom_histogram(binwidth = binwidth, color = "white", fill = "grey30"),

    greyscale = base_plot +
      geom_histogram(
        aes(fill = after_stat(x)),
        binwidth = binwidth,
        color = "white"
      ) +
      scale_fill_gradient(
        low = "grey85",
        high = "grey25",
        guide = "none"
      ),

    bluescale = base_plot +
      geom_histogram(
        aes(fill = after_stat(x)),
        binwidth = binwidth,
        color = "white"
      ) +
      scale_fill_gradient(
        low = "#c6dbef",
        high = "#2171b5",
        guide = "none"
      )
  )
}

# helper for bar chart colour variants
make_bar_variants <- function(base_plot) {
  list(
    default = base_plot +
      geom_bar(fill = "grey30"),

    greyscale = base_plot +
      geom_bar(aes(fill = after_stat(count))) +
      scale_fill_gradient(
        low = "grey85",
        high = "grey25",
        guide = "none"
      ),

    bluescale = base_plot +
      geom_bar(aes(fill = after_stat(count))) +
      scale_fill_gradient(
        low = "#c6dbef",
        high = "#2171b5",
        guide = "none"
      )
  )
}

# helper for alpha barplot colour variants
make_alpha_bar_variants <- function(base_plot) {
  list(
    default = base_plot +
      geom_col(fill = "grey30"),

    blue = base_plot +
      geom_col(fill = "#4f81bd")
  )
}

# 4. small prep work for descriptives

# here we create response indicators for country-of-birth questions
# because we have 2 questions on country of birth, right?
# one as a pre-selected list, another as an auto-fill open text answer field with ~203 suggestions
# well, inevitably, some respondents skip the coded list and answer the "other country" open field
# so for response-rate reporting, we count either as a valid response
ds_cleaned <- ds_cleaned %>%
  mutate(
    resp_birth_self = !is.na(ci01) | !is.na(ci02),
    resp_birth_mother = !is.na(ci03) | !is.na(ci04),
    resp_birth_father = !is.na(ci05) | !is.na(ci06)
  )

# a bit of type cleanup for later visuals / correlations
ds_cleaned <- ds_cleaned %>%
  mutate(
    institution_mean = suppressWarnings(as.numeric(institution_mean)),
    institution_mean_minimal = suppressWarnings(as.numeric(
      institution_mean_minimal
    )),
    discrim_mean = suppressWarnings(as.numeric(discrim_mean)),
    pb_total = suppressWarnings(as.numeric(pb_total)),
    pb_citizenship = suppressWarnings(as.numeric(pb_citizenship)),
    pb_belonging = suppressWarnings(as.numeric(pb_belonging)),
    exp_number = suppressWarnings(as.numeric(exp_number)),
    bund_mean = suppressWarnings(as.numeric(bund_mean)),
    council_mean = suppressWarnings(as.numeric(council_mean)),
    police_mean = suppressWarnings(as.numeric(police_mean)),
    court_mean = suppressWarnings(as.numeric(court_mean)),
    media_mean = suppressWarnings(as.numeric(media_mean)),
    immigr_mean = suppressWarnings(as.numeric(immigr_mean)),
    citizens_mean = suppressWarnings(as.numeric(citizens_mean))
  )

# numeric helper version of migration generation for broad descriptive correlations only
# this is not exactly theoretically sound, but I'm doing it for comparison's sake
ds_cleaned <- ds_cleaned %>%
  mutate(
    migration_generation_num = case_when(
      migration_generation == "Non-migrant" ~ 0,
      migration_generation == "2nd gen" ~ 1,
      migration_generation == "1st gen" ~ 2,
      TRUE ~ NA_real_
    )
  )

# 5. table 1 - Response rates for key inputs

table1_vars <- c(
  "institution_mean",
  "institution_mean_minimal",
  "german_citizen_binary",
  "resp_birth_self",
  "resp_birth_mother",
  "resp_birth_father"
)

tab1 <- response_rates(ds_cleaned, table1_vars) %>%
  mutate(
    variable = recode(
      variable,
      institution_mean = "Institutional trust (broad mean)",
      institution_mean_minimal = "Institutional trust (minimal mean)",
      german_citizen_binary = "German citizenship (binary)",
      resp_birth_self = "Own country of birth (CI01 or CI02)",
      resp_birth_mother = "Mother's country of birth (CI03 or CI04)",
      resp_birth_father = "Father's country of birth (CI05 or CI06)"
    )
  )

save_html_table(
  tbl = tab1,
  html_file = "Descriptives/Tables/Response rate/table1_response_rates.html",
  caption_text = "Table 1. Response rates (key variables)"
)

# 6. selected correlations - small set for table 2

# table 2 now stays smaller and cleaner
# discrim_mean and pb_total move to the bigger table 3
ds_corr_small <- ds_cleaned %>%
  transmute(
    Trust_broad = institution_mean,
    Trust_minimal = institution_mean_minimal,
    German_citizenship = suppressWarnings(as.numeric(as.character(
      german_citizen_binary
    ))),
    Migration_background = suppressWarnings(as.numeric(as.character(
      migration_background
    )))
  )

csmall <- psych::corr.test(
  ds_corr_small,
  use = "pairwise",
  method = "pearson",
  adjust = "none"
)

# 7. table 2 - small correlation matrix

r_mat_small <- round(csmall$r, 2)
r_tbl_small <- as.data.frame(r_mat_small) %>%
  rownames_to_column("var")

save_html_table(
  tbl = r_tbl_small,
  html_file = "Descriptives/Tables/Correlations/table2_corr_small.html",
  caption_text = "Table 2. Correlations (core variables)"
)

corr_tidy_small <- make_corr_tidy(csmall)

write.table(
  corr_tidy_small,
  file = "Descriptives/Text outputs/Correlations/corr_selected_tidy_small.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

write.table(
  r_tbl_small,
  file = "Descriptives/Text outputs/Correlations/table2_corr_small.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

write_csv(
  corr_tidy_small,
  "Descriptives/Tables/Correlations/corr_selected_tidy_small.csv"
)

save_html_table(
  tbl = corr_tidy_small,
  html_file = "Descriptives/Tables/Correlations/corr_selected_tidy_small.html",
  caption_text = "Table 2a. Tidy correlations (core variables)"
)

p_heat_small <- make_corr_heatmap(
  csmall,
  "Correlation heatmap (core variables)"
)
ggsave(
  "Descriptives/Figures/Correlations/heatmap_corr_selected_small.png",
  p_heat_small,
  width = 7,
  height = 5.5,
  dpi = 300
)

# 8. expanded correlations - bigger set for table 3

# here we build a broader descriptive correlation dataset for notes and writing
ds_corr_big <- ds_cleaned %>%
  transmute(
    Trust_broad = institution_mean,
    Trust_minimal = institution_mean_minimal,
    German_citizenship = suppressWarnings(as.numeric(as.character(
      german_citizen_binary
    ))),
    Migration_background = suppressWarnings(as.numeric(as.character(
      migration_background
    ))),
    Migration_generation = migration_generation_num,
    Discrimination_frequency = discrim_mean,
    Personal_belonging_total = pb_total,
    Belonging_citizenship = pb_citizenship,
    Belonging_sense = pb_belonging,
    Personal_experience_number = exp_number,
    Age = age,
    Gender = suppressWarnings(as.numeric(as.character(gender_binary))),
    University_education = suppressWarnings(as.numeric(as.character(
      uni_binary
    )))
  )

cbig <- psych::corr.test(
  ds_corr_big,
  use = "pairwise",
  method = "pearson",
  adjust = "none"
)

# 9. table 3 - expanded correlation matrix

r_mat_big <- round(cbig$r, 2)
r_tbl_big <- as.data.frame(r_mat_big) %>%
  rownames_to_column("var")

save_html_table(
  tbl = r_tbl_big,
  html_file = "Descriptives/Tables/Correlations/table3_corr_big.html",
  caption_text = "Table 3. Correlations (expanded variables)"
)

corr_tidy_big <- make_corr_tidy(cbig)

write.table(
  corr_tidy_big,
  file = "Descriptives/Text outputs/Correlations/corr_selected_tidy_big.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

write.table(
  r_tbl_big,
  file = "Descriptives/Text outputs/Correlations/table3_corr_big.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

write_csv(
  corr_tidy_big,
  "Descriptives/Tables/Correlations/corr_selected_tidy_big.csv"
)

save_html_table(
  tbl = corr_tidy_big,
  html_file = "Descriptives/Tables/Correlations/corr_selected_tidy_big.html",
  caption_text = "Table 3a. Tidy correlations (expanded variables)"
)

p_heat_big <- make_corr_heatmap(
  cbig,
  "Correlation heatmap (expanded variables)"
)
ggsave(
  "Descriptives/Figures/Correlations/heatmap_corr_selected_big.png",
  p_heat_big,
  width = 9,
  height = 7.5,
  dpi = 300
)

# 10. descriptive plots - samples and responses

# bar chart - migration generation (responses)
bar_gen_base <- ds_cleaned %>%
  filter(!is.na(migration_generation)) %>%
  ggplot(aes(x = migration_generation)) +
  labs(x = "Migration generation", y = "Count")

bar_gen_variants <- make_bar_variants(bar_gen_base)

save_plot_variants(
  plot_default = bar_gen_variants$default,
  plot_greyscale = bar_gen_variants$greyscale,
  plot_bluescale = bar_gen_variants$bluescale,
  file_base = "bar_migration_generation",
  subfolder = "Response rate",
  width = 6,
  height = 4,
  dpi = 300
)

# bar chart - migration background (minus NA)
bar_mig_base <- ds_cleaned %>%
  filter(!is.na(migration_background)) %>%
  ggplot(aes(x = factor(migration_background))) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Migration background", y = "Count")

bar_mig_variants <- make_bar_variants(bar_mig_base)

save_plot_variants(
  plot_default = bar_mig_variants$default,
  plot_greyscale = bar_mig_variants$greyscale,
  plot_bluescale = bar_mig_variants$bluescale,
  file_base = "bar_migration_background",
  subfolder = "Response rate",
  width = 5,
  height = 4,
  dpi = 300
)

# bar chart - citizenship (minus NA)
bar_cit_base <- ds_cleaned %>%
  filter(!is.na(german_citizen_binary)) %>%
  ggplot(aes(x = factor(german_citizen_binary))) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship", y = "Count")

bar_cit_variants <- make_bar_variants(bar_cit_base)

save_plot_variants(
  plot_default = bar_cit_variants$default,
  plot_greyscale = bar_cit_variants$greyscale,
  plot_bluescale = bar_cit_variants$bluescale,
  file_base = "bar_citizenship",
  subfolder = "Response rate",
  width = 5,
  height = 4,
  dpi = 300
)

# 11. descriptive plots - distributions of central variables

# histogram - overall institutional trust (broad)
hist_trust_base <- ds_cleaned %>%
  filter(!is.na(institution_mean)) %>%
  ggplot(aes(x = institution_mean)) +
  labs(x = "Overall institutional trust (broad mean)", y = "Count")

hist_trust_variants <- make_hist_variants(hist_trust_base, binwidth = 0.5)

save_plot_variants(
  plot_default = hist_trust_variants$default,
  plot_greyscale = hist_trust_variants$greyscale,
  plot_bluescale = hist_trust_variants$bluescale,
  file_base = "hist_trust",
  subfolder = "Response rate",
  width = 6,
  height = 4,
  dpi = 300
)

# histogram - overall institutional trust (minimal)
hist_trust_min_base <- ds_cleaned %>%
  filter(!is.na(institution_mean_minimal)) %>%
  ggplot(aes(x = institution_mean_minimal)) +
  labs(x = "Overall institutional trust (minimal mean)", y = "Count")

hist_trust_min_variants <- make_hist_variants(
  hist_trust_min_base,
  binwidth = 0.5
)

save_plot_variants(
  plot_default = hist_trust_min_variants$default,
  plot_greyscale = hist_trust_min_variants$greyscale,
  plot_bluescale = hist_trust_min_variants$bluescale,
  file_base = "hist_trust_minimal",
  subfolder = "Response rate",
  width = 6,
  height = 4,
  dpi = 300
)

# histogram - discrimination
hist_discr_base <- ds_cleaned %>%
  filter(!is.na(discrim_mean)) %>%
  ggplot(aes(x = discrim_mean)) +
  labs(x = "Discrimination (mean)", y = "Count")

hist_discr_variants <- make_hist_variants(hist_discr_base, binwidth = 0.25)

save_plot_variants(
  plot_default = hist_discr_variants$default,
  plot_greyscale = hist_discr_variants$greyscale,
  plot_bluescale = hist_discr_variants$bluescale,
  file_base = "hist_discrimination",
  subfolder = "Response rate",
  width = 6,
  height = 4,
  dpi = 300
)

# histogram - belonging, citizenship subscale
hist_belong_cit_base <- ds_cleaned %>%
  filter(!is.na(pb_citizenship)) %>%
  ggplot(aes(x = pb_citizenship)) +
  labs(x = "Belonging (citizenship subscale mean)", y = "Count")

hist_belong_cit_variants <- make_hist_variants(
  hist_belong_cit_base,
  binwidth = 0.5
)

save_plot_variants(
  plot_default = hist_belong_cit_variants$default,
  plot_greyscale = hist_belong_cit_variants$greyscale,
  plot_bluescale = hist_belong_cit_variants$bluescale,
  file_base = "hist_belonging_citizenship",
  subfolder = "Response rate",
  width = 6,
  height = 4,
  dpi = 300
)

# histogram - belonging, sense of belonging subscale
hist_belong_sense_base <- ds_cleaned %>%
  filter(!is.na(pb_belonging)) %>%
  ggplot(aes(x = pb_belonging)) +
  labs(x = "Belonging (sense of belonging subscale mean)", y = "Count")

hist_belong_sense_variants <- make_hist_variants(
  hist_belong_sense_base,
  binwidth = 0.5
)

save_plot_variants(
  plot_default = hist_belong_sense_variants$default,
  plot_greyscale = hist_belong_sense_variants$greyscale,
  plot_bluescale = hist_belong_sense_variants$bluescale,
  file_base = "hist_belonging_sense",
  subfolder = "Response rate",
  width = 6,
  height = 4,
  dpi = 300
)

# histogram - total personal belonging
hist_belong_total_base <- ds_cleaned %>%
  filter(!is.na(pb_total)) %>%
  ggplot(aes(x = pb_total)) +
  labs(x = "Total personal belonging", y = "Count")

hist_belong_total_variants <- make_hist_variants(
  hist_belong_total_base,
  binwidth = 1
)

save_plot_variants(
  plot_default = hist_belong_total_variants$default,
  plot_greyscale = hist_belong_total_variants$greyscale,
  plot_bluescale = hist_belong_total_variants$bluescale,
  file_base = "hist_belonging_total",
  subfolder = "Response rate",
  width = 6,
  height = 4,
  dpi = 300
)

# histogram - personal experience frequency
hist_exp_base <- ds_cleaned %>%
  filter(!is.na(exp_number)) %>%
  ggplot(aes(x = exp_number)) +
  labs(x = "Personal experience number", y = "Count")

hist_exp_variants <- make_hist_variants(hist_exp_base, binwidth = 1)

save_plot_variants(
  plot_default = hist_exp_variants$default,
  plot_greyscale = hist_exp_variants$greyscale,
  plot_bluescale = hist_exp_variants$bluescale,
  file_base = "hist_exp_number",
  subfolder = "Response rate",
  width = 6,
  height = 4,
  dpi = 300
)

# 12. descriptive plots - group comparisons for trust

# boxplot - trust by migration generation (broad)
p_box_gen <- ds_cleaned %>%
  filter(!is.na(migration_generation), !is.na(institution_mean)) %>%
  ggplot(aes(x = migration_generation, y = institution_mean)) +
  geom_boxplot() +
  labs(
    x = "Migration generation",
    y = "Overall institutional trust (broad mean)"
  )

ggsave(
  "Descriptives/Figures/Correlations/box_trust_by_migration_generation.png",
  p_box_gen,
  width = 6.5,
  height = 4,
  dpi = 300
)

# boxplot - minimal trust by migration generation
p_box_gen_min <- ds_cleaned %>%
  filter(!is.na(migration_generation), !is.na(institution_mean_minimal)) %>%
  ggplot(aes(x = migration_generation, y = institution_mean_minimal)) +
  geom_boxplot() +
  labs(x = "Migration generation", y = "Institutional trust (minimal mean)")

ggsave(
  "Descriptives/Figures/Correlations/box_trust_minimal_by_migration_generation.png",
  p_box_gen_min,
  width = 6.5,
  height = 4,
  dpi = 300
)

# boxplot - trust by citizenship, faceted by migration background
p_main <- ds_cleaned %>%
  filter(
    !is.na(german_citizen_binary),
    !is.na(migration_background),
    !is.na(institution_mean)
  ) %>%
  ggplot(aes(x = factor(german_citizen_binary), y = institution_mean)) +
  geom_boxplot() +
  facet_wrap(
    ~ factor(
      migration_background,
      levels = c(0, 1),
      labels = c("Non-migrant", "Migrant")
    )
  ) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship", y = "Overall institutional trust (broad mean)")

ggsave(
  "Descriptives/Figures/Correlations/plot_trust_by_citizenship_facet_migrationbg.png",
  p_main,
  width = 8,
  height = 4.5,
  dpi = 300
)

# boxplot - minimal trust by citizenship, faceted by migration background
p_main_min <- ds_cleaned %>%
  filter(
    !is.na(german_citizen_binary),
    !is.na(migration_background),
    !is.na(institution_mean_minimal)
  ) %>%
  ggplot(aes(x = factor(german_citizen_binary), y = institution_mean_minimal)) +
  geom_boxplot() +
  facet_wrap(
    ~ factor(
      migration_background,
      levels = c(0, 1),
      labels = c("Non-migrant", "Migrant")
    )
  ) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship", y = "Institutional trust (minimal mean)")

ggsave(
  "Descriptives/Figures/Correlations/plot_trust_minimal_by_citizenship_facet_migrationbg.png",
  p_main_min,
  width = 8,
  height = 4.5,
  dpi = 300
)

# boxplot - trust by citizenship, faceted by migration generation
p_cit_x_gen <- ds_cleaned %>%
  filter(
    !is.na(migration_generation),
    !is.na(german_citizen_binary),
    !is.na(institution_mean)
  ) %>%
  ggplot(aes(x = factor(german_citizen_binary), y = institution_mean)) +
  geom_boxplot() +
  facet_wrap(~migration_generation) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship", y = "Overall institutional trust (broad mean)")

ggsave(
  "Descriptives/Figures/Correlations/box_trust_by_citizenship_facet_migration_generation.png",
  p_cit_x_gen,
  width = 9,
  height = 4.5,
  dpi = 300
)

# 13. descriptive plots - bivariate scatterplots

# scatterplot - broad trust vs minimal trust
p_scatter_trust_compare <- ds_cleaned %>%
  filter(!is.na(institution_mean), !is.na(institution_mean_minimal)) %>%
  ggplot(aes(x = institution_mean_minimal, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Institutional trust (minimal mean)",
    y = "Institutional trust (broad mean)"
  )

ggsave(
  "Descriptives/Figures/Correlations/scatter_trust_broad_vs_minimal.png",
  p_scatter_trust_compare,
  width = 6,
  height = 4,
  dpi = 300
)

# scatterplot - trust vs discrimination
p_scatter_discr <- ds_cleaned %>%
  filter(!is.na(discrim_mean), !is.na(institution_mean)) %>%
  ggplot(aes(x = discrim_mean, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Discrimination (mean)",
    y = "Overall institutional trust (broad mean)"
  )

ggsave(
  "Descriptives/Figures/Correlations/scatter_trust_vs_discrimination.png",
  p_scatter_discr,
  width = 6,
  height = 4,
  dpi = 300
)

# scatterplot - minimal trust vs discrimination
p_scatter_discr_min <- ds_cleaned %>%
  filter(!is.na(discrim_mean), !is.na(institution_mean_minimal)) %>%
  ggplot(aes(x = discrim_mean, y = institution_mean_minimal)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Discrimination (mean)", y = "Institutional trust (minimal mean)")

ggsave(
  "Descriptives/Figures/Correlations/scatter_trust_minimal_vs_discrimination.png",
  p_scatter_discr_min,
  width = 6,
  height = 4,
  dpi = 300
)

# scatterplot - trust vs citizenship-related belonging
p_scatter_belong_citizenship <- ds_cleaned %>%
  filter(!is.na(pb_citizenship), !is.na(institution_mean)) %>%
  ggplot(aes(x = pb_citizenship, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Belonging (citizenship subscale mean)",
    y = "Overall institutional trust (broad mean)"
  )

ggsave(
  "Descriptives/Figures/Correlations/scatter_trust_vs_belonging_citizenship.png",
  p_scatter_belong_citizenship,
  width = 6,
  height = 4,
  dpi = 300
)

# scatterplot - trust vs sense of belonging
p_scatter_belong_sense <- ds_cleaned %>%
  filter(!is.na(pb_belonging), !is.na(institution_mean)) %>%
  ggplot(aes(x = pb_belonging, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Belonging (sense of belonging subscale mean)",
    y = "Overall institutional trust (broad mean)"
  )

ggsave(
  "Descriptives/Figures/Correlations/scatter_trust_vs_belonging_sense.png",
  p_scatter_belong_sense,
  width = 6,
  height = 4,
  dpi = 300
)

# scatterplot - trust vs total personal belonging
p_scatter_pbtotal <- ds_cleaned %>%
  filter(!is.na(pb_total), !is.na(institution_mean)) %>%
  ggplot(aes(x = pb_total, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Total personal belonging",
    y = "Overall institutional trust (broad mean)"
  )

ggsave(
  "Descriptives/Figures/Correlations/scatter_trust_vs_pb_total.png",
  p_scatter_pbtotal,
  width = 6,
  height = 4,
  dpi = 300
)

# scatterplot - trust vs personal experience number
p_scatter_expfreq <- ds_cleaned %>%
  filter(!is.na(exp_number), !is.na(institution_mean)) %>%
  ggplot(aes(x = exp_number, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Personal experience number",
    y = "Overall institutional trust (broad mean)"
  )

ggsave(
  "Descriptives/Figures/Correlations/scatter_trust_vs_exp_number.png",
  p_scatter_expfreq,
  width = 6,
  height = 4,
  dpi = 300
)

# 14. descriptive plots - institution-specific trust

# institution-specific trust distributions (boxplots, flipped)
inst_long <- ds_cleaned %>%
  select(
    bund_mean,
    council_mean,
    police_mean,
    court_mean,
    media_mean,
    immigr_mean,
    citizens_mean
  ) %>%
  pivot_longer(everything(), names_to = "institution", values_to = "trust") %>%
  filter(!is.na(trust)) %>%
  mutate(
    institution = recode(
      institution,
      bund_mean = "Bundestag",
      council_mean = "Local council",
      police_mean = "Police",
      court_mean = "Courts",
      media_mean = "Public media",
      immigr_mean = "Immigration office",
      citizens_mean = "Citizens' office"
    )
  )

p_inst <- ggplot(inst_long, aes(x = institution, y = trust)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL, y = "Trust (mean)")

ggsave(
  "Descriptives/Figures/Correlations/box_trust_by_institution.png",
  p_inst,
  width = 7,
  height = 4.5,
  dpi = 300
)

# 15. descriptive plots - trust by institution x personal experience with that institution

# here we match each institution-specific trust mean to the corresponding personal experience binary
inst_exp_long <- bind_rows(
  ds_cleaned %>%
    transmute(
      institution = "Bundestag",
      trust = bund_mean,
      experience = pe01_exp_binary
    ),
  ds_cleaned %>%
    transmute(
      institution = "Local council",
      trust = council_mean,
      experience = pe02_exp_binary
    ),
  ds_cleaned %>%
    transmute(
      institution = "Police",
      trust = police_mean,
      experience = pe03_exp_binary
    ),
  ds_cleaned %>%
    transmute(
      institution = "Courts",
      trust = court_mean,
      experience = pe04_exp_binary
    ),
  ds_cleaned %>%
    transmute(
      institution = "Public media",
      trust = media_mean,
      experience = pe05_exp_binary
    ),
  ds_cleaned %>%
    transmute(
      institution = "Immigration office",
      trust = immigr_mean,
      experience = pe06_exp_binary
    ),
  ds_cleaned %>%
    transmute(
      institution = "Citizens' office",
      trust = citizens_mean,
      experience = pe07_exp_binary
    )
) %>%
  filter(!is.na(trust), !is.na(experience)) %>%
  mutate(
    experience = factor(
      experience,
      levels = c(0, 1),
      labels = c("No experience", "Had experience")
    )
  )

# boxplot: trust by personal experience, faceted by institution
p_inst_exp <- ggplot(inst_exp_long, aes(x = experience, y = trust)) +
  geom_boxplot() +
  facet_wrap(~institution) +
  labs(
    x = "Personal experience with institution",
    y = "Trust (mean)"
  ) +
  theme_minimal()

ggsave(
  "Descriptives/Figures/Correlations/box_trust_by_institution_x_experience.png",
  p_inst_exp,
  width = 10,
  height = 6,
  dpi = 300
)

# 16. scale checks - Cronbach's alpha

# Cronbach's alpha as a simple reliability check to see if the items in a battery seem to hang together as one scale
# is it better than EFA? I don't know, but I read that it's simpler and may just work better for our low-N pool
# we use it here mainly for trust and personal belonging

# trust items
trust_items <- ds_cleaned %>%
  select(matches("^in0[1-8]_")) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

alpha_trust <- psych::alpha(trust_items)
save_txt_output(
  alpha_trust,
  "Descriptives/Text outputs/Cronbach's alpha/alpha_trust.txt"
)

# personal belonging items - all 5 together
pb_items <- ds_cleaned %>%
  select(any_of(c("pb01_01", "pb01_02", "pb01_03", "pb01_04", "pb01_05"))) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

alpha_pb_total <- psych::alpha(pb_items)
save_txt_output(
  alpha_pb_total,
  "Descriptives/Text outputs/Cronbach's alpha/alpha_pb_total.txt"
)

# personal belonging, citizenship subscale
pb_cit_items <- ds_cleaned %>%
  select(any_of(c("pb01_01", "pb01_03"))) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

alpha_pb_cit <- psych::alpha(pb_cit_items)
save_txt_output(
  alpha_pb_cit,
  "Descriptives/Text outputs/Cronbach's alpha/alpha_pb_citizenship.txt"
)

# personal belonging, sense of belonging subscale
pb_belong_items <- ds_cleaned %>%
  select(any_of(c("pb01_02", "pb01_04", "pb01_05"))) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

alpha_pb_belong <- psych::alpha(pb_belong_items)
save_txt_output(
  alpha_pb_belong,
  "Descriptives/Text outputs/Cronbach's alpha/alpha_pb_belonging.txt"
)

# discrimination items - optional, but useful to see whether the frequency battery hangs together
discrim_items <- ds_cleaned %>%
  select(matches("^di01_0?[1-7]$")) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

alpha_discrim <- psych::alpha(discrim_items)
save_txt_output(
  alpha_discrim,
  "Descriptives/Text outputs/Cronbach's alpha/alpha_discrimination.txt"
)

# make a small alpha summary table so the text outputs also have a visual representation
alpha_summary <- tibble(
  Scale = c(
    "Trust battery",
    "Personal belonging (all 5 items)",
    "PB citizenship subscale",
    "PB belonging subscale",
    "Discrimination battery"
  ),
  Alpha = round(
    c(
      alpha_trust$total$raw_alpha,
      alpha_pb_total$total$raw_alpha,
      alpha_pb_cit$total$raw_alpha,
      alpha_pb_belong$total$raw_alpha,
      alpha_discrim$total$raw_alpha
    ),
    3
  )
)

save_html_table(
  tbl = alpha_summary,
  html_file = "Descriptives/Tables/Cronbach's alpha/alpha_summary.html",
  caption_text = "Table 4. Cronbach's alpha summary"
)

# also make visual versions as bar charts
alpha_base <- ggplot(
  alpha_summary,
  aes(x = reorder(Scale, Alpha), y = Alpha)
) +
  coord_flip() +
  labs(x = NULL, y = "Cronbach's alpha", title = "Reliability summary") +
  theme_minimal() +
  theme(legend.position = "none")

alpha_variants <- make_alpha_bar_variants(alpha_base)

ggsave(
  "Descriptives/Figures/Cronbach's alpha/alpha_summary_barplot.png",
  alpha_variants$default,
  width = 7,
  height = 4.5,
  dpi = 300
)

ggsave(
  "Descriptives/Figures/Cronbach's alpha/alpha_summary_barplot_blue.png",
  alpha_variants$blue,
  width = 7,
  height = 4.5,
  dpi = 300
)

# 16. scale checks - EFA

# this is kind of backwards-tracking, but since we already saw using correlations that 2 & 3 personal belonging items
# are kind of separate, it would be good to have "proper" empirical justification as to why we split the battery for the regression

# EFA for personal belonging items
png(
  "Descriptives/Figures/EFA/efa_parallel_pb.png",
  width = 900,
  height = 700
)
fa.parallel(
  pb_items,
  fa = "fa",
  fm = "minres",
  main = "Parallel analysis: personal belonging items"
)
dev.off()

# one-factor solution
efa_pb_1 <- fa(pb_items, nfactors = 1, rotate = "oblimin", fm = "minres")
save_txt_output(
  efa_pb_1,
  "Descriptives/Text outputs/EFA/efa_pb_1factor.txt"
)

# two-factor solution - this is the key comparison for our split battery idea
efa_pb_2 <- fa(pb_items, nfactors = 2, rotate = "oblimin", fm = "minres")
save_txt_output(
  efa_pb_2,
  "Descriptives/Text outputs/EFA/efa_pb_2factor.txt"
)

# visual summary tables for EFA PB loadings
efa_pb_1_tbl <- as.data.frame(unclass(efa_pb_1$loadings)) %>%
  rownames_to_column("item") %>%
  mutate(across(-item, ~ round(., 3)))

efa_pb_2_tbl <- as.data.frame(unclass(efa_pb_2$loadings)) %>%
  rownames_to_column("item") %>%
  mutate(across(-item, ~ round(., 3)))

save_html_table(
  tbl = efa_pb_1_tbl,
  html_file = "Descriptives/Tables/EFA/efa_pb_1factor.html",
  caption_text = "Table 5. EFA loadings for personal belonging items (1-factor)"
)

save_html_table(
  tbl = efa_pb_2_tbl,
  html_file = "Descriptives/Tables/EFA/efa_pb_2factor.html",
  caption_text = "Table 6. EFA loadings for personal belonging items (2-factor)"
)

# also EFA for trust items
png(
  "Descriptives/Figures/EFA/efa_parallel_trust.png",
  width = 900,
  height = 700
)
fa.parallel(
  trust_items,
  fa = "fa",
  fm = "minres",
  main = "Parallel analysis: trust items"
)
dev.off()

# one-factor solution - probably useful if we want to justify one overall trust measure
efa_trust_1 <- fa(trust_items, nfactors = 1, rotate = "oblimin", fm = "minres")
save_txt_output(
  efa_trust_1,
  "Descriptives/Text outputs/EFA/efa_trust_1factor.txt"
)

# two-factor solution - probably useful as a comparison if the trust battery does not look clearly one-dimensional
efa_trust_2 <- fa(trust_items, nfactors = 2, rotate = "oblimin", fm = "minres")
save_txt_output(
  efa_trust_2,
  "Descriptives/Text outputs/EFA/efa_trust_2factor.txt"
)

# visual summary tables for EFA trust loadings
efa_trust_1_tbl <- as.data.frame(unclass(efa_trust_1$loadings)) %>%
  rownames_to_column("item") %>%
  mutate(across(-item, ~ round(., 3)))

efa_trust_2_tbl <- as.data.frame(unclass(efa_trust_2$loadings)) %>%
  rownames_to_column("item") %>%
  mutate(across(-item, ~ round(., 3)))

save_html_table(
  tbl = efa_trust_1_tbl,
  html_file = "Descriptives/Tables/EFA/efa_trust_1factor.html",
  caption_text = "Table 7. EFA loadings for trust items (1-factor)"
)

save_html_table(
  tbl = efa_trust_2_tbl,
  html_file = "Descriptives/Tables/EFA/efa_trust_2factor.html",
  caption_text = "Table 8. EFA loadings for trust items (2-factor)"
)

# 17. mystery graph - here be dragons

frost_grade <- tibble(
  stage = c("After", "Before"),
  value = c(1.0, 5.0)
)

p_grade <- ggplot(frost_grade, aes(x = stage, y = value, fill = stage)) +
  geom_col() +
  labs(
    title = "FROST grade before vs after this graph",
    x = NULL,
    y = "Grade"
  ) +
  scale_fill_manual(values = c("Before" = "grey70", "After" = "deeppink")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

ggsave(
  "Descriptives/Figures/Misc/FROST_grade_plot.png",
  p_grade,
  width = 5,
  height = 3.5,
  dpi = 300
)
