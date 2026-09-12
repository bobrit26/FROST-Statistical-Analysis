library(readr) # read_csv, write_csv
library(dplyr) # mutate, filter, transmute, recode, etc.
library(psych) # corr.test, alpha, fa, fa.parallel
library(ggplot2) # plots + ggsave
library(tidyr) # pivot_longer
library(knitr) # kable
library(kableExtra) # styling + save_kable
library(webshot2) # HTML -> PNG screenshots (for PPT)
library(tibble) # rownames_to_column


# 1. reading the data again

ds_cleaned <- read_csv("data_frost_cleaned.csv", show_col_types = FALSE)


# 2. getting our folders

dir.create("Visuals", showWarnings = FALSE)
dir.create("Visuals/Tables", recursive = TRUE, showWarnings = FALSE)
dir.create("Visuals/Figures", recursive = TRUE, showWarnings = FALSE)


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

# small helper for exporting html tables as pngs
save_html_table_png <- function(tbl, html_file, png_file, caption_text) {
  tbl %>%
    kable(format = "html", caption = caption_text) %>%
    kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "condensed")
    ) %>%
    column_spec(1, bold = TRUE) %>%
    save_kable(file = html_file)

  webshot2::webshot(url = html_file, file = png_file, zoom = 2)
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
    discrim_mean = suppressWarnings(as.numeric(discrim_mean)),
    pb_total = suppressWarnings(as.numeric(pb_total)),
    pb_citizenship = suppressWarnings(as.numeric(pb_citizenship)),
    pb_belonging = suppressWarnings(as.numeric(pb_belonging)),
    bund_mean = suppressWarnings(as.numeric(bund_mean)),
    council_mean = suppressWarnings(as.numeric(council_mean)),
    police_mean = suppressWarnings(as.numeric(police_mean)),
    court_mean = suppressWarnings(as.numeric(court_mean)),
    media_mean = suppressWarnings(as.numeric(media_mean)),
    immigr_mean = suppressWarnings(as.numeric(immigr_mean)),
    citizens_mean = suppressWarnings(as.numeric(citizens_mean))
  )


# 5. table 1 - Response rates for key inputs

table1_vars <- c(
  "institution_mean",
  "german_citizen_binary",
  "resp_birth_self",
  "resp_birth_mother",
  "resp_birth_father"
)

tab1 <- response_rates(ds_cleaned, table1_vars) %>%
  mutate(
    variable = recode(
      variable,
      institution_mean = "Institutional trust (mean)",
      german_citizen_binary = "German citizenship (binary)",
      resp_birth_self = "Own country of birth (CI01 or CI02)",
      resp_birth_mother = "Mother's country of birth (CI03 or CI04)",
      resp_birth_father = "Father's country of birth (CI05 or CI06)"
    )
  )

save_html_table_png(
  tbl = tab1,
  html_file = "Visuals/Tables/table1_response_rates.html",
  png_file = "Visuals/Tables/table1_response_rates.png",
  caption_text = "Table 1. Response rates (key variables)"
)


# 6. selected correlations

# here we build a small correlation dataset for our main variables of interest
ds_corr_small <- ds_cleaned %>%
  transmute(
    Trust = suppressWarnings(as.numeric(institution_mean)),
    Citizenship = suppressWarnings(as.numeric(as.character(
      german_citizen_binary
    ))),
    MigrationBG = suppressWarnings(as.numeric(as.character(
      migration_background
    ))),
    Belonging = suppressWarnings(as.numeric(pb_belonging)),
    Discrimination = suppressWarnings(as.numeric(discrim_mean))
  )

csmall <- psych::corr.test(
  ds_corr_small,
  use = "pairwise",
  method = "pearson",
  adjust = "none"
)


# 7. table 2 - correlation matrix

r_mat <- round(csmall$r, 2)
r_tbl <- as.data.frame(r_mat) %>%
  rownames_to_column("var")

save_html_table_png(
  tbl = r_tbl,
  html_file = "Visuals/Tables/table2_corr_small.html",
  png_file = "Visuals/Tables/table2_corr_small.png",
  caption_text = "Table 2. Correlations (selected variables)"
)


# 8) Exporting a correlation list (most interpretable for notes)

r_long <- as.data.frame(csmall$r) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r")

p_long <- as.data.frame(csmall$p) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "p")

n_long <- as.data.frame(csmall$n) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "n")

corr_tidy <- r_long %>%
  left_join(p_long, by = c("var1", "var2")) %>%
  left_join(n_long, by = c("var1", "var2")) %>%
  # keep unique pairs only (upper triangle), avoids cleaned duplicates
  filter(var1 < var2) %>%
  mutate(
    r = round(r, 3),
    p = round(p, 3),
    stars = sig_stars(p),
    r_with_sig = paste0(r, stars)
  ) %>%
  arrange(p)

write_csv(corr_tidy, "Visuals/Tables/corr_selected_tidy.csv")


# 9. correlation heatmap (with significance stars)

heat_df <- as.data.frame(csmall$r) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
  left_join(
    as.data.frame(csmall$p) %>%
      rownames_to_column("var1") %>%
      pivot_longer(-var1, names_to = "var2", values_to = "p"),
    by = c("var1", "var2")
  ) %>%
  mutate(
    stars = sig_stars(p),
    label = paste0(round(r, 2), stars),
    var1 = factor(var1, levels = colnames(csmall$r)),
    var2 = factor(var2, levels = colnames(csmall$r))
  )

p_heat <- ggplot(heat_df, aes(x = var2, y = var1, fill = r)) +
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
  labs(
    x = NULL,
    y = NULL,
    title = "Correlation heatmap (r, with significance)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  "Visuals/Figures/heatmap_corr_selected.png",
  p_heat,
  width = 7.5,
  height = 6,
  dpi = 300
)


# 10. descriptive plots - sample composition and group structure

# bar chart - migration generation (responses)
p_bar_gen <- ds_cleaned %>%
  filter(!is.na(migration_generation)) %>%
  ggplot(aes(x = migration_generation)) +
  geom_bar() +
  labs(x = "Migration generation", y = "Count")

ggsave(
  "Visuals/Figures/bar_migration_generation.png",
  p_bar_gen,
  width = 6,
  height = 4,
  dpi = 300
)

# bar chart - migration background (minus NA)
p_bar_mig <- ds_cleaned %>%
  filter(!is.na(migration_background)) %>%
  ggplot(aes(x = factor(migration_background))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Migration background", y = "Count")

ggsave(
  "Visuals/Figures/bar_migration_background.png",
  p_bar_mig,
  width = 5,
  height = 4,
  dpi = 300
)

# bar chart - citizenship (minus NA)
p_bar_cit <- ds_cleaned %>%
  filter(!is.na(german_citizen_binary)) %>%
  ggplot(aes(x = factor(german_citizen_binary))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship", y = "Count")

ggsave(
  "Visuals/Figures/bar_citizenship.png",
  p_bar_cit,
  width = 5,
  height = 4,
  dpi = 300
)


# 11. descriptive plots - distributions of central variables

# histogram - overall institutional trust
p_hist_trust <- ds_cleaned %>%
  filter(!is.na(institution_mean)) %>%
  ggplot(aes(x = institution_mean)) +
  geom_histogram(binwidth = 0.5, color = "white") +
  labs(x = "Overall institutional trust (mean)", y = "Count")

ggsave(
  "Visuals/Figures/hist_trust.png",
  p_hist_trust,
  width = 6,
  height = 4,
  dpi = 300
)

# histogram - discrimination
p_hist_discr <- ds_cleaned %>%
  filter(!is.na(discrim_mean)) %>%
  ggplot(aes(x = discrim_mean)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  labs(x = "Discrimination (mean)", y = "Count")

ggsave(
  "Visuals/Figures/hist_discrimination.png",
  p_hist_discr,
  width = 6,
  height = 4,
  dpi = 300
)

# histogram - belonging, citizenship subscale
p_hist_belong_citizenship <- ds_cleaned %>%
  filter(!is.na(pb_citizenship)) %>%
  ggplot(aes(x = pb_citizenship)) +
  geom_histogram(binwidth = 0.5, color = "white") +
  labs(x = "Belonging (citizenship subscale mean)", y = "Count")

ggsave(
  "Visuals/Figures/hist_belonging_citizenship.png",
  p_hist_belong_citizenship,
  width = 6,
  height = 4,
  dpi = 300
)

# histogram - belonging, sense of belonging subscale
p_hist_belong_sense <- ds_cleaned %>%
  filter(!is.na(pb_belonging)) %>%
  ggplot(aes(x = pb_belonging)) +
  geom_histogram(binwidth = 0.5, color = "white") +
  labs(x = "Belonging (sense of belonging subscale mean)", y = "Count")

ggsave(
  "Visuals/Figures/hist_belonging_sense.png",
  p_hist_belong_sense,
  width = 6,
  height = 4,
  dpi = 300
)


# 12. descriptive plots - group comparisons for trust

# boxplot - trust by migration generation
p_box_gen <- ds_cleaned %>%
  filter(!is.na(migration_generation), !is.na(institution_mean)) %>%
  ggplot(aes(x = migration_generation, y = institution_mean)) +
  geom_boxplot() +
  labs(x = "Migration generation", y = "Overall institutional trust (mean)")

ggsave(
  "Visuals/Figures/box_trust_by_migration_generation.png",
  p_box_gen,
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
  labs(x = "German citizenship", y = "Overall institutional trust (mean)")

ggsave(
  "Visuals/Figures/plot_trust_by_citizenship_facet_migrationbg.png",
  p_main,
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
  labs(x = "German citizenship", y = "Overall institutional trust (mean)")

ggsave(
  "Visuals/Figures/box_trust_by_citizenship_facet_migration_generation.png",
  p_cit_x_gen,
  width = 9,
  height = 4.5,
  dpi = 300
)


# 13. descriptive plots - bivariate scatterplots

# scatterplot - trust vs discrimination
p_scatter_discr <- ds_cleaned %>%
  filter(!is.na(discrim_mean), !is.na(institution_mean)) %>%
  ggplot(aes(x = discrim_mean, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Discrimination (mean)", y = "Overall institutional trust (mean)")

ggsave(
  "Visuals/Figures/scatter_trust_vs_discrimination.png",
  p_scatter_discr,
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
    y = "Overall institutional trust (mean)"
  )

ggsave(
  "Visuals/Figures/scatter_trust_vs_belonging_citizenship.png",
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
    y = "Overall institutional trust (mean)"
  )

ggsave(
  "Visuals/Figures/scatter_trust_vs_belonging_sense.png",
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
  labs(x = "Total personal belonging", y = "Overall institutional trust (mean)")

ggsave(
  "Visuals/Figures/scatter_trust_vs_pb_total.png",
  p_scatter_pbtotal,
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
  "Visuals/Figures/box_trust_by_institution.png",
  p_inst,
  width = 7,
  height = 4.5,
  dpi = 300
)


# 15. scale checks - Cronbach's alpha

# Cronbach's alpha as a simple reliability check to see if the items in a battery seem to hang together as one scale
# is it better than EFA? I don't know, but I read that it's simpler and may just work better for our low-N pool
# we use it here mainly for trust and personal belonging

# trust items
trust_items <- ds_cleaned %>%
  select(matches("^in0[1-8]_")) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

alpha_trust <- psych::alpha(trust_items)
capture.output(alpha_trust, file = "Visuals/Tables/alpha_trust.txt")

# personal belonging items - all 5 together
pb_items <- ds_cleaned %>%
  select(any_of(c("pb01_01", "pb01_02", "pb01_03", "pb01_04", "pb01_05"))) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

alpha_pb_total <- psych::alpha(pb_items)
capture.output(alpha_pb_total, file = "Visuals/Tables/alpha_pb_total.txt")

# personal belonging, citizenship subscale
pb_cit_items <- ds_cleaned %>%
  select(any_of(c("pb01_01", "pb01_03"))) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

alpha_pb_cit <- psych::alpha(pb_cit_items)
capture.output(alpha_pb_cit, file = "Visuals/Tables/alpha_pb_citizenship.txt")

# personal belonging, sense of belonging subscale
pb_belong_items <- ds_cleaned %>%
  select(any_of(c("pb01_02", "pb01_04", "pb01_05"))) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

alpha_pb_belong <- psych::alpha(pb_belong_items)
capture.output(alpha_pb_belong, file = "Visuals/Tables/alpha_pb_belonging.txt")

# discrimination items - optional, but useful to see whether the frequency battery hangs together
discrim_items <- ds_cleaned %>%
  select(matches("^di01_0?[1-7]$")) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

alpha_discrim <- psych::alpha(discrim_items)
capture.output(alpha_discrim, file = "Visuals/Tables/alpha_discrimination.txt")


# 16. scale checks - EFA

# this is kind of backwards-tracking, but since we already saw using correlations that 2 & 3 personal belonging items
# are kind of separate, it would be good to have "proper" empirical justification as to why we split the battery for the regression

# EFA for personal belonging items

png("Visuals/Figures/efa_parallel_pb.png", width = 900, height = 700)
fa.parallel(
  pb_items,
  fa = "fa",
  fm = "minres",
  main = "Parallel analysis: personal belonging items"
)
dev.off()

# one-factor solution
efa_pb_1 <- fa(pb_items, nfactors = 1, rotate = "oblimin", fm = "minres")
capture.output(efa_pb_1, file = "Visuals/Tables/efa_pb_1factor.txt")

# two-factor solution - this is the key comparison for our split battery idea
efa_pb_2 <- fa(pb_items, nfactors = 2, rotate = "oblimin", fm = "minres")
capture.output(efa_pb_2, file = "Visuals/Tables/efa_pb_2factor.txt")

# also EFA for trust items

# one-factor solution - probably useful if we want to justify one overall trust measure
efa_trust_1 <- fa(trust_items, nfactors = 1, rotate = "oblimin", fm = "minres")
capture.output(efa_trust_1, file = "Visuals/Tables/efa_trust_1factor.txt")

# two-factor solution - probably useful as a comparison if the trust battery does not look clearly one-dimensional
efa_trust_2 <- fa(trust_items, nfactors = 2, rotate = "oblimin", fm = "minres")
capture.output(efa_trust_2, file = "Visuals/Tables/efa_trust_2factor.txt")

# 17. mystery graph - here be dragons

frost_grade <- tibble(
  stage = c("After", "Before"),
  value = c("1,0", "5,0")
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
  "Visuals/Figures/FROST_grade_plot.png",
  p_grade,
  width = 5,
  height = 3.5,
  dpi = 300
)
