library(readr)      # read_csv, write_csv
library(dplyr)      # mutate, filter, transmute, recode, etc.
library(psych)      # corr.test
library(ggplot2)    # plots + ggsave
library(tidyr)      # pivot_longer
library(knitr)      # kable
library(kableExtra) # styling + save_kable
library(webshot2)   # HTML -> PNG screenshots (for PPT)
library(tibble)     # rownames_to_column

# -----------------------------
# Load data
# -----------------------------
ds_cleaned <- read_csv("data_frost_cleaned.csv", show_col_types = FALSE)

# -----------------------------
# Output folders (Tables + Figures)
# -----------------------------
dir.create("Visuals", showWarnings = FALSE)
dir.create("Visuals/Tables", recursive = TRUE, showWarnings = FALSE)
dir.create("Visuals/Figures", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Helper function: response rates
# What it does:
#   For a list of variables, it reports:
#     - total rows in dataset
#     - count of non-missing responses per variable
#     - % non-missing responses per variable
# ============================================================
response_rates <- function(data, vars) {
  tibble(
    variable = vars,
    n_total = nrow(data),
    n_nonmissing = sapply(data[vars], function(x) sum(!is.na(x))),
    pct_nonmissing = round(100 * n_nonmissing / n_total, 1)
  )
}

# ============================================================
# 1) Create response indicators for country-of-birth questions
# Why:
#   Some respondents skip the coded list (CI01/CI03/CI05) and answer
#   the "other country" open field (CI02/CI04/CI06).
#   For response-rate reporting, we count either as a valid response.
# ============================================================
ds_cleaned <- ds_cleaned %>%
  mutate(
    resp_birth_self   = !is.na(ci01) | !is.na(ci02),
    resp_birth_mother = !is.na(ci03) | !is.na(ci04),
    resp_birth_father = !is.na(ci05) | !is.na(ci06)
  )

# ============================================================
# 2) TABLE 1 (HTML + PNG): Response rates for key inputs
# ============================================================
table1_vars <- c(
  "institution_mean",
  "german_citizen_binary",
  "resp_birth_self", "resp_birth_mother", "resp_birth_father"
)

tab1 <- response_rates(ds_cleaned, table1_vars) %>%
  mutate(variable = recode(variable,
                           institution_mean = "Institutional trust (mean)",
                           german_citizen_binary = "German citizenship (binary)",
                           resp_birth_self = "Own country of birth (CI01 or CI02)",
                           resp_birth_mother = "Mother's country of birth (CI03 or CI04)",
                           resp_birth_father = "Father's country of birth (CI05 or CI06)"
  ))

f_tab1_html <- "Visuals/Tables/table1_response_rates.html"
f_tab1_png  <- "Visuals/Tables/table1_response_rates.png"

tab1 %>%
  kable(format = "html", caption = "Table 1. Response rates (key variables)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "condensed")) %>%
  column_spec(1, bold = TRUE) %>%
  save_kable(file = f_tab1_html)

# Convert the HTML table into a PNG image (easy to paste into PowerPoint)
webshot2::webshot(url = f_tab1_html, file = f_tab1_png, zoom = 2)

# ============================================================
# 3) TABLE 2 (HTML + PNG): Correlation matrix (selected variables)
# Notes:
#   - corr.test requires numeric columns.
#   - binary vars stored as "0"/"1" might come in as character/factor;
#     using as.numeric(as.character(...)) avoids_cleaned 1/2 factor coding.
# ============================================================
ds_corr_small <- ds_cleaned %>%
  transmute(
    Trust = suppressWarnings(as.numeric(institution_mean)),
    Citizenship = suppressWarnings(as.numeric(as.character(german_citizen_binary))),
    MigrationBG = suppressWarnings(as.numeric(as.character(migration_background))),
    Belonging = suppressWarnings(as.numeric(pb_belonging)),
    Discrimination = suppressWarnings(as.numeric(discrim_mean))
  )

csmall <- psych::corr.test(ds_corr_small, use = "pairwise", method = "pearson", adjust = "none")

# Display matrix for table export
r_mat <- round(csmall$r, 2)
r_tbl <- as.data.frame(r_mat) %>% tibble::rownames_to_column("var")

f_tab2_html <- "Visuals/Tables/table2_corr_small.html"
f_tab2_png  <- "Visuals/Tables/table2_corr_small.png"

r_tbl %>%
  kable(format = "html", caption = "Table 2. Correlations (selected variables)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "condensed")) %>%
  column_spec(1, bold = TRUE) %>%
  save_kable(file = f_tab2_html)

webshot2::webshot(url = f_tab2_html, file = f_tab2_png, zoom = 2)

# ============================================================
# 4) Export a tidy correlation list (most interpretable for notes)
# Output columns: var1, var2, r, p, n, stars, r_with_sig
# ============================================================
r_long <- as.data.frame(csmall$r) %>%
  tibble::rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r")

p_long <- as.data.frame(csmall$p) %>%
  tibble::rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "p")

n_long <- as.data.frame(csmall$n) %>%
  tibble::rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "n")

corr_tidy <- r_long %>%
  left_join(p_long, by = c("var1", "var2")) %>%
  left_join(n_long, by = c("var1", "var2")) %>%
  # keep unique pairs only (upper triangle), avoids_cleaned duplicates
  filter(var1 < var2) %>%
  mutate(
    r = round(r, 3),
    p = round(p, 3),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.1   ~ ".",
      TRUE ~ ""
    ),
    r_with_sig = paste0(r, stars)
  ) %>%
  arrange(p)

write_csv(corr_tidy, "Visuals/Tables/corr_selected_tidy.csv")

# ============================================================
# 5) Correlation heatmap (with significance stars)
# ============================================================
heat_df <- as.data.frame(csmall$r) %>%
  tibble::rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
  left_join(
    as.data.frame(csmall$p) %>%
      tibble::rownames_to_column("var1") %>%
      pivot_longer(-var1, names_to = "var2", values_to = "p"),
    by = c("var1", "var2")
  ) %>%
  mutate(
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.1   ~ ".",
      TRUE ~ ""
    ),
    label = paste0(round(r, 2), stars),
    var1 = factor(var1, levels = colnames(csmall$r)),
    var2 = factor(var2, levels = colnames(csmall$r))
  )

p_heat <- ggplot(heat_df, aes(x = var2, y = var1, fill = r)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = label), size = 3) +
  scale_fill_gradient2(
    low = "#b2182b", mid = "white", high = "#2166ac",
    limits = c(-1, 1), name = "r"
  ) +
  coord_fixed() +
  labs(x = NULL, y = NULL, title = "Correlation heatmap (r, with significance)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave("Visuals/Figures/heatmap_corr_selected.png", p_heat,
       width = 7.5, height = 6, dpi = 300)

# ============================================================
# 6) DESCRIPTIVE PLOTS (all saved as PNG)
# Rule:
#   Always filter out NA values for any variable used in the plot.
# ============================================================

# --- Bar chart: migration generation (responses)
p_bar_gen <- ds_cleaned %>%
  filter(!is.na(migration_generation)) %>%
  ggplot(aes(x = migration_generation)) +
  geom_bar() +
  labs(x = "Migration generation", y = "Count")
ggsave("Visuals/Figures/bar_migration_generation.png", p_bar_gen,
       width = 6, height = 4, dpi = 300)

# --- Boxplot: trust by migration generation
p_box_gen <- ds_cleaned %>%
  filter(!is.na(migration_generation), !is.na(institution_mean)) %>%
  ggplot(aes(x = migration_generation, y = institution_mean)) +
  geom_boxplot() +
  labs(x = "Migration generation", y = "Overall institutional trust (mean)")
ggsave("Visuals/Figures/box_trust_by_migration_generation.png", p_box_gen,
       width = 6.5, height = 4, dpi = 300)

# --- Boxplot: trust by citizenship, faceted by migration background
p_main <- ds_cleaned %>%
  filter(!is.na(german_citizen_binary), !is.na(migration_background), !is.na(institution_mean)) %>%
  ggplot(aes(x = factor(german_citizen_binary), y = institution_mean)) +
  geom_boxplot() +
  facet_wrap(~ factor(migration_background, levels = c(0, 1),
                      labels = c("Non-migrant", "Migrant"))) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship", y = "Overall institutional trust (mean)")
ggsave("Visuals/Figures/plot_trust_by_citizenship_facet_migrationbg.png", p_main,
       width = 8, height = 4.5, dpi = 300)

# --- Boxplot: trust by citizenship, faceted by migration generation
p_cit_x_gen <- ds_cleaned %>%
  filter(!is.na(migration_generation), !is.na(german_citizen_binary), !is.na(institution_mean)) %>%
  ggplot(aes(x = factor(german_citizen_binary), y = institution_mean)) +
  geom_boxplot() +
  facet_wrap(~ migration_generation) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship", y = "Overall institutional trust (mean)")
ggsave("Visuals/Figures/box_trust_by_citizenship_facet_migration_generation.png",
       p_cit_x_gen, width = 9, height = 4.5, dpi = 300)

# --- Histograms
p_hist_trust <- ds_cleaned %>%
  filter(!is.na(institution_mean)) %>%
  ggplot(aes(x = institution_mean)) +
  geom_histogram(binwidth = 0.5, color = "white") +
  labs(x = "Overall institutional trust (mean)", y = "Count")
ggsave("Visuals/Figures/hist_trust.png", p_hist_trust,
       width = 6, height = 4, dpi = 300)

p_hist_discr <- ds_cleaned %>%
  filter(!is.na(discrim_mean)) %>%
  ggplot(aes(x = discrim_mean)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  labs(x = "Discrimination (mean)", y = "Count")
ggsave("Visuals/Figures/hist_discrimination.png", p_hist_discr,
       width = 6, height = 4, dpi = 300)

p_hist_belong_citizenship <- ds_cleaned %>%
  filter(!is.na(pb_citizenship)) %>%
  ggplot(aes(x = pb_citizenship)) +
  geom_histogram(binwidth = 0.5, color = "white") +
  labs(x = "Belonging (citizenship subscale mean)", y = "Count")
ggsave("Visuals/Figures/hist_belonging_citizenship.png", p_hist_belong_citizenship,
       width = 6, height = 4, dpi = 300)

p_hist_belong_sense <- ds_cleaned %>%
  filter(!is.na(pb_belonging)) %>%
  ggplot(aes(x = pb_belonging)) +
  geom_histogram(binwidth = 0.5, color = "white") +
  labs(x = "Belonging (sense of belonging subscale mean)", y = "Count")
ggsave("Visuals/Figures/hist_belonging_sense.png", p_hist_belong_sense,
       width = 6, height = 4, dpi = 300)

# --- Bar charts: migration background / citizenship (exclude NA)
p_bar_mig <- ds_cleaned %>%
  filter(!is.na(migration_background)) %>%
  ggplot(aes(x = factor(migration_background))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Migration background", y = "Count")
ggsave("Visuals/Figures/bar_migration_background.png", p_bar_mig,
       width = 5, height = 4, dpi = 300)

p_bar_cit <- ds_cleaned %>%
  filter(!is.na(german_citizen_binary)) %>%
  ggplot(aes(x = factor(german_citizen_binary))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship", y = "Count")
ggsave("Visuals/Figures/bar_citizenship.png", p_bar_cit,
       width = 5, height = 4, dpi = 300)

# mystery graph
p_grade <- ggplot(frost_grade, aes(x = stage, y = value, fill = stage)) +
  geom_col() +
  labs(
    title = "FROST grade before vs after this graph",
    x = NULL, y = "Grade"
  ) +
  scale_fill_manual(values = c("Before" = "grey70", "After" = "deeppink")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

ggsave("Visuals/Figures/FROST_grade_plot.png", p_fun, width = 5, height = 3.5, dpi = 30)

# --- Scatterplots: trust vs discrimination / belonging (exclude NA in x and y)
p_scatter_discr <- ds_cleaned %>%
  filter(!is.na(discrim_mean), !is.na(institution_mean)) %>%
  ggplot(aes(x = discrim_mean, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Discrimination (mean)", y = "Overall institutional trust (mean)")
ggsave("Visuals/Figures/scatter_trust_vs_discrimination.png", p_scatter_discr,
       width = 6, height = 4, dpi = 300)

p_scatter_belong_citizenship <- ds_cleaned %>%
  filter(!is.na(pb_citizenship), !is.na(institution_mean)) %>%
  ggplot(aes(x = pb_citizenship, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Belonging (citizenship subscale mean)", y = "Overall institutional trust (mean)")
ggsave("Visuals/Figures/scatter_trust_vs_belonging_citizenship.png", p_scatter_belong_citizenship,
       width = 6, height = 4, dpi = 300)

p_scatter_belong_sense <- ds_cleaned %>%
  filter(!is.na(pb_belonging), !is.na(institution_mean)) %>%
  ggplot(aes(x = pb_belonging, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Belonging (sense of belonging subscale mean)", y = "Overall institutional trust (mean)")
ggsave("Visuals/Figures/scatter_trust_vs_belonging_sense.png", p_scatter_belong_sense,
       width = 6, height = 4, dpi = 300)

p_scatter_pbtotal <- ds_cleaned %>%
  filter(!is.na(pb_total), !is.na(institution_mean)) %>%
  ggplot(aes(x = pb_total, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Total personal belonging", y = "Overall institutional trust (mean)")
ggsave("Visuals/Figures/scatter_trust_vs_pb_total.png", p_scatter_pbtotal,
       width = 6, height = 4, dpi = 300)

# --- Institution-specific trust distributions (boxplots, flipped)
inst_long <- ds_cleaned %>%
  select(bund_mean, council_mean, police_mean, court_mean, media_mean, immigr_mean, citizens_mean) %>%
  pivot_longer(everything(), names_to = "institution", values_to = "trust") %>%
  filter(!is.na(trust)) %>%
  mutate(institution = recode(institution,
                              bund_mean = "Bundestag",
                              council_mean = "Local council",
                              police_mean = "Police",
                              court_mean = "Courts",
                              media_mean = "Public media",
                              immigr_mean = "Immigration office",
                              citizens_mean = "Citizens' office"
  ))

p_inst <- ggplot(inst_long, aes(x = institution, y = trust)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL, y = "Trust (mean)")
ggsave("Visuals/Figures/box_trust_by_institution.png", p_inst,
       width = 7, height = 4.5, dpi = 300)