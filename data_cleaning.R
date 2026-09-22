library(readxl)
library(readr)
library(janitor)
library(dplyr)

# to truncate values when they are being visualised
options(digits = 3)

# okay, after days-long distractions, I keep forgetting what is what, so for you and for future me...
# 1. reading the data

ds_raw <- read_excel("data_frost.xlsx", sheet = 1) %>% clean_names()
# glimpse(ds_raw)
# summary(ds_raw)

#2. recoding SoSci Survey's special non-answer values to NA
nonresp_num <- c(-1, -9) # numeric missing codes to NA
nonresp_chr <- c("-1", "-9", "") # common string missing signals to NA

recode_special_na <- function(x) {
  if (is.numeric(x)) {
    x[x %in% nonresp_num] <- NA_real_
    return(x)
  } else {
    y <- trimws(as.character(x))
    y[y %in% nonresp_chr] <- NA
    return(y)
  }
}

# safe row aggregation helpers
# return NA if all contributing items are missing
safe_rowmean <- function(df) {
  if_else(
    rowSums(!is.na(df)) > 0,
    rowMeans(df, na.rm = TRUE),
    NA_real_
  )
}

safe_rowsum <- function(df) {
  if_else(
    rowSums(!is.na(df)) > 0,
    rowSums(df, na.rm = TRUE),
    NA_real_
  )
}

ds <- ds_raw %>%
  mutate(across(everything(), recode_special_na)) %>%
  # also, removing completely empty rows
  filter(!if_all(everything(), is.na))

# 3. numeric transition section
ds <- ds %>%
  mutate(
    across(
      c(
        any_of(c(
          "de05_01",
          "de02",
          "de03",
          "ci11",
          "ci15",
          "ci01",
          "ci02",
          "ci03",
          "ci04",
          "ci05",
          "ci06",
          "di03"
        )),
        matches("^(in0[1-8]_|di01_|pb01_)")
      ),
      ~ suppressWarnings(as.numeric(.))
    )
  )

# 4. here we recode the simple stuff

survey_year <- 2026

ds <- ds %>%
  mutate(
    # age from birth year
    age = if_else(!is.na(de05_01), survey_year - de05_01, NA_real_),

    # gender binary
    gender_binary = case_when(
      de02 == 1 ~ 0, # female
      de02 == 2 ~ 1, # male
      TRUE ~ NA_real_
    ),

    # keeping all 3 for descriptive statistics
    gender_categories = factor(
      de02,
      levels = c(1, 2, 3),
      labels = c("Female", "Male", "Diverse")
    ),

    # university education binary
    uni_binary = case_when(
      de03 %in% c(8, 9, 11, 14, 13) ~ 1, # uni
      # 13 = "other", coded as uni, since all (current) cases refer to "Diplom"; will be rechecked later
      de03 %in% c(1, 2, 3, 5, 6) ~ 0, # non-uni
      TRUE ~ NA_real_
    ),

    # length of residence in Germany, collapsed into 4 values
    length_residence_4cats = case_when(
      # yes, cats is a great abbreviation for categories
      ci11 == 1 ~ 1, # 0-5 years, ~15 responses
      ci11 %in% c(2, 3) ~ 2, # 6-15 years, ~15 responses
      ci11 %in% c(4, 5, 6, 7) ~ 3, # 16+ years, ~11 responses
      ci11 == 8 ~ 4, # since birth, ~44 responses
      TRUE ~ NA_real_
    ),

    length_residence_4cats = factor(
      length_residence_4cats,
      levels = c(1, 2, 3, 4),
      labels = c("0-5 years", "6-15 years", "16+ years", "Since birth")
    ),

    # German citizenship binary
    german_citizen_binary = case_when(
      ci15 == 2 ~ 0, # no
      ci15 == 1 ~ 1, # yes
      TRUE ~ NA_real_
    ),

    # member of discriminated group binary
    discriminated_group_binary = case_when(
      di03 == 2 ~ 0, # no
      di03 == 1 ~ 1, # yes
      TRUE ~ NA_real_
    )
  )

#5. here we aggregate scales/indexes

ds <- ds %>%
  mutate(
    # for each institution - average across all available items
    bund_mean = safe_rowmean(pick(matches("^in01_"))), # Bundestag
    council_mean = safe_rowmean(pick(matches("^in02_"))), # local council
    police_mean = safe_rowmean(pick(matches("^in03_"))), # police
    court_mean = safe_rowmean(pick(matches("^in05_"))), # courts
    media_mean = safe_rowmean(pick(matches("^in06_"))), # public media
    immigr_mean = safe_rowmean(pick(matches("^in07_"))), # immigration office
    citizens_mean = safe_rowmean(pick(matches("^in08_"))), # citizens' office

    # final institutional trust average
    institution_mean = safe_rowmean(pick(matches("^in0[1-8]_"))),

    #minimal trust average, aka only the 1st "I trust X" items from each battery
    institution_mean_minimal = safe_rowmean(
      pick(any_of(c(
        "in01_01",
        "in02_01",
        "in03_01",
        "in05_01",
        "in06_01",
        "in07_01",
        "in08_01"
      )))
    ),

    # final discrimination frequency average
    discrim_mean = safe_rowmean(pick(matches("^di01_0?[1-7]$"))),

    # final personal belonging total
    pb_total = safe_rowsum(pick(matches("^pb01_0?[1-5]$"))),

    # 2 PB averages - as a reminder from the future for you and for me, I figured that our 5-item battery was really measuring...
    # ...2 different things (as a consequence of the battery being made by multiple people), and, as such, I split it here into...

    # ...pb_citizenship, which includes questions about opinions on the general, non-personal consequences of one's posessing German citizenship...
    pb_citizenship = safe_rowmean(pick(any_of(c("pb01_01", "pb01_03")))),

    # ...and pb_belonging, which includes questions about the respondent's personal sense of belonging and its importance
    pb_belonging = safe_rowmean(pick(any_of(c(
      "pb01_02",
      "pb01_04",
      "pb01_05"
    )))),

    # the following is the migration background coding

    # if ci01/ci03/ci05 != 1 OR ci02/ci04/ci06 has any non-missing entry >>> migration background
    self_born_abroad = (!is.na(ci01) & ci01 != 1) | !is.na(ci02), # ci02 for other, non-list countries
    mother_born_abroad = (!is.na(ci03) & ci03 != 1) | !is.na(ci04),
    father_born_abroad = (!is.na(ci05) & ci05 != 1) | !is.na(ci06),

    # coding binary migration vs no migration variable
    migration_background = as.numeric(
      self_born_abroad | mother_born_abroad | father_born_abroad
    ),

    # 3-category migration generation variable
    migration_generation = case_when(
      self_born_abroad ~ "1st gen",
      !self_born_abroad & (mother_born_abroad | father_born_abroad) ~ "2nd gen",
      !self_born_abroad &
        !mother_born_abroad &
        !father_born_abroad ~ "Non-migrant",
      TRUE ~ NA_character_
    ),

    # making sure it's treated as a categorical variable in OLS
    migration_generation = factor(
      migration_generation,
      levels = c("Non-migrant", "2nd gen", "1st gen")
    )
  )

# 6. here we recode the personal experience questions...
# this one just to see if there was any experience at all
pers_exp <- grep("^pe0[1-7]$", names(ds), value = TRUE) # just looking at matching column names

ds <- ds %>%
  mutate(
    across(all_of(pers_exp), ~ suppressWarnings(as.numeric(.)))
  ) %>%
  mutate(
    # any experience: 1 if responses 2/3/4, else 0 if response 1
    across(
      all_of(pers_exp),
      ~ case_when(
        . == 1 ~ 0,
        . %in% 2:4 ~ 1,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_exp_binary"
    ),

    # the degree of experience: 1 = neg, 2 = mixed, 3 = pos; NA if no experience
    across(
      all_of(pers_exp),
      ~ case_when(
        . == 2 ~ 1,
        . == 3 ~ 2,
        . == 4 ~ 3,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_exp_degree"
    )
  )

# 7. ...and here we aggregate scores for personal experience
pers_exp_frequency <- grep("_exp_binary$", names(ds), value = TRUE)
pers_exp_degree <- grep("_exp_degree$", names(ds), value = TRUE)

ds <- ds %>%
  mutate(
    # frequency of experience, aka number of institutions with any experience
    exp_frequency = safe_rowsum(pick(all_of(pers_exp_frequency))),

    # if experienced, mean degree across institutions; 1 = neg, 2 = mixed, 3 = pos
    exp_degree_mean = safe_rowmean(pick(all_of(pers_exp_degree)))
  )

# converting our variables into categorical variables-ish, mostly for reference categories
ds <- ds %>%
  mutate(
    migration_background = as.factor(migration_background), #
    german_citizen_binary = as.factor(german_citizen_binary),
    migration_generation = factor(
      migration_generation,
      levels = c("Non-migrant", "2nd gen", "1st gen")
    )
  )

# 9. and here we write the cleaned data for the next 2 scripts
write_csv(ds, "data_frost_cleaned.csv")

# taking a peek at the new data
ds_cleaned <- read_csv("data_frost_cleaned.csv")
# glimpse(ds_cleaned)
