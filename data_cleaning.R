library(readxl)
library(readr)
library(janitor)
library(dplyr)


#to truncate values when they are being visualised
options(digits = 3)

ds_raw <- read_excel("data_frost.xlsx", sheet = 1) %>% clean_names() #
#glimpse(ds_raw)
#summary(ds_raw)

#recoding -1, -9 as NAs
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

ds <- ds_raw %>% mutate(across(everything(), recode_special_na))

#also, removing completely empty rows
ds <- ds %>% filter(!if_all(everything(), is.na))

#numeric transition section
ds <- ds %>%
  mutate(across(
    c(any_of(c("de05_01","de02","de03","ci15","ci01","ci02","ci03","ci04","ci05","ci06")),
      matches("^(in0[1-8]_|di01_|pb01_)")),
    ~ suppressWarnings(as.numeric(.))
  ))


#very simple data recoding section
survey_year <- 2026

ds <- ds %>%
  mutate(
    #age from birth year
    age = if_else(!is.na(de05_01), survey_year - de05_01, NA_real_),

    #gender binary
    gender_binary = case_when(
      de02 == 1 ~ 0, #female
      de02 == 2 ~ 1, #male
      TRUE ~ NA_real_
    ),
    #keeping all 3 for descriptive statistics
    gender_categories = factor(de02, levels = c(1, 2, 3),
                        labels = c("Female", "Male", "Diverse")),
    
    #university education binary
    uni_binary = case_when(
      de03 %in% c(8, 9, 11, 14, 13) ~ 1, #uni
      #13 = "other", coded as uni, since all (current) cases refer to "Diplom"; will be rechecked later
      de03 %in% c(1, 2, 3, 5, 6) ~ 0, #non-uni
      TRUE ~ NA_real_
    ),
   
    #German citizenship binary
    german_citizen_binary = case_when(
      ci15 == 2 ~ 0, #no
      ci15 == 1 ~ 1, #yes
      TRUE ~ NA_real_
    ),

   #member of discriminated group binary
      discriminated_group_binary = case_when(
        di03 == 2 ~ 0, #no
        di03 == 1 ~ 1, #yes
        TRUE ~ NA_real_
    ),
  )


#scale/index aggregation section
#to-be expanded !!!

ds <- ds %>%
  mutate(

    #for each institution (01..08) - average across all given items
    bund_mean = rowMeans(pick(matches("^in01_")), na.rm = TRUE), #Bundestag
    council_mean = rowMeans(pick(matches("^in02_")), na.rm = TRUE), #local council
    police_mean = rowMeans(pick(matches("^in03_")), na.rm = TRUE), #police
    court_mean = rowMeans(pick(matches("^in05_")), na.rm = TRUE), #courts
    media_mean = rowMeans(pick(matches("^in06_")), na.rm = TRUE), #public media
    immigr_mean = rowMeans(pick(matches("^in07_")), na.rm = TRUE), #immigration office
    citizens_mean = rowMeans(pick(matches("^in08_")), na.rm = TRUE), #citizens' office
    
    #final institutional trust average
    institution_mean = rowMeans(pick(matches("^in0[1-8]_")), na.rm = TRUE),
    
    #final discrimination frequency average
    discrim_mean = rowMeans(pick(matches("^di01_0?[1-7]$")), na.rm = TRUE),
    
    #final personal belonging average
    pb_total = rowSums(pick(matches("^pb01_0?[1-5]$")), na.rm = TRUE),
    
    #2 PB averages
    pb_citizenship = rowMeans(pick(any_of(c("pb01_01", "pb01_03"))), na.rm = TRUE), #citizenship average
    pb_belonging = rowMeans(pick(any_of(c("pb01_02", "pb01_04", "pb01_05"))), na.rm = TRUE), #senes of belonging
    
    #the following is the migration background coding
    
    #if ci01/ci03/ci05 != 1 OR ci02/ci04/ci06 has any non-missing entry >>> migration background
    self_born_abroad   = (!is.na(ci01) & ci01 != 1) | !is.na(ci02), #ci02 for other, non-list countries
    mother_born_abroad = (!is.na(ci03) & ci03 != 1) | !is.na(ci04),
    father_born_abroad = (!is.na(ci05) & ci05 != 1) | !is.na(ci06),
    
    #coding binary migration vs no migration variable 
    migration_background = as.numeric(self_born_abroad | mother_born_abroad | father_born_abroad),
    
    #3-category migration generation variable
    migration_generation = case_when(
      self_born_abroad ~ "1st gen",
      !self_born_abroad & (mother_born_abroad | father_born_abroad) ~ "2nd gen",
      !self_born_abroad & !mother_born_abroad & !father_born_abroad ~ "Non-migrant",
      TRUE ~ NA_character_
    ),
    
    #making sure it's treated as a categorical variable in OLS
    migration_generation = factor(migration_generation,
                                  levels = c("Non-migrant", "2nd gen", "1st gen"))
)

    #recoding the personal experience questions
pers_exp <- grep("^pe0[1-7]$", names(ds), value = TRUE) #just looking at matching column names

ds <- ds %>%
  mutate(across(all_of(pers_exp), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(
    #any experience: 1 if responses 2/3/4, else 0 if response 1
    across(all_of(pers_exp),
           ~ case_when(. == 1 ~ 0,
                       . %in% 2:4 ~ 1,
                       TRUE ~ NA_real_),
           .names = "{.col}_exp_binary"),
    #the degree of experience: 1=neg,2=mixed,3=pos; NA if no experience
    across(all_of(pers_exp),
           ~ case_when(. == 2 ~ 1,
                       . == 3 ~ 2,
                       . == 4 ~ 3,
                       TRUE ~ NA_real_),
           .names = "{.col}_exp_degree")
  )

    #also having aggregate scores for personal experience just in case
pers_exp_frequency <- grep("_exp_binary$", names(ds), value = TRUE)

ds <- ds %>%
  mutate(
    #frequency of experience, aka number of institutions with any experience
    exp_frequency = rowSums(pick(all_of(pers_exp_frequency)), na.rm = TRUE),
    
    #if experienced, mean valence across institutions; 1=neg,2=mixed,3=pos
    exp_degree_mean = rowMeans(pick(ends_with("_exp_degree")), na.rm = TRUE)
    
  )

# Keep ALL columns; only fix types for analysis convenience
ds <- ds %>%
  mutate(
    migration_background   = as.factor(migration_background),
    german_citizen_binary  = as.factor(german_citizen_binary),
    migration_generation   = factor(migration_generation,
                                    levels = c("Non-migrant", "2nd gen", "1st gen"))
  )

#writing cleaned data
write_csv(ds, "data_frost_cleaned.csv")

#taking a peek at the new data
ds_cleaned <- read_csv("data_frost_cleaned.csv")
#glimpse(ds_cleaned)