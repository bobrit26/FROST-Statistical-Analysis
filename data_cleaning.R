library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(tidyr)
library(psych)
library(shiny)

#to truncate values when they are being visualised
options(digits = 3)

ds_raw <- read_excel("data_frost2026_2026-07-17_14-31.xlsx", sheet = 1) %>% clean_names() #
#glimpse(ds_raw)
summary(ds_raw)

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
    
    #coding binary migration vs no migration variable
    #if ci01/ci03/ci05 != 1 OR ci02/ci04/ci06 has any non-missing entry >>> migration background
    migration_background = as.numeric(
      ( !is.na(ci01) & ci01 != 1 ) | !is.na(ci02) |
        ( !is.na(ci03) & ci03 != 1 ) | !is.na(ci04) |
        ( !is.na(ci05) & ci05 != 1 ) | !is.na(ci06) 
        #0 = no background, 1 = background    
      )
  )


#writing cleaned data
write.csv(ds, "data_frost_cleaned.csv", row.names = FALSE)

#taking a peek at the new data
ds_cleaned <- read.csv("data_frost_cleaned.csv")