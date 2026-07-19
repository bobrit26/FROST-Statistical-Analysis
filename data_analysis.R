library(dplyr)
library(psych)
library(ggplot2)
library(tidyr)


ds_cleaned <- read.csv("data_frost_cleaned.csv")

#core variables for the draft
core_vars <- c(
  "institution_mean",                      #overall institutional trust
  "bund_mean","council_mean","police_mean","court_mean",
  "media_mean","immigr_mean","citizens_mean",   #institution-specific trust
  "german_citizen_binary",            # 1=yes, 0=no
  "migration_background",             # 1=background, 0=no background
  "discrim_mean",                        # discrimination
  "pb_total",           #total personal belonging
  "pb_citizenship","pb_belonging",            #subsamples
  "age","gender_binary","uni_binary"   #controls
)

ds_analysis <- ds_cleaned %>%
  select(any_of(core_vars)) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))  #jic numeric for corr

#basic descriptive stats (N, mean, sd, min, max)
desc <- psych::describe(ds_analysis)
desc

#save
write.csv(desc, "desc_analysis.csv")

#correlation matrix with p-values
#pairwise complete observations
corr <- psych::corr.test(ds_analysis, use = "pairwise", method = "pearson", adjust = "none")
corr$r      # correlation coefficients
corr$p      # p-values

#export correlation matrix
write.csv(round(corr$r, 3), "corr_matrix_r.csv")
write.csv(round(corr$p, 3), "corr_matrix_p.csv")

#key group means
#trust by citizenship ~ migrants
ds_analysis %>%
  filter(migration_background == 1) %>%
  summarise(
    n = sum(!is.na(institution_mean) & !is.na(german_citizen_binary)),
    trust_mean_cit0 = mean(institution_mean[german_citizen_binary == 0], na.rm = TRUE),
    trust_mean_cit1 = mean(institution_mean[german_citizen_binary == 1], na.rm = TRUE)
  )

#trust by migration background
ds_analysis %>%
  summarise(
    n = sum(!is.na(institution_mean) & !is.na(migration_background)),
    trust_mean_nonmigrant = mean(institution_mean[migration_background == 0], na.rm = TRUE),
    trust_mean_migrant    = mean(institution_mean[migration_background == 1], na.rm = TRUE)
  )



#visuals
#histograms
ggplot(ds, aes(x = institution_mean)) +
  geom_histogram(binwidth = 0.5, color = "white") +
  labs(x = "Overall institutional trust (mean)", y = "Count")

ggplot(ds, aes(x = discrim_mean)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  labs(x = "Discrimination (mean)", y = "Count")

ggplot(ds, aes(x = pb_citizenship)) +
  geom_histogram(binwidth = 0.5, color = "white") +
  labs(x = "Belonging subscale A (mean)", y = "Count")

#bar charts
ggplot(ds, aes(x = factor(migration_background))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Migration background", y = "Count")

ggplot(ds, aes(x = factor(german_citizen_binary))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship", y = "Count")

# boxplots: trust by groups
ggplot(ds, aes(x = factor(german_citizen_binary), y = institution_mean)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship", y = "Overall institutional trust (mean)")

ggplot(ds, aes(x = factor(migration_background), y = institution_mean)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Migration background", y = "Overall institutional trust (mean)")

#within migrants only
ggplot(filter(ds, migration_background == 1),
       aes(x = factor(german_citizen_binary), y = institution_mean)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "German citizenship (migrants only)", y = "Overall institutional trust (mean)")

#scatterplots: trust vs discrimination / belonging (with linear fit)
ggplot(ds, aes(x = discrim_mean, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Discrimination (mean)", y = "Overall institutional trust (mean)")

ggplot(ds, aes(x = pb_citizenship, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Belonging subscale A (mean)", y = "Overall institutional trust (mean)")


#recheck later
ggplot(ds, aes(x = pb_total, y = institution_mean)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Total personal belonging (mean)", y = "Overall institutional trust (mean)")

#all institutions plot: mean trust by institution 

inst_long <- ds %>%
  select(bund_mean, council_mean, police_mean, court_mean, media_mean, immigr_mean, citizens_mean) %>%
  pivot_longer(everything(), names_to = "institution", values_to = "trust") %>%
  mutate(institution = recode(institution,
                              bund_mean = "Bundestag",
                              council_mean = "Local council",
                              police_mean = "Police",
                              court_mean = "Courts",
                              media_mean = "Public media",
                              immigr_mean = "Immigration office",
                              citizens_mean = "Citizens' office"
  ))

ggplot(inst_long, aes(x = institution, y = trust)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL, y = "Trust (mean)")