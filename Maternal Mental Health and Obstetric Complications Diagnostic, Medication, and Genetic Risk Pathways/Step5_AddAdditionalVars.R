


# THE FOLLOWING SCRIPT ADDS PRS SCORES, DEMOGRAPHIC DATA, AND THE NEGATIVE OUTCOME CONTROL  ------------------------------------------------------



# load packages
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("stringr")) install.packages("stringr"); library(stringr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)

# load data
pregnancies = fread("processed/Step4_AddMedications.csv", na.strings=c("", NA))
PRS_SCZ = read.table("raw_UKB/PRS_SCZ.txt", header = TRUE, sep = "")
PRS_BIPOL = read.table("raw_UKB/PRS_BIPOL.txt", header = TRUE, sep = "")
PRS_ANX = read.table("raw_UKB/PRS_ANX.txt", header = TRUE, sep = "")
PRS_DEPR = read.table("raw_UKB/PRS_DEPR.txt", header = TRUE, sep = "")
principal_components = read.table("raw_UKB/PRS_PrincipalComponents.txt", header = TRUE, sep = "")
UKB_vars = fread("raw_UKB/pathways_additional_vars.csv", na.strings=c("", NA))


# clean demographics
demo = UKB_vars %>%
  select(c(`Participant ID`, `Townsend deprivation index at recruitment`, matches("Qualifications"))) %>%
  rename(eid = `Participant ID`,
         TownsendDeprivationIndex = `Townsend deprivation index at recruitment`) %>%
  mutate(University = case_when(
           rowSums(across(contains("Qualifications"), 
                   ~ grepl("University", .))) > 0 ~ 1, TRUE ~ 0 )) %>%
  select(-matches("Qualifications"))


# clean the negative control - glaucoma
glaucoma = UKB_vars %>%
  select(c(`Participant ID`, `Date H40 first reported (glaucoma)`)) %>%
  rename(eid = `Participant ID`,
         Glaucoma = `Date H40 first reported (glaucoma)`) %>%
  mutate(Glaucoma = ifelse(!is.na(Glaucoma), 1, 0))
  

# combine PRS, demographics, and glaucoma with pregnancy data
pregnancies_1 = pregnancies %>%
  left_join(PRS_SCZ) %>%
  left_join(PRS_BIPOL) %>%
  left_join(PRS_ANX) %>%
  left_join(PRS_DEPR) %>%
  left_join(principal_components) %>%
  left_join(demo) %>%
  left_join(glaucoma) %>%
  rename(BirthYear = `Year of birth`)


# export
write.csv(pregnancies_1,"processed/Step5_AddAdditionalVars.csv", row.names=F)


# clear the environment
rm(list = ls())
