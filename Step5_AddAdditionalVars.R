

# ======================================================
# ADD PRS SCORES, DEMOGRAPHIC DATA, AND NEGATIVE CONTROL 
# ======================================================

# load packages
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("stringr")) install.packages("stringr"); library(stringr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)

# load data
pregnancies = fread("Data/processed/Step4_AddMedications.csv", na.strings=c("", NA))
PRS_SCZ = read.table("Data/processed/PRS/SCZ_PRS.txt", header = TRUE, sep = "") %>% rename(eid = IID, PRS_SCZ = Score)
PRS_BIPOL = read.table("Data/processed/PRS/BIP_PRS.txt", header = TRUE, sep = "")  %>% rename(eid = IID, PRS_BIPOL = Score)
PRS_ANX = read.table("Data/processed/PRS/AD_PRS.txt", header = TRUE, sep = "")  %>% rename(eid = IID, PRS_ANX = Score)
PRS_DEPR = read.table("Data/processed/PRS/MDD_PRS.txt", header = TRUE, sep = "")  %>% rename(eid = IID, PRS_DEPR = Score)
principal_components = read.table("Data/processed/PRS/PC_final.txt", header = TRUE, sep = "") %>% rename(eid = IID)
UKB_vars = fread("Data/raw_UKB/Pathways_All_Vars.csv", na.strings=c("", NA))


# clean demographics
demo = UKB_vars %>%
  select(c(`Participant ID`, `Year of birth`, `Townsend deprivation index at recruitment`, matches("Qualifications|Ethnic"))) %>%
  rename(eid = `Participant ID`,
         BirthYear = `Year of birth`,
         TownsendDeprivationIndex = `Townsend deprivation index at recruitment`) %>%
  mutate(University = case_when(
           rowSums(across(contains("Qualifications"), 
                   ~ grepl("University", .))) > 0 ~ 1, TRUE ~ 0 ),
         White_eth = case_when(
           rowSums(across(contains("Ethnic background"), 
                     ~ . %in% c(
                       "Any other white background",
                       "British",
                       "Irish",
                       "White",
                       "White and Asian",
                       "White and Black African",
                       "White and Black Caribbean"))) > 0 ~ 1, TRUE ~ 0)) %>%
  select(-matches("Qualifications|Ethnic"))


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
  mutate(PregYear = format(as.Date(min_date_preg, format = "%Y-%m-%d"), "%Y"),
         AgeAtPreg = as.numeric(PregYear) - BirthYear) %>% # add variable Age At Pregnancy
  filter(AgeAtPreg >=15 & AgeAtPreg <=49) %>% # remove pregnancies in mothers <15 or >49 years old
  select(-PregYear)


# export
write.csv(pregnancies_1,"Data/processed/Step5_AddAdditionalVars.csv", row.names=F)


# clear the environment
rm(list = ls())
