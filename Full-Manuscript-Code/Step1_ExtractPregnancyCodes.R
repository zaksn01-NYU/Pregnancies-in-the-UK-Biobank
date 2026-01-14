


# THE FOLLOWING SCRIPT SUBSETS ALL PREGNANCY-RELATED CODES FROM THE HOSPITAL AND PRIMARY CARE DATASETS ------------------------------------------------------



# load packages
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("stringr")) install.packages("stringr"); library(stringr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)
if (!require("readxl")) install.packages("readxl"); library(readxl)


# HOSPITAL ----------------------------------------------------------------


# load data
hesin_diagnosis = fread("raw_UKB/hesin_diag.csv", na.strings=c("", NA))
hesin = fread("raw_UKB/hesin.csv", na.strings=c("", NA))
ICD9to10conversion = read_excel("raw_UKB/all_lkps_maps_v4.xlsx", na=c("", NA), sheet="icd9_icd10")


# filter to keep only pregnancy-related codes from the "hesin_diagnosis" dataset
hesin_diagnosis_preg = hesin_diagnosis %>%
  filter(
    str_detect(`Diagnoses - ICD10`, "^(O|Z321|Z33|Z34|Z35|Z36|Z37|Z38|Z39)") |
      str_detect(`Diagnoses - ICD9`, "^(6[3-6][0-9]|67[0-6]|V22|V23|V24)"))


# merge hesin with hesin_diagnosis
hesin_preg = hesin_diagnosis_preg %>% 
  left_join(hesin) %>% 
  mutate(Date = ifelse(is.na(`Episode start date`), `Date of admission to hospital`, `Episode start date`),
         Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  select(c("Participant ID","Diagnoses - ICD10","Diagnoses - ICD9","Date"))


# subset to ICD9 codes and convert to ICD10 through conversion map + manual conversion
hesin_preg_ICD9 = hesin_preg %>%
  filter(!is.na(`Diagnoses - ICD9`)) %>%
  select(`Diagnoses - ICD9`) %>%
  distinct() %>%
  left_join(ICD9to10conversion, by = c("Diagnoses - ICD9" = "ICD9")) %>%
  select(-c(DESCRIPTION_ICD9, DESCRIPTION_ICD10)) %>%
  mutate(ICD10 = str_replace_all(ICD10, "X", ""), # remove trailing "X" from codes
         ICD10 = str_replace_all(ICD10, "\\.", ""), # remove dots for consistency
         
         ICD10 = case_when(                          # manually convert codes not present in the conversion map
           `Diagnoses - ICD9` == "6309" ~ "O01",     # Hydatidiform mole
           `Diagnoses - ICD9` == "6319" ~ "O02",     # Other abnormal products of conception
           `Diagnoses - ICD9` == "6329" ~ "O02",     # Missed abortion
           `Diagnoses - ICD9` == "6330" ~ "O00",     # Abdominal pregnancy
           `Diagnoses - ICD9` == "6484" ~ "O99",     # Mental disorder in mother compl. pregnancy, childbirth, puerperium
           `Diagnoses - ICD9` == "6487" ~ "O99",     # Bone joint dis. back, pelvis, legs in mother comp. preg. birth puerp.
           `Diagnoses - ICD9` == "6488" ~ "O99",     # Abn. glucose tolerance in mother compl. preg., birth, puerperium
           `Diagnoses - ICD9` == "6709" ~ "O86",     # Major puerperal infection
           `Diagnoses - ICD9` == "6740" ~ "O90",     # Cerebrovascular disorders in the puerperium
           `Diagnoses - ICD9` == "6743" ~ "O75",     # Other complications of obstetrical surgical wounds
           `Diagnoses - ICD9` == "6748" ~ "O90",     # Other specified complications of the puerperium, not elsewhere classified
           `Diagnoses - ICD9` == "6752" ~ "O91",     # Nonpurulent mastitis
           TRUE ~ ICD10))


# merge back with hesin_preg + clean       
hesin_preg_complete = hesin_preg %>%
  full_join(hesin_preg_ICD9, relationship = "many-to-many") %>%
  mutate(`Diagnoses - ICD10` = coalesce(`Diagnoses - ICD10`, ICD10),
         source = "Hospital") %>%
  select(-c(`Diagnoses - ICD9`, ICD10)) %>%
  rename(eid = `Participant ID`, ICD10code = `Diagnoses - ICD10`)


# export
write.csv(hesin_preg_complete, "processed/Step1A_ExtractPregnancyCodes_Hospital.csv", row.names = F)

# clear the environment
rm(list = ls())


# PRIMARY CARE ------------------------------------------------------------


# load data
v2codes = read_excel("raw_UKB/all_lkps_maps_v4.xlsx", na=c("", NA), sheet="read_v2_icd10")
CTV3codes = read_excel("raw_UKB/all_lkps_maps_v4.xlsx", na=c("", NA), sheet="read_ctv3_icd10")
PrimaryCare = fread("raw_UKB/gp_clinical.csv", na.strings=c("", NA))


# identify the pregnancy codes from Read V2
v2_preg = v2codes %>% 
  rename(read_V2_code = read_code) %>% # rename column
  filter(icd10_code_def == 1) %>% # remove v2 codes that are linked to multiple codes
  select(-icd10_code_def) %>%
  filter(grepl("^(O|Z321|Z33|Z34|Z35|Z36|Z37|Z38|Z39)", icd10_code)) 


# identify the pregnancy codes from Read CTV3
ctv3_preg = CTV3codes %>% 
  rename(read_ctv3_code = read_code) %>%
  filter(mapping_status != "R" & mapping_status != "A" & mapping_status != "U") %>% # remove codes where mapping status: R = Requires checking; A = Alternative mapping; U = Unspecified
  filter(grepl("^(O|Z321|Z33|Z34|Z35|Z36|Z37|Z38|Z39)", icd10_code)) %>% 
  select(c(read_ctv3_code,icd10_code))


# convert the Read V2 codes in the primary care dataset
PrimaryCare_v2_preg = PrimaryCare %>% 
  right_join(v2_preg, by = c("Read v2 code" = "read_V2_code"))


# convert the Read CTV3 codes in the primary care dataset
PrimaryCare_ctv3_preg = PrimaryCare %>% 
  right_join(ctv3_preg, by = c("CTV3 (Read v3) code" = "read_ctv3_code"), relationship = "many-to-many")


# combine + clean
PrimaryCare_preg_complete = bind_rows(PrimaryCare_v2_preg, PrimaryCare_ctv3_preg) %>%
  rename(eid = `Participant ID`, Date = `Date clinical code was entered`, ICD10code = icd10_code) %>%
  select(eid, Date, ICD10code) %>%
  filter(!is.na(Date)) %>%
  mutate(source = "PrimaryCare")


# export
write.csv(PrimaryCare_preg_complete,"processed/Step1B_ExtractPregnancyCodes_PrimaryCare.csv", row.names = F)


# clear the environment
rm(list = ls())


