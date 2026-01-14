


# THE FOLLOWING SCRIPT IDENTIFIES PSYCHIATRIC CASES FROM HOPITAL DATA, PRIMARY CARE DATA, AND UKB 'DATE OF FIRST DIAGNOSIS' VARIABLES ------------------------------------------------------



# load packages
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("stringr")) install.packages("stringr"); library(stringr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)
if (!require("readxl")) install.packages("readxl"); library(readxl)


# load data: Hospital
hesin_diagnosis = fread("raw_UKB/hesin_diag.csv", na.strings=c("", NA))
hesin = fread("raw_UKB/hesin.csv", na.strings=c("", NA))
ICD9to10conversion = read_excel("raw_UKB/all_lkps_maps_v4.xlsx", na=c("", NA), sheet="icd9_icd10")

# load data: Primary Care
v2codes = read_excel("raw_UKB/all_lkps_maps_v4.xlsx", na=c("", NA), sheet="read_v2_icd10")
CTV3codes = read_excel("raw_UKB/all_lkps_maps_v4.xlsx", na=c("", NA), sheet="read_ctv3_icd10")
PrimaryCare = fread("raw_UKB/gp_clinical.csv", na.strings=c("", NA))

# load data: Other
UKB_vars = fread("raw_UKB/pathways_additional_vars.csv", na.strings=c("", NA))
pregnancies = fread("processed/Step2_ClusterPregnancies.csv", na.strings=c("", NA))


# HOSPITAL ----------------------------------------------------------------

# filter to keep only relevant psychiatric codes from the "hesin_diagnosis" dataset
hesin_diagnosis_psych = hesin_diagnosis %>%
  filter(
    str_detect(`Diagnoses - ICD10`, "^(F20|F21|F22|F23|F24|F25|F28|F29|F31|F40|F41|F42|F32|F33)") |
      str_detect(`Diagnoses - ICD9`, "^(295|296|297|298|300|311)"))


# merge hesin with hesin_diagnosis
hesin_psych = hesin_diagnosis_psych %>% 
  left_join(hesin) %>% 
  mutate(Date = ifelse(is.na(`Episode start date`), `Date of admission to hospital`, `Episode start date`),
         Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  select(c("Participant ID","Diagnoses - ICD10","Diagnoses - ICD9","Date"))


# subset to ICD9 codes and convert to ICD10 through conversion map + manual conversion
hesin_psych_ICD9 = hesin_psych %>%
  filter(!is.na(`Diagnoses - ICD9`)) %>%
  select(`Diagnoses - ICD9`) %>%
  distinct() %>%
  left_join(ICD9to10conversion, by = c("Diagnoses - ICD9" = "ICD9")) %>%
  select(-c(DESCRIPTION_ICD9, DESCRIPTION_ICD10)) %>%
  mutate(ICD10 = str_replace_all(ICD10, "X", ""), #remove trailing "X" from codes
         ICD10 = str_replace_all(ICD10, "\\.", ""), #remove dots for consistency
         
         ICD10 = case_when(                          # manually convert codes not present in the conversion map
           `Diagnoses - ICD9` == "2954" ~ "F20", # Acute schizophrenic episode
           `Diagnoses - ICD9` == "2957" ~ "F25", # Schizophrenic psychosis, schizoaffective type
           `Diagnoses - ICD9` == "2959" ~ "F20", # Schizophrenic psychosis, unspecified
           `Diagnoses - ICD9` == "2960" ~ "F30", # Manic-depressive psychosis, manic type
           `Diagnoses - ICD9` == "2961" ~ "F30", # Manic-depressive psychosis, depressed type
           `Diagnoses - ICD9` == "2962" ~ "F32", # Manic-depressive psychosis, circular type but currently manic
           `Diagnoses - ICD9` == "2964" ~ "F32", # Manic-depressive psychosis, circular type, mixed
           `Diagnoses - ICD9` == "2965" ~ "F31", # Manic-depressive psychosis, circular type, current condition not specified
           `Diagnoses - ICD9` == "2966" ~ "F31", # Manic-depressive psychosis, other and unspecified
           `Diagnoses - ICD9` == "2970" ~ "F22", # Paranoid state, simple
           `Diagnoses - ICD9` == "2971" ~ "F22", # Paranoia
           `Diagnoses - ICD9` == "2979" ~ "F23", # Paranoid state, unspecified
           `Diagnoses - ICD9` == "2980" ~ "F32", # Other nonorganic psychoses, depressive type
           `Diagnoses - ICD9` == "2982" ~ "F44", # Reactive confusion
           `Diagnoses - ICD9` == "2983" ~ "F23", # Acute paranoid reaction
           `Diagnoses - ICD9` == "2984" ~ "F23", # Psychogenic paranoid psychosis
           `Diagnoses - ICD9` == "2988" ~ "F23", # Other and unspecified reactive psychosis
           `Diagnoses - ICD9` == "2989" ~ "F29", # Unspecified psychosis
           `Diagnoses - ICD9` == "3000" ~ "F41", # Anxiety states
           `Diagnoses - ICD9` == "3001" ~ "F44", # Hysteria
           `Diagnoses - ICD9` == "3002" ~ "F40", # Phobic state
           `Diagnoses - ICD9` == "3004" ~ "F34", # Neurotic depression
           `Diagnoses - ICD9` == "3005" ~ "F48", # Neurasthenia
           `Diagnoses - ICD9` == "3009" ~ "F48", # Neurotic disorder, unspecified
           `Diagnoses - ICD9` == "3119" ~ "F32", # Depressive disorder, not elsewhere classified
           TRUE ~ ICD10))


# merge back with hesin_psych + clean       
hesin_psych_complete = hesin_psych %>%
  full_join(hesin_psych_ICD9, relationship = "many-to-many") %>%
  mutate(`Diagnoses - ICD10` = coalesce(`Diagnoses - ICD10`, ICD10),
         source = "Hospital",
         Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  select(-c(`Diagnoses - ICD9`, ICD10)) %>%
  rename(eid = `Participant ID`, ICD10code = `Diagnoses - ICD10`)



# PRIMARY CARE ------------------------------------------------------------

# identify the psych codes from Read V2
v2_preg = v2codes %>% 
  rename(read_V2_code = read_code) %>% #rename column
  filter(icd10_code_def == 1) %>% #remove v2 codes that are linked to multiple codes
  select(-icd10_code_def) %>%
  filter(grepl("^(F20|F21|F22|F23|F24|F25|F28|F29|F31|F40|F41|F42|F32|F33)", icd10_code)) 


# identify the psych codes from Read CTV3
ctv3_preg = CTV3codes %>% 
  rename(read_ctv3_code = read_code) %>%
  filter(mapping_status != "R" & mapping_status != "A" & mapping_status != "U") %>% #remove codes where mapping status: R = Requires checking; A = Alternative mapping; U = Unspecified
  filter(grepl("^(F20|F21|F22|F23|F24|F25|F28|F29|F31|F40|F41|F42|F32|F33)", icd10_code)) %>% 
  select(c(read_ctv3_code,icd10_code))


# convert the Read V2 codes in the primary care dataset
PrimaryCare_v2_psych = PrimaryCare %>% 
  right_join(v2_preg, by = c("Read v2 code" = "read_V2_code"))


# convert the Read CTV3 codes in the primary care dataset
PrimaryCare_ctv3_psych = PrimaryCare %>% 
  right_join(ctv3_preg, by = c("CTV3 (Read v3) code" = "read_ctv3_code"), relationship = "many-to-many")


# combine + clean
PrimaryCare_psych_complete = bind_rows(PrimaryCare_v2_psych, PrimaryCare_ctv3_psych) %>%
  rename(eid = `Participant ID`, Date = `Date clinical code was entered`, ICD10code = icd10_code) %>%
  select(eid, Date, ICD10code) %>%
  filter(!is.na(Date)) %>%
  mutate(source = "PrimaryCare",
         Date = as.Date(Date, format = "%Y-%m-%d"))



# clean the environment
rm(list = setdiff(ls(), c("hesin_psych_complete", "PrimaryCare_psych_complete", "UKB_vars", "pregnancies")))



###################
## OTHER SOURCES ##
###################

# pivot dates
psych_date_columns = grep("Date", colnames(UKB_vars), value = TRUE)
psych_date_columns = psych_date_columns[!grepl("glaucoma", psych_date_columns, ignore.case = TRUE)]

first_psych_dx_dates = UKB_vars %>%
  select(`Participant ID`, all_of(psych_date_columns)) %>%
  mutate(across(all_of(psych_date_columns), ~ as.Date(.x, format = "%Y-%m-%d"))) %>%
  pivot_longer(
    cols = -`Participant ID`,
    names_to = "ICD10code",
    values_to = "Date") %>%
  filter(!is.na(Date)) %>%
  mutate(ICD10code = str_extract(ICD10code, "(?<=Date )\\S+")) # extract the ICD10 codes
  

# pivot sources
psych_source_columns = grep("Source", colnames(UKB_vars), value = TRUE)

first_psych_dx_sources = UKB_vars %>%
  select(`Participant ID`, all_of(psych_source_columns)) %>%
  pivot_longer(
    cols = -`Participant ID`,
    names_to = "ICD10code",
    values_to = "source") %>%
   mutate(ICD10code = str_extract(ICD10code, "\\b\\w+(?= \\()")) # extract the ICD10 codes


# combine
first_psych_dx_long = first_psych_dx_dates %>%
  left_join(first_psych_dx_sources) %>%
  rename(eid = `Participant ID`)


# combine all sources of psych diagnoses
all_psych = dplyr::bind_rows(hesin_psych_complete, PrimaryCare_psych_complete, first_psych_dx_long)


# define psych diagnosis types
all_psych = all_psych %>%
  mutate(Dx = case_when(
    grepl("^(F20|F21|F22|F23|F24|F25|F28|F29)", ICD10code) ~ "SCZ",
    grepl("^(F31)", ICD10code) ~ "BIPOL",
    grepl("^(F40|F41|F42)", ICD10code) ~ "ANX",
    grepl("^(F32|F33)", ICD10code) ~ "DEPR")) %>%
  filter(!is.na(Dx)) %>%
  # keep diagnoses with placeholder dates, but make them year 2099 to avoid classifying them as diagnoses made prior to pregnancy
  mutate(Date = ifelse(Date < as.Date("1950-01-01", format = "%Y-%m-%d"), as.Date("2099-01-01", format = "%Y-%m-%d"), Date),
         Date = as.Date(Date, format = "%Y-%m-%d"))


# get the earliest diagnosis date and combine sources of diagnoses
all_psych_summarized = all_psych %>%
  group_by(eid, Dx) %>%
  summarize(
    first_date = min(Date),
    sources = paste(unique(source), collapse = ", "),
    .groups = "drop")


# pivot
all_psych_wide = all_psych_summarized %>%
  pivot_wider(
    id_cols = eid,
    names_from = Dx,
    values_from = c(first_date, sources),
    names_glue = "{.value}_{Dx}")


# combine with pregnancies
pregnancies_1 = pregnancies %>% 
  left_join(all_psych_wide) %>%
  group_by(eid) %>%
  mutate(first_preg = min(min_date_preg)) %>%
  ungroup() %>% 
  mutate(
    SCZ_YN = ifelse(!is.na(first_date_SCZ), 1, 0),
    BIPOL_YN = ifelse(!is.na(first_date_BIPOL), 1, 0),
    ANX_YN = ifelse(!is.na(first_date_ANX), 1, 0),
    DEPR_YN = ifelse(!is.na(first_date_DEPR), 1, 0),
    
    SCZ_prior = case_when(first_date_SCZ < min_date_preg ~ 1, TRUE ~ 0),
    BIPOL_prior = case_when(first_date_BIPOL < min_date_preg ~ 1, TRUE ~ 0),
    ANX_prior = case_when(first_date_ANX < min_date_preg ~ 1, TRUE ~ 0),
    DEPR_prior = case_when(first_date_DEPR < min_date_preg ~ 1, TRUE ~ 0),
    
    SCZ_prior_to_first = case_when(first_date_SCZ < first_preg ~ 1, TRUE ~ 0),
    BIPOL_prior_to_first = case_when(first_date_BIPOL < first_preg ~ 1, TRUE ~ 0),
    ANX_prior_to_first = case_when(first_date_ANX < first_preg ~ 1, TRUE ~ 0),
    DEPR_prior_to_first = case_when(first_date_DEPR < first_preg ~ 1, TRUE ~ 0),
    
    SCZ_no_self_report = ifelse(!is.na(first_date_SCZ) & !sources_SCZ %in% c("Self-report only", "Self-report and other source(s)"), 1, 0),
    BIPOL_no_self_report = ifelse(!is.na(first_date_BIPOL) & !sources_BIPOL %in% c("Self-report only", "Self-report and other source(s)"), 1, 0),
    ANX_no_self_report = ifelse(!is.na(first_date_ANX) & !sources_ANX %in% c("Self-report only", "Self-report and other source(s)"),  1, 0),
    DEPR_no_self_report = ifelse(!is.na(first_date_DEPR) & !sources_DEPR %in% c("Self-report only", "Self-report and other source(s)"), 1, 0)) %>%
  ungroup() %>%
  select(-matches("first_date|sources"))
         

# export
write.csv(pregnancies_1,"processed/Step3_AddPsychiatricDxs.csv",row.names=F)


# clear the environment
rm(list = ls())
