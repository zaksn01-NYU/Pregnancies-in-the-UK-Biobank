


# THE FOLLOWING SCRIPT DEFINES OBSTETRIC OUTCOMES AND TRANSFORMS THE DATASET TO PREGNANCY-LEVEL WIDE FORMAT ------------------------------------------------------



# load packages
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("stringr")) install.packages("stringr"); library(stringr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)


# load data
pregnancies = fread("processed/Step5_AddAdditionalVars.csv", na.strings=c("", NA))


# define the outcomes
pregnancies = pregnancies %>%
  mutate(eid_pregnancy = paste(eid, pregnancy, sep = "-")) %>%
  group_by(eid_pregnancy) %>%
  mutate(
    Ectopic = ifelse(any(grepl("^O00", ICD10code)), 1, 0),
    Molar = ifelse(any(grepl("^O01", ICD10code)), 1, 0),
    SpontaneousAbortion = ifelse(any(grepl("^O03", ICD10code)),1, 0),
    Hypertensive = ifelse(any(grepl("^(O12|O13|O14|O15)", ICD10code)),1, 0),
    Hemorrhage = ifelse(any(grepl("^(O20|O44|O45|O46|O67|O72)", ICD10code)),1, 0),
    Vomiting = ifelse(any(grepl("^O21", ICD10code)),1, 0),
    Venous = ifelse(any(grepl("^(O22|O87|O88)", ICD10code)),1, 0),
    Diabetes = ifelse(any(grepl("^O244", ICD10code)),1, 0),
    AmnioticFluid = ifelse(any(grepl("^(O40|O41)", ICD10code)),1, 0),
    Preterm = ifelse(any(grepl("^(O42|O60|O343)", ICD10code)),1, 0),
    AbnormalLabor = case_when(
      grepl("deliv", eid_pregnancy) & any(grepl("^(O61|O62|O63|O64|O65|O66)", ICD10code)) ~ 1,
      grepl("deliv", eid_pregnancy) & !any(grepl("^(O61|O62|O63|O64|O65|O66)", ICD10code)) ~ 0,
      TRUE ~ NA_real_),
    NormalDeliv = ifelse(any(grepl("^O80", ICD10code)), 1, 0)) %>% # positive control
  ungroup()


# transform the dataset from person-level to pregnancy-level
pregnancies_1 = pregnancies %>%
  select(eid, eid_pregnancy, max_date_preg, age_at_preg, BirthYear, TownsendDeprivationIndex, University,
    Glaucoma, NormalDeliv, Ectopic, Molar, SpontaneousAbortion, Hypertensive, Hemorrhage, Vomiting, Venous, Diabetes, 
    AmnioticFluid, Preterm, AbnormalLabor, matches("_YN|_prior|_self_report|_multi|PRS_|^PC")) %>%
  mutate(across(-c(max_date_preg, age_at_preg, BirthYear, TownsendDeprivationIndex, matches("PRS_|^PC")), as.factor)) %>%
  distinct()
  

# add variables for number of previous deliveries (parity) and number of previous abortions (rpl, repeated pregnancy loss)
pregnancies_1 = pregnancies_1 %>%
  group_by(eid) %>%
  arrange(eid, max_date_preg) %>%
  mutate(delivery = ifelse(grepl("deliv", eid_pregnancy), 1, 0),
         parity = cumsum(delivery) - delivery,
         abort = ifelse(grepl("abort", eid_pregnancy), 1, 0),
         rpl = cumsum(abort) - abort) %>% 
  ungroup()
  

# export
write.csv(pregnancies_1,"processed/Step6_DefinePregnancyOutcomes.csv", row.names=F)


# clear the environment
rm(list = ls())

        