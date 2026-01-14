


# THE FOLLOWING SCRIPT CLASSIFIES PSYCHOTROPIC MEDICATION USE ------------------------------------------------------



# load packages
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("stringr")) install.packages("stringr"); library(stringr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)
if (!require("readxl")) install.packages("readxl"); library(readxl)

# load data
drugs = fread("raw_UKB/gp_scripts.csv",na.strings=c("", NA))
BNF_map = read_excel("raw_UKB/all_lkps_maps_v4.xlsx", na=c("", NA), sheet="bnf_lkp")
READ_map = read_excel("raw_UKB/all_lkps_maps_v4.xlsx", na=c("", NA), sheet="read_v2_drugs_lkp")
pregnancies = fread("processed/Step3_AddPsychiatricDxs.csv", na.strings=c("", NA))


# drug codes Read V2                  
antipsychotics_readv2 = c('^d4','^d5') 
moodstabilizers_readv2 = c('^dn','^d6')
anxiolyitcs_readv2 = c('^d2')
antidepressants_readv2 = c('^d7','^d8','^d9','^da')


# drug codes BNF                
antipsychotics_bnf = c('^040201') 
moodstabilizers_bnf = c('^040203','^040801')
anxiolyitcs_bnf = c('^040102')
antidepressants_bnf = c('^0403')


# remove '." from BNF codes
drugs$bnf_clean = gsub("\\.", "", drugs$bnf_code) 


# create the drug type columns
drugs = drugs %>%
  mutate(
    antipsychotics = ifelse(replace_na(str_detect(read_2, str_c(antipsychotics_readv2, collapse = "|")), FALSE) |
                              replace_na(str_detect(bnf_clean, str_c(antipsychotics_bnf, collapse = "|")), FALSE), 1, 0),
    
    moodstabilizers = ifelse(replace_na(str_detect(read_2, str_c(moodstabilizers_readv2, collapse = "|")), FALSE) |
                               replace_na(str_detect(bnf_clean, str_c(moodstabilizers_bnf, collapse = "|")), FALSE), 1, 0),
    
    anxiolytics = ifelse(replace_na(str_detect(read_2, str_c(anxiolyitcs_readv2, collapse = "|")), FALSE) |
                           replace_na(str_detect(bnf_clean, str_c(anxiolyitcs_bnf, collapse = "|")), FALSE), 1, 0),
    
    antidepressants = ifelse(replace_na(str_detect(read_2, str_c(antidepressants_readv2, collapse = "|")), FALSE) |
                               replace_na(str_detect(bnf_clean, str_c(antidepressants_bnf, collapse = "|")), FALSE), 1, 0))

  
# keep rows with relevant drugs AND those not yet determined from D+MD codes
drugs_keep = drugs %>% 
  mutate(dmd_fill = ifelse(is.na(read_2) & is.na(bnf_code) & !is.na(dmd_code) & !is.na(drug_name), 1, 0)) %>%
  filter(antipsychotics == 1 | moodstabilizers == 1 | anxiolytics == 1 | 
                                antidepressants == 1 | dmd_fill == 1)


## To identify drugs through DM+D, gather keywords for each drug type:

# function to extract keywords for the drug types
extract_keywords = function(bnf_codes, read_codes, BNF_map, READ_map) {
  
  bnf_terms = BNF_map %>%
    filter(str_detect(BNF_Presentation_Code, str_c(bnf_codes, collapse = "|"))) %>%
    select(BNF_Product, BNF_Chemical_Substance) %>%
    pivot_longer(cols = everything()) %>%
    pull(value)
  
  read_terms = READ_map %>%
    filter(str_detect(read_code, str_c(read_codes, collapse = "|"))) %>%
    select(term_description) %>%
    pivot_longer(cols = everything()) %>%
    pull(value)
  
  unique(c(bnf_terms, read_terms))}


# specify terms to exclude from keywords
exclude_vector = c(
  "acet", "acetate", "acid", "acuphase", "addon", "adult", "ampoule", "ampoules", "astrazeneca", "ayrtons diazepam", "beads", "beecham", "boots", "bnf", 
  "carbonate", "cap", "caps", "capsules", "capsule",   
  "carb", "central", "chewable", "chronosphere", "cit", "citrate", "clp pharm", "CNS", "co", "community", "conc",  "concentrate",  "consta", "control", "controlled release", 
  "controlled-release", "cp", "cr", "crushable", "decan","decanoate", "del", "depot", "depot", "dhe", "dihydrochloride", "dipot", "dipotassium", "dispersible", 
  "drops", "drug", "drugs", "dup", "elixir", "embon", "embonate", "enatate", "enantate", "ep", "enanthate", "for", "forte", "free", "fte", "generic", "g", "granules", "gx", "healthaid htp", "HCl", "htp", "hydrochloride", 
  "hydrobromide", "hydrob", "hydrogen", "imap", "immediate release", "immediate-release", "in",  "infusion", "infatabs", "inj", "intramuscular", "injection", 
  "injections", "initiation", "ir", "iv", "li", "liq", "liquid", "low", "mal", "maleate", "maintena", "mg", "mgs", "micrograms", "mL", "modified-release", 
  "modified release", "monotherapy", "morningside", "mr", "nervous", "new", "numark", "of", "oily", "only", "oral", "orange", "oro", "other", "pack", "palm", "palmitate", 
  "pamoate", "parent", "pep", "pdr", "pfs", "pharm", "prefilled", "preps", "prepn", "pdrsolvent", "powder", "powder+solvent", "powdersolvent", "pro-Plus", "prolonged-release", 
  "prolonged release", "pr", "pack", "quicklet", "recon", "rectal", "retard", "s3b", "sachet", "sachets", "salt", "salts", "see", "sls", "smithkline", "sod", "sodium", "solution", "solv", "solvent", 
  "sprinkle", "starter", "sublingual", "suspension", "sugar", "succ", "succinate", "sulf", "sulfate", "suppositories", "suppository", "susp",
  "suspension", "syrp", "syrup", "system", "systemic", "systemic",  "syringe", "syringes", "tab",
  "tablet", "tablets", "tabs", "titration", "treatment", "uk", "use", "velotab", "volume", "v2", "xl", "xr")


# specify patterns to exclude   
exclude_pattern = paste0(
  "\\b(", paste(exclude_vector, collapse = "|"), ")\\b",      # exact terms from above
  "|\\b(?=\\w*[A-Za-z])(?=\\w*\\d)\\w+\\b",                   # words with both letters and digits
  "|[^\\w\\s]",                                               # non-alphanumeric characters (except space)
  "|\\S*/\\S*",                                               # words with a slash
  "|\\([^)]*\\)",                                             # anything in parentheses
  "|\\[[^]]*\\]",                                             # anything in brackets
  "|\\d+")                                                    # exclude numbers (standalone)


# function to clean the key words according to the logic above
clean_keywords = function(keywords, exclude_pattern) {
  keywords %>%
    str_remove_all(regex(exclude_pattern, ignore_case = TRUE)) %>%
    str_trim() %>%
    tolower() %>%
    unique() %>%
    .[. != ""]}


# combined input for extraction + cleaning
drug_classes = list(
  antipsychotics = list(bnf = antipsychotics_bnf, read = antipsychotics_readv2),
  moodstabilizers = list(bnf = moodstabilizers_bnf, read = moodstabilizers_readv2),
  anxiolytics = list(bnf = anxiolyitcs_bnf, read = anxiolyitcs_readv2),
  antidepressants = list(bnf = antidepressants_bnf, read = antidepressants_readv2))


# extract + clean keywords
for (drug in c("antipsychotics", "moodstabilizers", "anxiolytics", "antidepressants")) {
  raw_keywords = extract_keywords(
    drug_classes[[drug]]$bnf,
    drug_classes[[drug]]$read,
    BNF_map,
    READ_map)
  
  cleaned = clean_keywords(raw_keywords, exclude_pattern)
  assign(paste0(drug, "_keywords_cleaned"), cleaned)}


# match keywords to DM+D drug descriptions to classify those drugs
drugs_keep = drugs_keep %>%
  mutate(antipsychotics_dmd = ifelse(dmd_fill == 1,
                                     as.integer(str_detect(drug_name, regex(paste0("\\b(", paste(antipsychotics_keywords_cleaned, collapse = "|"), ")\\b"),
                                                                            ignore_case = TRUE))), 0),
         moodstabilizers_dmd = ifelse(dmd_fill == 1,
                                      as.integer(str_detect(drug_name, regex(paste0("\\b(", paste(moodstabilizers_keywords_cleaned, collapse = "|"), ")\\b"),
                                                                             ignore_case = TRUE))), 0),
         anxiolytics_dmd = ifelse(dmd_fill == 1,
                                  as.integer(str_detect(drug_name, regex(paste0("\\b(", paste(anxiolytics_keywords_cleaned, collapse = "|"), ")\\b"),
                                                                         ignore_case = TRUE))), 0),
         antidepressants_dmd = ifelse(dmd_fill == 1,
                                      as.integer(str_detect(drug_name, regex(paste0("\\b(", paste(antidepressants_keywords_cleaned, collapse = "|"), ")\\b"),
                                                                             ignore_case = TRUE))), 0))

#combine DM+D drug classifications with READ and BNF
drugs_keep_1 = drugs_keep %>%
  mutate(Rx = case_when(antipsychotics == 1 | antipsychotics_dmd == 1 ~ "antipsychotics",
                        moodstabilizers == 1 | moodstabilizers_dmd == 1 ~ "moodstabilizers",
                        anxiolytics == 1 | anxiolytics_dmd == 1 ~ "anxiolytics",
                        antidepressants == 1 | antidepressants_dmd ~ "antidepressants"),
         issue_date = as.Date(issue_date, format = "%Y-%m-%d")) %>%
  filter(!is.na(issue_date)) %>% # remove drugs without a date
  filter(!is.na(Rx)) %>% # remove rows that aren't the drugs of interest
  select(-contains(c("_dmd", "dmd_fill"))) %>%
  distinct() # remove duplicates


# get the earliest prescription date for each medication type
drugs_summarized = drugs_keep_1 %>%
  group_by(eid, Rx) %>%
  summarize(
    first_date = min(issue_date),
    n_Rx = n(),
    .groups = "drop")


# pivot
drugs_wide = drugs_summarized %>%
  pivot_wider(
    id_cols = eid,
    names_from = Rx,
    values_from = c(first_date, n_Rx),
    names_glue = "{.value}_{Rx}")


# combine with pregnancies
pregnancies_1 = pregnancies %>% 
  left_join(drugs_wide) %>%
  mutate(
    antipsychotics_YN = ifelse(!is.na(first_date_antipsychotics), 1, 0),
    moodstabilizers_YN = ifelse(!is.na(first_date_moodstabilizers), 1, 0),
    anxiolytics_YN = ifelse(!is.na(first_date_anxiolytics), 1, 0),
    antidepressants_YN = ifelse(!is.na(first_date_antidepressants), 1, 0),
    
    antipsychotics_prior = case_when(first_date_antipsychotics < min_date_preg ~ 1, TRUE ~ 0),
    moodstabilizers_prior = case_when(first_date_moodstabilizers < min_date_preg ~ 1, TRUE ~ 0),
    anxiolytics_prior = case_when(first_date_anxiolytics < min_date_preg ~ 1, TRUE ~ 0),
    antidepressants_prior = case_when(first_date_antidepressants < min_date_preg ~ 1, TRUE ~ 0),
    
    antipsychotics_prior_to_first = case_when(first_date_antipsychotics < first_preg ~ 1, TRUE ~ 0),
    moodstabilizers_prior_to_first = case_when(first_date_moodstabilizers < first_preg ~ 1, TRUE ~ 0),
    anxiolytics_prior_to_first = case_when(first_date_anxiolytics < first_preg ~ 1, TRUE ~ 0),
    antidepressants_prior_to_first = case_when(first_date_antidepressants < first_preg ~ 1, TRUE ~ 0),
    
    antipsychotics_multi = ifelse(!is.na(first_date_antipsychotics) & n_Rx_antipsychotics >=2, 1, 0),
    moodstabilizers_multi = ifelse(!is.na(first_date_moodstabilizers) & n_Rx_moodstabilizers >=2, 1, 0),
    anxiolytics_multi = ifelse(!is.na(first_date_anxiolytics) & n_Rx_anxiolytics >=2, 1, 0),
    antidepressants_multi = ifelse(!is.na(first_date_antidepressants) & n_Rx_antidepressants >=2, 1, 0)) %>%
  select(-matches("(Rx|first_date)"))


# export
write.csv(pregnancies_1,"processed/Step4_AddMedications.csv",row.names=F)


# clear the environment
rm(list = ls())
