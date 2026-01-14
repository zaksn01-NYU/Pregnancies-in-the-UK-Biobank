


# THE FOLLOWING SCRIPT CLUSTERS ICD CODES TO FORM PREGNANCY EVENTS ------------------------------------------------------



# load packages
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("stringr")) install.packages("stringr"); library(stringr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)
if (!require("zoo")) install.packages("zoo"); library(zoo)


# load data
hospital = fread("processed/Step1A_ExtractPregnancyCodes_Hospital.csv", na.strings=c("", NA))
primarycare = fread("processed/Step1B_ExtractPregnancyCodes_PrimaryCare.csv", na.strings=c("", NA))
withdrawn = fread("raw_UKB/withdrawn.csv", na.strings=c("", NA)) %>% pull(eid) 
UKB_vars = fread("raw_UKB/pathways_additional_vars.csv", na.strings=c("", NA))
gp_clinical = fread("raw_UKB/gp_clinical.csv", na.strings=c("", NA)) %>% pull(`Participant ID`) %>% unique()


# merge hospital + primary care data
pregnancies = full_join(hospital, primarycare) %>%
  distinct() # remove duplicates


# classify the delivery and abortive outcome codes as such
delivery_codes = c("O6", "O7", "O8", "O91", "O92", "O94", "V24", "Z37", "Z38", "Z39")
abortive_codes = c("O00", "O01", "O02", "O03", "O04", "O06", "O08")

pregnancies_1 = pregnancies %>%
  mutate(Delivery = grepl(paste0("^(", paste(delivery_codes, collapse = "|"), ")"), ICD10code),
         Abortive = grepl(paste0("^(", paste(abortive_codes, collapse = "|"), ")"), ICD10code),
         Date = as.Date(Date, format = "%Y-%m-%d")) 

# Define a function that clusters pregnancy outcomes codes (deliveries, abortive outcomes) belonging to the same vs different pregnancies. 
# It groups the first date (the anchor date) with succeeding dates as long as they are within [gap_days] of the anchor date. Once a date 
# is more than [gap_days] days from the anchor date, that becomes the new anchor date for the succeeding pregnancy.

group_by_anchor <- function(.x, .y, date_col = "Date", gap_days, group_col = "grouper") {
  dates <- .x[[date_col]]
  n <- length(dates)
  group_ids <- integer(n)
  
  if (n == 1) {
    group_ids[1] <- 1
  } else {
    group_id <- 1
    anchor <- dates[1]
    group_ids[1] <- group_id
    
    for (i in 2:n) {
      if (as.numeric(difftime(dates[i], anchor, units = "days")) > gap_days) {
        group_id <- group_id + 1
        anchor <- dates[i]
      }
      group_ids[i] <- group_id
    }
  }
  
  .x[[group_col]] <- group_ids
  .x
}

#
#
#

# cluster delivery codes that belong to the same pregnancy #
gap_days = 322

df_Delivery = pregnancies_1 %>%
  filter(Delivery) %>%
  arrange(eid, Date) %>%
  group_by(eid) %>%
  group_modify(~ group_by_anchor(.x, .y, date_col = "Date", gap_days = gap_days)) %>%
  ungroup() %>%                          
  mutate(deliv_ID = paste("deliv", grouper, sep = "_")) %>% 
  group_by(eid, deliv_ID) %>%
  mutate(max_date_deliv = max(Date)) %>%
  ungroup() %>%
  select(-grouper)


# cluster abortive outcome codes that belong to the same pregnancy #
gap_days = 189

df_Abortive = pregnancies_1 %>%
  filter(Abortive == TRUE) %>%  
  arrange(eid, Date) %>%                       
  group_by(eid) %>%
  group_modify(~ group_by_anchor(.x, .y, date_col = "Date", gap_days = gap_days)) %>%
  ungroup() %>%                     
  mutate(abort_ID = paste("abort", grouper, sep = "_")) %>% 
  group_by(eid, abort_ID) %>%
  mutate(max_date_abort = max(Date)) %>%
  ungroup() %>%
  select(-grouper)


# merge delivery and abortive outcome subsets back in with the pregnancy cohort
pregnancies_2 = pregnancies_1 %>%
  full_join(df_Delivery) %>%
  full_join(df_Abortive) %>%
  filter(Date >= as.Date("1950-01-01", format = "%Y-%m-%d"), # remove rows with dates before 1950
         !eid %in% withdrawn,  # remove the withdrawn individuals
         eid %in% UKB_vars$`Participant ID`[UKB_vars$Sex == "Female"], # remove males
         eid %in% gp_clinical)# remove individuals without primary care linkage


# clean the environment
rm(hospital, primarycare, withdrawn, gp_clinical)


# compare pregnancy outcome dates to dates of other codes to determine if they belong to the same pregnancy
pregnancies_3 = pregnancies_2 %>%
  group_by(eid) %>%
  arrange(eid, Date, desc(is.na(deliv_ID)), desc(is.na(abort_ID))) %>%
  
  # fill with the last observation carried backward, which are the pregnancy outcomes
  mutate(next_non_na_deliv_ID        = na.locf(deliv_ID, na.rm = FALSE, fromLast = TRUE),
         next_non_na_max_date_deliv  = na.locf(max_date_deliv, na.rm = FALSE, fromLast = TRUE),
         next_non_na_abort_ID        = na.locf(abort_ID, na.rm = FALSE, fromLast = TRUE),
         next_non_na_max_date_abort  = na.locf(max_date_abort, na.rm = FALSE, fromLast = TRUE)) %>%
  
  # cluster codes to pregnancies if within respective time windows
  mutate(deliv_within_window = as.numeric(next_non_na_max_date_deliv - Date) <= 322,
         abort_within_window = as.numeric(next_non_na_max_date_abort - Date) <= 189,
         
         deliv_ID_mod       = ifelse(deliv_within_window, next_non_na_deliv_ID, NA),
         max_date_deliv_mod = ifelse(deliv_within_window, next_non_na_max_date_deliv, NA),
         abort_ID_mod       = ifelse(abort_within_window, next_non_na_abort_ID, NA),
         max_date_abort_mod = ifelse(abort_within_window, next_non_na_max_date_abort, NA)) %>%
  
  # resolve overlapping assignments
  mutate(overlap = !is.na(deliv_ID_mod) & !is.na(abort_ID_mod),
         
         deliv_ID_mod = case_when(
           overlap & max_date_abort_mod < max_date_deliv_mod ~ NA,
           TRUE ~ deliv_ID_mod),
         
         max_date_deliv_mod = case_when(
           overlap & max_date_abort_mod < max_date_deliv_mod ~ NA,
           TRUE ~ max_date_deliv_mod),
         
         abort_ID_mod = case_when(
           overlap & max_date_deliv_mod < max_date_abort_mod ~ NA,
           TRUE ~ abort_ID_mod),
         
         max_date_abort_mod = case_when(
           overlap & max_date_deliv_mod < max_date_abort_mod ~ NA,
           TRUE ~ max_date_abort_mod)) %>%
  
  ungroup() %>%
  select(-matches("next_non_na_|within_window"), -c(deliv_ID, max_date_deliv, abort_ID, max_date_abort)) %>%
  mutate(across(contains("date"), ~ as.Date(.x)),
         overlap = !is.na(deliv_ID_mod) & !is.na(abort_ID_mod))


# remove the deliveries and abortive outcomes too close to one another to be biologically possible
pregnancies_flag_short = pregnancies_3 %>% 
  distinct(eid, deliv_ID_mod, max_date_deliv_mod, abort_ID_mod, max_date_abort_mod) %>%
  mutate(date_ = coalesce(max_date_deliv_mod, max_date_abort_mod)) %>% # (temporary date column for ordering)
  filter(!is.na(date_)) %>%
  group_by(eid) %>%
  arrange(eid, date_) %>%
  
  # calculate lagged values
  mutate(lag_deliv = lag(max_date_deliv_mod),
         lag_abort = lag(max_date_abort_mod),
    
  # flag short intervals
         short_deliv = !is.na(max_date_deliv_mod) & (
           (!is.na(lag_abort) & as.numeric(max_date_deliv_mod - lag_abort) < 189) |
             (!is.na(lag_deliv) & as.numeric(max_date_deliv_mod - lag_deliv) < 189)),
    
         short_abort = !is.na(max_date_abort_mod) & (
           (!is.na(lag_deliv) & as.numeric(max_date_abort_mod - lag_deliv) < 42) |
             (!is.na(lag_abort) & as.numeric(max_date_abort_mod - lag_abort) < 42)),
    
    # determine which records to remove based on short time interval between delivery and abortive outcome...
         remove_short = case_when(
           !is.na(deliv_ID_mod) & lead(short_abort, default = FALSE) ~ TRUE,
           short_abort & !is.na(lag(deliv_ID_mod)) ~ TRUE,
      
           !is.na(abort_ID_mod) & lead(short_deliv, default = FALSE) ~ TRUE,
           short_deliv & !is.na(lag(abort_ID_mod)) ~ TRUE,
      
      # ...and when two consecutive deliveries or abortions are too close, flag to remove the latter 
           short_abort & !is.na(lag(abort_ID_mod)) ~ TRUE,
           short_deliv & !is.na(lag(deliv_ID_mod)) ~ TRUE,
           TRUE ~ FALSE)) %>%
  ungroup() %>%
  select(-date_, -lag_deliv, -lag_abort)


# remove the biologically impossible pregnancies...
pregnancies_4 = pregnancies_3 %>%
  full_join(pregnancies_flag_short) %>%
  mutate(pregnancy = coalesce(deliv_ID_mod, abort_ID_mod),
         max_date_preg = coalesce(max_date_deliv_mod, max_date_abort_mod)) 

# (count number of individuals with impossible pregnancies)
pregnancies_4 %>%
  filter(remove_short) %>%
  distinct(eid) %>%
  nrow()

# (count number of impossible pregnancies)
pregnancies_4 %>%
  filter(remove_short) %>%
  distinct(eid, pregnancy) %>%
  nrow()

# ...resume removing the biologically impossible pregnancies
pregnancies_4 = pregnancies_4 %>%
  filter(is.na(remove_short) | remove_short == FALSE) %>%
  select(-matches("short|_mod"))
    

# cluster codes that don't belong to a pregnancy with delivery or abortive outcome #
gap_days = 189

df_Unknown = pregnancies_4 %>%
  filter(is.na(pregnancy)) %>%   
  arrange(eid, Date) %>%                       
  group_by(eid) %>%
  group_modify(~ group_by_anchor(.x, .y, date_col = "Date", gap_days = gap_days)) %>%
  ungroup() %>%                     
  mutate(unknown_ID = paste("unknown", grouper, sep = "_")) %>% 
  group_by(eid, unknown_ID) %>%
  mutate(max_date_unknown = max(Date)) %>%
  ungroup() %>%
  select(-grouper)


# merge unknown outcome subset back with the pregnancy cohort
pregnancies_5 = pregnancies_4 %>%
  full_join(df_Unknown) %>%
  mutate(pregnancy = coalesce(pregnancy, unknown_ID),
         max_date_preg = as.Date(max_date_preg),
         max_date_preg = coalesce(max_date_preg, max_date_unknown)) %>%
  select(-c(unknown_ID, max_date_unknown)) %>%
  group_by(eid, pregnancy) %>%
  mutate(max_date_preg = max(Date),
         min_date_preg = min(Date)) %>%
  ungroup()


# collapse certain unknown outcome pregnancies with deliveries or abortive outcomes
pregnancies_collapsed = pregnancies_5 %>%
  distinct(eid, pregnancy, max_date_preg, min_date_preg) %>%
  group_by(eid) %>%
  mutate(
    # define input for logical flags
    deliv_window = 322,
    abort_window = 189,
    
    is_deliv = grepl("deliv", pregnancy),
    is_abort = grepl("abort", pregnancy),
    is_unknown = grepl("unknown", pregnancy),
    
    lag_is_deliv = grepl("deliv", lag(pregnancy)),
    lag_is_abort = grepl("abort", lag(pregnancy)),
    lag_is_unknown = grepl("unknown", lag(pregnancy)),
    
    lead_is_deliv = grepl("deliv", lead(pregnancy)),
    lead_is_abort = grepl("abort", lead(pregnancy)),
    
    # flags for whether two adjacent pregnancies should be collapsed
    collapse_deliv = (is_deliv | is_unknown) & 
      (lag_is_deliv | lag_is_unknown) &
      !(is_unknown & lag_is_unknown) &
      as.numeric(max_date_preg - lag(min_date_preg)) < deliv_window,
    
    collapse_abort = (is_abort | is_unknown) & 
      (lag_is_abort | lag_is_unknown) &
      !(is_unknown & lag_is_unknown) &
      as.numeric(max_date_preg - lag(min_date_preg)) < abort_window,
    
    # collapsing logic
    pregnancy_mod = case_when(
      is_unknown & lead_is_deliv & lead(collapse_deliv) ~ lead(pregnancy),
      is_unknown & lag_is_deliv & collapse_deliv        ~ lag(pregnancy),
      is_deliv & lag_is_deliv & collapse_deliv        ~ lag(pregnancy),
      
      is_unknown & lead_is_abort & lead(collapse_abort) ~ lead(pregnancy),
      is_unknown & lag_is_abort & collapse_abort        ~ lag(pregnancy),
      is_abort & lag_is_abort & collapse_abort        ~ lag(pregnancy),
      
      TRUE ~ pregnancy)) %>%
  ungroup() %>%
  select(-starts_with("is_"), -starts_with("lag_is_"), -starts_with("lead_is_"), -deliv_window, -abort_window)
           

# merge collapsed dataset back with pregnancy cohort           
pregnancies_6 = pregnancies_5 %>%
  full_join(pregnancies_collapsed) %>%
  mutate(pregnancy = pregnancy_mod) %>%
  group_by(eid, pregnancy) %>%
  mutate(max_date_preg = max(Date),
         min_date_preg = min(Date)) %>%
  ungroup()
  

# modify names of pregnancies
pregnancies_7 = pregnancies_6 %>%
  mutate(pregnancy_type = sub("\\d+", "", pregnancy)) %>%  # extract the pregnancy outcomes ("deliv", "abort", "unknown")
  group_by(eid, pregnancy_type) %>%
  mutate(unique_count = dense_rank(max_date_preg),
         pregnancy = paste(pregnancy_type, unique_count, sep = "")) %>%  # modify labels to reflect modified pregnancy counts
  ungroup() %>%
  group_by(eid, pregnancy) %>%
  mutate(max_date_preg = max(Date),
         min_date_preg = min(Date)) %>%
  ungroup() %>%
  select(c(eid, pregnancy, ICD10code, Date, source, max_date_preg, min_date_preg))


# remove pregnancies in mothers <15 or >49 years old at time of pregnancy
pregnancies_8 = pregnancies_7 %>%
  left_join(UKB_vars %>% select(`Participant ID`, `Year of birth`), by = c("eid" = "Participant ID")) %>%
  mutate(preg_year = format(as.Date(min_date_preg, format = "%Y-%m-%d"), "%Y"),
         age_at_preg = as.numeric(preg_year) - `Year of birth`) %>% # add variable Age At Pregnancy
  filter(age_at_preg >=15 & age_at_preg <=49) %>% # remove pregnancies in mothers <15 or >49 years old
  select(-preg_year)


# export
write.csv(pregnancies_8,"processed/Step2_ClusterPregnancies.csv", row.names=F)


# clear the environment
rm(list = ls())
