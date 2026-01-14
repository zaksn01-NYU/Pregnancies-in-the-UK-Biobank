# Pregnancies-in-the-UK-Biobank

# Overview
This code identifies pregnancy-related clinical codes in the UK Biobank (UKB) and clusters these codes into distinct pregnancy events. Each pregnancy is classified according to delivery, abortive, or unknown outcome. 

# Required UK Biobank Data
To run this code, the following UKB resources must be downloaded:

## Database Tables
-***hesin.csv*** – Hospital Episode Statistics (HES) inpatient core dataset
-***hesin_diag.csv*** – HES inpatient diagnoses
-***gp_clinical.csv*** – General Practitioner (GP) clinical events

## UKB clinical coding classification systems and mapping files 
-***all_lkps_maps_v4.xlsx*** – downloaded from https://biobank.ndph.ox.ac.uk/ukb/ukb/auxdata/primarycare_codings.zip

## Participant sex and year of birth
-***demo.csv*** – A user-generated output file of UKB fields ‘Participant ID’, ‘Sex’, and ‘Year of birth’. The output file should be exported with field titles, and not field names, for compatibility with the included scripts

## Consent withdrawal
-***withdrawn.csv*** – A user-generated file containing the most recent list of participants who have withdrawn consent for data use. This should be a single column named ‘eid’ 

# Directory Structure
For compatibility with the included scripts, all data should be organized into the following two folders:
-***raw_UKB*** – contains all the UKB data files listed above
-***processed*** – contains all newly generated/processed data created by the scripts
