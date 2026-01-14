


# THE FOLLOWING SCRIPT RUNS THE MIXED MODELS BY DEFINING INPUT VARIABLES FOR EACH ANALYSIS AND SOURCING THE MODEL LOOP --------



# load packages
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("stringr")) install.packages("stringr"); library(stringr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)
if (!require("lme4")) install.packages("lme4"); library(lme4)




# SETUP -------------------------------------------------------------------

# load data
pregnancies = fread("processed/Step6_DefinePregnancyOutcomes.csv", na.strings=c("", NA))


# specify psychiatric exposures
psych_exposures = c("SCZ_YN","BIPOL_YN","ANX_YN","DEPR_YN",
                    "PRS_SCZ", "PRS_BIPOL", "PRS_ANX", "PRS_DEPR",
                    "antipsychotics_YN","moodstabilizers_YN","anxiolytics_YN","antidepressants_YN") 


# specify obstetric outcomes
obstetric_outcomes = c("Ectopic", "Molar", "SpontaneousAbortion", "Hypertensive", "Hemorrhage", "Vomiting", 
                       "Venous", "Diabetes", "AmnioticFluid", "Preterm", "AbnormalLabor")


# list all the pre-defined variables above
keep = c("pregnancies", "psych_exposures", "obstetric_outcomes", "keep")



# PRIMARY ANALYSIS --------------------------------------------------------

# specify input
dataset = pregnancies
exposures = psych_exposures
outcomes = obstetric_outcomes

source("Step7B_AnalysesLoop.R", local = TRUE)

# export
write.csv(main_effects_table, "processed/PrimaryAnalysis.csv", row.names=F) # ORs, 95% CIs, and Pvals for main effects
write.csv(main_effects_long, "processed/PrimaryAnalysis_output_for_plots.csv", row.names=F) # long format for plotting
write.csv(all_raw_data_unformatted, "processed/PrimaryAnalysis_raw_output.csv", row.names=F) # all raw data

rm(list = setdiff(ls(), keep))



# SENSITIVITY ANALYSIS 1: EXPOSURES PRIOR TO PREGNANCY --------------------

# specify input
dataset = pregnancies
exposures = gsub("_YN", "_prior", psych_exposures[!grepl("PRS", psych_exposures)])
outcomes = obstetric_outcomes

source("Step7B_AnalysesLoop.R", local = TRUE)

# export
write.csv(main_effects_table, "processed/Sensitivity1_ExposuresPrior.csv", row.names=F)

rm(list = setdiff(ls(), keep))



# SENSITIVITY ANALYSIS 2: EXPOSURES PRIOR TO FIRST PREGNANCY --------------

# specify input
dataset = pregnancies
exposures = gsub("_YN", "_prior_to_first", psych_exposures[!grepl("PRS", psych_exposures)])
outcomes = obstetric_outcomes

source("Step7B_AnalysesLoop.R", local = TRUE)

# export
write.csv(main_effects_table, "processed/Sensitivity2_ExposuresPriorToFirst.csv", row.names=F)

rm(list = setdiff(ls(), keep))



# SENSITIVITY ANALYSIS 3: PURE PSYCHIATRIC EXPOSURE GROUPS ---------------

# specify input
dataset = pregnancies %>%
  mutate(Num_Dxs = rowSums(select(., SCZ_YN,BIPOL_YN,ANX_YN,DEPR_YN)),
         Num_Meds = rowSums(select(., antipsychotics_YN,moodstabilizers_YN,anxiolytics_YN,antidepressants_YN)),
  
         #*where >1 comorbidity, make NA*#
  across(c(SCZ_YN, BIPOL_YN, ANX_YN, DEPR_YN), ~ ifelse(Num_Dxs > 1, NA, .)),
  across(c(antipsychotics_YN, moodstabilizers_YN, anxiolytics_YN, antidepressants_YN), ~ ifelse(Num_Meds > 1, NA, .)))
  
exposures = psych_exposures[!grepl("PRS", psych_exposures)]
outcomes = obstetric_outcomes

source("Step7B_AnalysesLoop.R", local = TRUE)

# export
write.csv(main_effects_table, "processed/Sensitivity3_PureExposures.csv", row.names=F)

rm(list = setdiff(ls(), keep))



# SENSITIVITY ANALYSIS 4: EXCLUDING SELF-REPORTED PSYCHIATRIC DIAGNOSES --------

# specify input
dataset = pregnancies
exposures = c("SCZ_no_self_report", "BIPOL_no_self_report", "ANX_no_self_report", "DEPR_no_self_report")
outcomes = obstetric_outcomes

source("Step7B_AnalysesLoop.R", local = TRUE)

# export
write.csv(main_effects_table, "processed/Sensitivity4_NoSelfReportedExposures.csv", row.names=F)

rm(list = setdiff(ls(), keep))



# SENSITIVITY ANALYSIS 5: AT LEAST TWO MEDICATION PRESCRIPTIONS  ----------

# specify input
dataset = pregnancies
exposures = c("antipsychotics_multi", "moodstabilizers_multi", "anxiolytics_multi", "antidepressants_multi")
outcomes = obstetric_outcomes

source("Step7B_AnalysesLoop.R", local = TRUE)

# export
write.csv(main_effects_table, "processed/Sensitivity5_MultipleScripts.csv", row.names=F)

rm(list = setdiff(ls(), keep))



# SENSITIVITY ANALYSIS 6: FIRST PREGNANCY ---------------------------------

# specify input
dataset = pregnancies %>%
  group_by(eid) %>%
  filter(max_date_preg == min(max_date_preg)) %>%
  ungroup()

exposures = psych_exposures
outcomes = obstetric_outcomes

## you will receive warning message; "fixed-effect model matrix is rank deficient so dropping 1 column / coefficient" 
## this is due to parity / rpl which equal 0 in the first pregnancy. 

source("Step7B_AnalysesLoop.R", local = TRUE)

# export
write.csv(main_effects_table, "processed/Sensitivity6_FirstPregnancy.csv", row.names=F)

rm(list = setdiff(ls(), keep))



# SENSITIVITY ANALYSIS 7: EXCLUDING PREGNANCIES WITH UNKNOWN OUTCOME --------

# specify input
dataset = pregnancies %>%
  filter(!grepl("unknown", eid_pregnancy))
exposures = psych_exposures
outcomes = obstetric_outcomes

source("Step7B_AnalysesLoop.R", local = TRUE)

# export
write.csv(main_effects_table, "processed/Sensitivity7_NoUnknownPregnancies.csv", row.names=F)

rm(list = setdiff(ls(), keep))



# DEPRESSION VS ANTIDEPRESSANTS -------------------------------------------

# specify input
dataset_depressed = pregnancies %>% filter(DEPR_prior == 1)


# Hemorrhage
mod_Hem = glmer(Hemorrhage ~ antidepressants_prior + age_at_preg + BirthYear + TownsendDeprivationIndex + University + delivery + parity + rpl + (1 | eid),
                             data = dataset_depressed, family = binomial, nAGQ = 0)
mod_Hem = data.frame(summary(mod_Hem)$coefficients) %>%
  mutate(Outcome = "Hemorrhage")


# Spontaneous Abortion
mod_SponAbor = glmer(SpontaneousAbortion ~ antidepressants_prior + age_at_preg + BirthYear + TownsendDeprivationIndex + University + parity + rpl + (1 | eid),
            data = dataset_depressed, family = binomial, nAGQ = 0)
mod_SponAbor = data.frame(summary(mod_SponAbor)$coefficients) %>%
  mutate(Outcome = "SpontaneousAbortion")


# Vomiting
mod_Vom = glmer(Vomiting ~ antidepressants_prior + age_at_preg + BirthYear + TownsendDeprivationIndex + University + delivery + parity + rpl + (1 | eid),
            data = dataset_depressed, family = binomial, nAGQ = 0)
mod_Vom = data.frame(summary(mod_Vom)$coefficients) %>%
  mutate(Outcome = "Vomiting")


# combine models and format for export
DeprVsAntidepr =  mod_Hem %>%
  bind_rows(mod_SponAbor) %>%
  bind_rows(mod_Vom) %>%
  filter(grepl("antidepressants_prior", rownames(.))) %>%
  mutate(
    OR = exp(Estimate),
    CI_lower = exp(Estimate - 1.96 * Std..Error),
    CI_upper = exp(Estimate + 1.96 * Std..Error),
    OR_CI = sprintf("%.2f [%.2f-%.2f]", OR, CI_lower, CI_upper)) %>%
  select(c(Outcome, OR_CI, Pr...z..)) %>%
  pivot_wider(names_from = Outcome, values_from = c(OR_CI, Pr...z..))


# export
write.csv(DeprVsAntidepr, "processed/DeprVsAntidepr.csv", row.names=F)



# POSITIVE AND NEGATIVE OUTCOME CONTROLS ------------------------------------------

# specify input
dataset = pregnancies
exposures = psych_exposures
outcomes = c("NormalDeliv", "Glaucoma")

source("Step7B_AnalysesLoop.R", local = TRUE)

# export
write.csv(main_effects_table, "processed/PosNegControls.csv", row.names=F)

rm(list = setdiff(ls(), keep))


