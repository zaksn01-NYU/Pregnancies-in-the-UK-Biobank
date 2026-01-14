

# ---- 1. Specify covariates ----

# specify covariates used in each model + iteration
iteration_covariates_base = list(
  c("age_at_preg", "BirthYear"),
  c("age_at_preg", "BirthYear", "TownsendDeprivationIndex", "University"),
  c("age_at_preg", "BirthYear", "TownsendDeprivationIndex", "University", "parity", "rpl"))


# specify principal component columns:
pc_covariates = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")


# ---- 2. Covariate adjustment function ----

# function to adjust for covariates depending on outcome group and exposure
adjust_covariates <- function(base_covars, outcome, exposure) {
  extra_covars <- c()
  
  # adjust for delivery (yes/no) if the outcome is not abnormal labor or an abortive outcome
  if (outcome %in% c("Hypertensive", "Hemorrhage", "Vomiting", 
                     "Venous", "Diabetes", "AmnioticFluid", "Preterm")) {
    extra_covars <- c("delivery")
  } 
  
  # PRS exposures are also adjusted for their principal components
  if (grepl("PRS", exposure)) { 
    extra_covars <- c(extra_covars, pc_covariates)}
  
  return(c(base_covars, extra_covars))}


# ---- 3. GLMM model runner ----
run_model_iteration <- function(iteration_num, outcome_vars, psych_cols, data) {
  
  covariates_base <- iteration_covariates_base[[iteration_num]]
  results_list <- list()
  
  for (outcome in outcome_vars) {
    outcome_results <- list()
    
    for (exposure in psych_cols) {
      
      # Adjust for covariates based on outcome and exposure
      covariates <- adjust_covariates(covariates_base, outcome, exposure)
      
      # Handle exposure scaling if it's a PRS
      if (grepl("PRS", exposure)) {
        exposure_term <- paste0("scale(", exposure, ")")
      } else {
        exposure_term <- exposure
      }
      
      # Build formula
      formula_str <- paste(outcome, "~", paste(c(exposure_term, covariates), collapse = " + "), "+ (1 | eid)")
      formula <- as.formula(formula_str)
      
      # Fit model with error handling
      model <- tryCatch({
        # Use data
        model_data <- data
        
        # Fit the model
        glmer(formula, data = model_data, family = binomial, nAGQ = 0)
      }, error = function(e) NULL)
      
      # Store results
      if (!is.null(model)) {
        coefs <- summary(model)$coefficients
        df <- data.frame(
          Exposure = exposure,
          Outcome = outcome,
          Column = rownames(coefs),
          Estimate = coefs[, 1],
          Std_Error = coefs[, 2],
          Z_value = coefs[, 3],
          P_value = coefs[, 4],
          stringsAsFactors = FALSE
        )
        outcome_results[[exposure]] <- df
      }
    }
    
    results_list[[outcome]] <- do.call(rbind, outcome_results)
  }
  
  iteration_df <- do.call(rbind, results_list)
  iteration_df$iteration <- iteration_num
  return(iteration_df)
}

# ---- 4. Run across iterations ----

# Run and combine results across iterations
all_raw_data_unformatted <- bind_rows(
  lapply(1:length(iteration_covariates_base), function(i) {
    run_model_iteration(i, outcomes, exposures, dataset)
  })
)


# ---- 5. Filter and format ----

# identify exposure-outcome combinations with cell counts of n < 5
smallest_cell = dataset %>%
  select(all_of(c(outcomes, exposures))) %>%
  pivot_longer(cols = all_of(exposures), names_to = "condition", values_to = "condition_value") %>%
  filter(condition_value == 1) %>%  # Keep rows where the psychiatric condition is present
  pivot_longer(cols = all_of(outcomes), names_to = "obstetric_col", values_to = "value") %>%
  filter(value == 1) %>%  # Keep rows where the obstetric outcome is present
  count(condition, obstetric_col) %>%
  mutate(five_plus = ifelse(n >= 5, paste(condition, obstetric_col, sep = "-"), NA))


# handle name change for PRS scores, e.g., "scale(PRS_SCZ)"
scaled_exposures = paste0("scale(", exposures[grepl("PRS", exposures)], ")")


# filter
main_effects_long = all_raw_data_unformatted %>%
  filter(Column %in% c(exposures, scaled_exposures)) %>% # keep only the main effects (i.e., exclude the effects of the covariates)
  mutate(five_plus = ifelse(paste(Exposure, Outcome, sep = "-") %in% smallest_cell$five_plus | grepl("PRS", Exposure), 1, 0)) %>%
  filter(five_plus == 1 & iteration == 3) %>% # keep only rows with frequencies >= 5 and the fully adjusted models
  mutate(
    P_value_FDR = p.adjust(P_value, method = "fdr")) %>% 
  ungroup() %>%
  mutate(
    OR = exp(Estimate), 
    CI_lower = exp(Estimate - 1.96 * Std_Error),
    CI_upper = exp(Estimate + 1.96 * Std_Error),
    OR_CI = sprintf("%.2f [%.2f-%.2f]", OR, CI_lower, CI_upper)) %>%
  select(-c(five_plus, iteration))


# main analysis
main_effects_table = main_effects_long %>%
  select(Exposure, Outcome, OR_CI, P_value, P_value_FDR) %>%
  pivot_wider(
    names_from = Outcome,
    values_from = c(OR_CI, P_value, P_value_FDR),
    names_vary = "slowest") 

