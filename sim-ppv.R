library(tidyverse)
library(marginaleffects)
# --- Simulation Parameters ---

# --- Parameters for Data Generation ---
n_reports <- 500 # Number of reports
n_items_per_report <- 20 # Number of potential items to extract per report
prevalence <- 0.30 # Proportion of items that are truly positive (need extraction)

# --- Extractor Characteristics ---
# Human Coders
n_human_coders_total <- 5
n_human_coders_per_report <- 2

# Beta distribution shape parameters for HUMAN sensitivity
# Mean ~ 0.98, lower variance
beta_sens_shape1 <- 99
beta_sens_shape2 <- 1

# Beta distribution shape parameters for HUMAN specificity
# Mean ~ 0.99, lower variance
beta_spec_shape1 <- 99
beta_spec_shape2 <- 1

# LLM's true sensitivity and specificity
sens_llm <- 0.99
spec_llm <- 0.99

# --- Hypothesis Testing / Estimation Parameters ---
# If you were doing non-inferiority/equivalence for PPV difference:
# non_inferiority_margin_ppv <- 0.05
# equivalence_bounds_ppv <- c(-non_inferiority_margin_ppv, Inf)
alpha <- 0.05
n_simulations <- 50 # Increase for more stable results


# --- Setup ---
human_coder_ids <- paste0("human_", 1:n_human_coders_total)
report_ids <- paste0("report_", 1:n_reports)
item_ids_base <- paste0("item_", 1:n_items_per_report)
llm_id <- "llm"

set.seed(1234)

results_ppv <- data.frame()

# --- Simulation Loop ---
# Using pbapply for progress bar, replace pblapply with lapply if not installed
results_list <- lapply(1:n_simulations, function(sim_num) {
  
  # --- Generate Human Characteristics for this simulation run ---
  human_sensitivities <- rbeta(n_human_coders_total, shape1 = beta_sens_shape1, shape2 = beta_sens_shape2)
  human_specificities <- rbeta(n_human_coders_total, shape1 = beta_spec_shape1, shape2 = beta_spec_shape2)
  
  extractor_lookup <- tibble(
    extractor_id = c(human_coder_ids, llm_id),
    sensitivity = c(human_sensitivities, sens_llm),
    specificity = c(human_specificities, spec_llm),
    is_llm = c(rep(0, n_human_coders_total), 1)
  )
  
  # --- Generate Data for this simulation run ---
  sim_data_list <- vector("list", n_reports)
  for (r in 1:n_reports) {
    # Select humans for this report
    selected_human_ids <- sample(human_coder_ids, n_human_coders_per_report, replace = FALSE)
    report_extractors <- c(selected_human_ids, llm_id)
    report_extractor_info <- extractor_lookup |> filter(extractor_id %in% report_extractors)
    
    # Generate item-level data for this report
    report_items_data <- tibble(
      report_id = report_ids[r],
      item_id = paste0(report_ids[r], "_", item_ids_base),
      # Generate ground truth for each item
      true_state = rbinom(n_items_per_report, 1, prevalence) # 1 = Positive, 0 = Negative
    ) |>
      # Cross join with extractors for this report
      crossing(report_extractor_info) |>
      # Simulate predictions based on true state and extractor sensitivity/specificity
      mutate(
        prob_predict_positive = if_else(true_state == 1, sensitivity, 1 - specificity),
        prediction = rbinom(n(), 1, prob_predict_positive) # 1 = Predicted Positive, 0 = Predicted Negative
      ) |>
      select(report_id, item_id, extractor_id, is_llm, true_state, prediction)
    
    sim_data_list[[r]] <- report_items_data
  }
  
  sim_data_long <- bind_rows(sim_data_list)
  
  # --- Analysis: Estimate PPV difference ---
  
  # Filter data: PPV is conditional on making a positive prediction
  sim_data_positive_predictions <- sim_data_long |>
    filter(prediction == 1)
  
  # Check if we have enough data after filtering
  n_llm_pos <- sum(sim_data_positive_predictions$is_llm == 1)
  n_human_pos <- sum(sim_data_positive_predictions$is_llm == 0)
  
  if(n_llm_pos < 10 || n_human_pos < 10) {
    warning(paste("Simulation", sim_num, ": Insufficient positive predictions for reliable GLM fit. LLM:", n_llm_pos, "Human:", n_human_pos))
    # Return NA results for this simulation iteration
    return(data.frame(term = "is_llm1", comparison = "difference",
                      estimate = NA_real_, std.error = NA_real_, statistic = NA_real_,
                      p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
                      predicted_lo = NA_real_, predicted_hi = NA_real_, predicted = NA_real_,
                      true_state = NA_real_, is_llm = NA_real_ # Adjust if different vars used
                      #, p.value.noninf = NA_real_ # Add if equivalence testing
    ))
  }
  
  
  # Fit GLM model: Predict true state (correctness) given a positive prediction
  # The outcome 'true_state' (0 or 1) here represents whether a positive prediction was correct
  model_fit_ppv <- glm(true_state ~ is_llm,
                       data = sim_data_positive_predictions,
                       family = binomial(link = "logit"))
  
  # Calculate difference in P(true_state=1 | prediction=1) using marginal effects
  # This difference is the PPV(LLM) - PPV(Human Average)
  # Use two-way clustering: by report and by the original extractor (coder/LLM)
  # Note: Clustering by item_id might also be reasonable depending on assumptions.
  # Clustering by extractor_id accounts for within-extractor correlation.
  # Clustering by report_id accounts for within-report correlation.
  comparison_ppv <- avg_comparisons(
    model_fit_ppv,
    variables = "is_llm",
    type = "response", # Get difference in probabilities (PPVs)
    vcov = ~report_id + extractor_id # Two-way clustering
    # equivalence = equivalence_bounds_ppv # Uncomment if doing non-inferiority
  )
  
  # Return relevant results
  return(comparison_ppv |> data.frame() |> select(estimate, conf.low, conf.high, p.value)) # Add p.value.noninf if used
})

# Combine results from all simulations
results_ppv <- bind_rows(results_list) |>
  mutate(sim = row_number()) |>
  filter(!is.na(estimate)) # Remove sims that failed check

# --- Summarize Results ---

# Average Estimated Difference in PPV
mean_ppv_diff <- mean(results_ppv$estimate, na.rm = TRUE)
print(paste("Average Estimated PPV Difference (LLM - Human):", round(mean_ppv_diff, 4)))

# Coverage of the Confidence Intervals (should be close to 1 - alpha = 0.95)
# Calculate the 'true' PPVs for this setup (can be complex with varying humans)
# For simplicity, let's approximate true human PPV using average sens/spec
avg_human_sens <- mean(rbeta(10000, beta_sens_shape1, beta_sens_shape2)) # Approximation
avg_human_spec <- mean(rbeta(10000, beta_spec_shape1, beta_spec_shape2)) # Approximation

# Calculate True PPV based on prevalence and average characteristics
calc_ppv <- function(sens, spec, prev) {
  tp = sens * prev
  fp = (1 - spec) * (1 - prev)
  ppv = tp / (tp + fp)
  return(ppv)
}

true_ppv_llm <- calc_ppv(sens_llm, spec_llm, prevalence)
true_ppv_human_avg <- calc_ppv(avg_human_sens, avg_human_spec, prevalence)
true_ppv_diff <- true_ppv_llm - true_ppv_human_avg

print(paste("Approx True PPV Difference (LLM - Human):", round(true_ppv_diff, 4)))

coverage <- mean(results_ppv$conf.low <= true_ppv_diff & results_ppv$conf.high >= true_ppv_diff, na.rm = TRUE)
print(paste("Confidence Interval Coverage:", round(coverage, 3)))

# Power calculation (example for H1: LLM PPV > Human PPV)
power_gt <- mean(results_ppv$p.value / 2 < alpha & results_ppv$estimate > 0, na.rm = TRUE) # One-sided test adjustment
print(paste("Power (H1: PPV_LLM > PPV_Human):", round(power_gt, 3)))

# Power calculation (example for non-inferiority H1: LLM PPV - Human PPV > -margin)
# Ensure you added 'p.value.noninf' to the select() if using equivalence in avg_comparisons
# non_inf_margin <- 0.05 # Example margin
# power_noninf <- mean(results_ppv$conf.low > -non_inf_margin, na.rm = TRUE)
# # Or using the p.value.noninf if calculated:
# # power_noninf_p <- mean(results_ppv$p.value.noninf < alpha, na.rm = TRUE)
# print(paste("Power (Non-inferiority, margin=", non_inf_margin, "):", round(power_noninf, 3)))


# --- Plot Results ---
results_ppv |>
  ggplot(aes(x = sim, y = estimate)) +
  geom_point(alpha = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  geom_hline(yintercept = true_ppv_diff, color = "blue", linetype = "dashed") +
  # geom_hline(yintercept = -non_inf_margin, color = "red", linetype = "dotted") + # If relevant
  theme_light() +
  labs(
    title = "Simulation Results: PPV Difference (LLM - Human)",
    subtitle = paste("Based on", nrow(results_ppv), "successful simulations."),
    x = "Simulation Run",
    y  = "Estimated PPV Difference",
    caption = paste("True Difference Approx:", round(true_ppv_diff, 4),
                    "\nAvg Sens/Spec (Human):", round(avg_human_sens,3), "/", round(avg_human_spec,3),
                    "Sens/Spec (LLM):", sens_llm, "/", spec_llm,
                    "Prevalence:", prevalence)
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )