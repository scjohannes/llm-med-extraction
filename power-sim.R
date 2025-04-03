library(tidyverse)
library(marginaleffects)
library(lme4)

# I think we have type error inflation for but only in one sense. For each
# simulation the true difference between humans and the LLM is slightly
# different, depending on the sample from the beta distribution and depending on
# how many reports everyone analyses. I think, but have not implemented yet, the
# the 95% CI will cover these varying true difference between humans and the llm
# 95% of the time. But not the "grand" truth from which we sample. I will have
# to check, but I think we only have type I error inflation in the 2nd sense.

# How to check. Keep performance and number of reports extracted by each person
# the same for each simulation. Ground truth should be difference between
# weighted mean of humans and the single value of llm. We will have some
# variation because we still do berounlli trials.

# UPDATE: I implemented something truth at the level of the simulation, but the
# type I error is STILL TOOO HIGH!

# --- Simulation Parameters ---

# --- Parameters for Data Generation ---
# Number of human coders involved in the study
n_human_coders_total <- 10 # Total pool of human coders
n_human_coders_per_report <- 2 # Number of humans coding each report

# Beta distribution shape parameters for human coder accuracy
# Higher shape1 relative to shape2 means higher mean accuracy.
# Higher sum (shape1+shape2) means lower variance.
beta_shape1 <- 80
beta_shape2 <- 4.204

# Check mean: beta_shape1 / (beta_shape1 + beta_shape2) -> ~0.95

# LLM's true accuracy
p_llm   <- 0.95 # Assumed true accuracy for LLM

# --- Hypothesis Testing Parameters ---
non_inferiority_margin <- 0.05
equivalence_bounds <- c(-non_inferiority_margin, Inf) # Test H1: LLM - Human > -0.03
alpha <- 0.05
n_simulations <- 500 # Increase for more stable results
n_reports <- 500

human_coder_ids <- paste0("human_", 1:n_human_coders_total)
report_ids <- paste0("report_", 1:n_reports)
llm_id <- "llm"

set.seed(1234)

results <- data.frame()

for(i in 1:100){
message(paste0("Simulation ", i))
  
human_accuracies <- rbeta(n_human_coders_total, shape1 = beta_shape1, shape2 = beta_shape2)

extractor_lookup <- tibble(
    extractor_id = c(human_coder_ids, llm_id),
    accuracy_p = c(human_accuracies, p_llm),
    is_llm = c(rep(0, n_human_coders_total), 1)
  )

sim_data_list <- vector("list", n_reports)

for (i in 1:n_reports) {
  selected_human_ids <- sample(human_coder_ids, n_human_coders_per_report, replace = FALSE)
  report_extractors <- c(selected_human_ids, llm_id)
  report_extractor_info <- extractor_lookup |> filter(extractor_id %in% report_extractors)
  report_data <- report_extractor_info |>
    mutate(
      report_id = report_ids[i],
      correct = rbinom(n(), 1, accuracy_p)
    ) |>
    select(report_id, extractor_id, is_llm, correct) # Removed accuracy_p
  sim_data_list[[i]] <- report_data
}

sim_data_long <- bind_rows(sim_data_list)
  
# Fit Simple GLM Model (ignoring clustering structure here)
model_fit <- glm(correct ~ is_llm, data = sim_data_long, family = binomial(link = "logit"))

# Calculate difference using marginal effects. Uses delta method and cluster robust standard errors.
comparison <- avg_comparisons(
      model_fit,
      variables = "is_llm",
      type = "response",
      # Specify two-way clustering formula for vcov
      vcov = ~report_id + extractor_id,
      equivalence = equivalence_bounds
    )

results <- bind_rows(
  comparison |> data.frame() |> select(estimate, conf.low, conf.high, p.value, p.value.noninf), 
  results)
}

#Calculate Power
power <- mean(results$p.value.noninf < 0.05)


results |> 
  mutate(sim = row_number()) |> 
  data.frame() |> 
  ggplot(aes(x = sim, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  theme_light() +
  labs(
    x = "Simulations",
    y  = "Difference (LLM - Humans)"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


#### Repeat again, when LLM is actually worse to get Tupe I error rate
# Everything stays the same, except for llm-accuracy

p_llm   <- 0.899 # Just more than 5% worse. How many times wouuld we say that llm is not more than 5% worse, if it actually is?

set.seed(2024)

results <- data.frame()
results.ml <- data.frame()
true_difference_sim <- numeric()

for(i in 1:30){
  message(paste0("Simulation ", i))
  
  human_accuracies <- rbeta(n_human_coders_total, shape1 = beta_shape1, shape2 = beta_shape2)
  
  extractor_lookup <- tibble(
    extractor_id = c(human_coder_ids, llm_id),
    accuracy_p = c(human_accuracies, p_llm),
    is_llm = c(rep(0, n_human_coders_total), 1)
  )
  
  sim_data_list <- vector("list", n_reports)
  
  for (j in 1:n_reports) {
    selected_human_ids <- sample(human_coder_ids, n_human_coders_per_report, replace = FALSE)
    report_extractors <- c(selected_human_ids, llm_id)
    report_extractor_info <- extractor_lookup |> filter(extractor_id %in% report_extractors)
    report_data <- report_extractor_info |>
      mutate(
        report_id = report_ids[j],
        correct = rbinom(n(), 1, accuracy_p)
      ) |>
      select(report_id, extractor_id, is_llm, correct, accuracy_p)
    sim_data_list[[j]] <- report_data
  }
  
  sim_data_long <- bind_rows(sim_data_list)
  
  human_data_sim <- sim_data_long |> filter(is_llm == 0)
  true_mean_human_accuracy_sim <- sum(human_data_sim$accuracy_p) / nrow(human_data_sim)
  
  true_difference_sim[i] <- p_llm - true_mean_human_accuracy_sim
  
  
  # Fit Simple GLM Model (ignoring clustering structure here)
  model_fit <- glm(correct ~ is_llm, data = sim_data_long, family = binomial(link = "logit"))
  # ml.model_fit <- glmer(correct ~ is_llm + (is_llm|report_id) + (1|extractor_id), data = sim_data_long, family = binomial(link = "logit"))
  
  # Calculate difference using marginal effects. Uses delta method and cluster robust standard errors.
  comparison <- avg_comparisons(
    model_fit,
    variables = "is_llm",
    type = "response",
    # Specify two-way clustering formula for vcov
    vcov = ~report_id + extractor_id,
    equivalence = equivalence_bounds
  )
  
  # ml.comparison <- avg_comparisons(
  #   ml.model_fit,
  #   variables = "is_llm",
  #   type = "response",
  #   equivalence = equivalence_bounds,
  #   re.form = NULL
  # )
  
  results <- bind_rows(
    comparison |> data.frame() |> select(estimate, conf.low, conf.high, p.value, p.value.noninf), 
    results)
  
  # results.ml <- bind_rows(
  #   ml.comparison |> data.frame() |> select(estimate, conf.low, conf.high, p.value, p.value.noninf), 
  #   results)
  
}

#Calculate Power
type_i_error_rate <- mean(results$p.value.noninf < 0.05)
type_i_error_rate

results |> 
  mutate(sim = row_number()) |> 
  data.frame() |> 
  ggplot(aes(x = sim, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  theme_light() +
  labs(
    x = "Simulations",
    y  = "Difference (LLM - Humans)"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

cbind(results, true_difference_sim) |> 
  mutate(sim = row_number()) |> 
  data.frame() |> 
  ggplot() +
  geom_point(aes(x = sim, y = estimate)) +
  geom_errorbar(aes(x = sim, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_point(aes(y = true_difference_sim, x = sim), color = "blue", size = 2) +
  theme_light() +
  labs(
    x = "Simulations",
    y  = "Difference (LLM - Humans)"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

cbind(results, true_difference_sim) |> 
  mutate(type_i_error = if_else(conf.low > true_difference_sim, 1, 0)) |> 
  pull(type_i_error) |> mean()

results.m |> 
  mutate(sim = row_number()) |> 
  data.frame() |> 
  ggplot(aes(x = sim, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  theme_light() +
  labs(
    x = "Simulations",
    y  = "Difference (LLM - Humans)"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
