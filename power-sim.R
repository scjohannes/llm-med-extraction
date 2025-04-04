library(tidyverse)
library(marginaleffects)
library(lme4)

# In the first simulation I implemented the acccuracy of each coder is the only
# random varaible. The underlying accuracies of each coder, the llm, and hoe
# many reports each coder extract is the same in each simulation. Consinder 2nd
# version of simulation for that.

# In a first version of the simulation I actually had Type I error inflation
# when using cluster robus standard errors, because was data was INdependent,
# because I'm stupid and did no assign a difficult parameter for each report.
# This messed up the clustering formula.

# TODO: Would like to make performance for LLM and humans different for each
# report (baiscally a random slope). Separate, but slightly negatively
# correlated difficulty for humans and llm each.


# Null Hypthesis is true, LLM is actually slightly worse than humans.

set.seed(2024)

p_llm   <- 0.899 # Just more than 5% worse. How many times wouuld we say that llm is not more than 5% worse, if it actually is?

results <- data.frame()
results.ml <- data.frame()
true_difference_sim <- numeric()

n_human_coders_per_report <- 2 # Number of humans coding each report

sd_difficulty <- 0.5 # Standard deviation for report difficulty (logit scale)

n_human_coders_total <- 6 
human_accuracies <- c(0.95, 0.95, 0.94, 0.93, 0.96, 0.97) #arithmetric mean = 0.95

non_inferiority_margin <- 0.05
equivalence_bounds <- c(-non_inferiority_margin, Inf) # Test H1: LLM - Human > -0.05
alpha <- 0.05
n_reports <- 500

human_coder_ids <- paste0("human_", 1:n_human_coders_total)
report_ids <- paste0("report_", 1:n_reports)
llm_id <- "llm"

# --- Pre-calculate Lookups ---
extractor_lookup <- tibble(
  extractor_id = c(human_coder_ids, llm_id),
  accuracy_p_baseline = c(human_accuracies, p_llm),
  logit_accuracy_baseline = qlogis(accuracy_p_baseline),
  is_llm = c(rep(0, n_human_coders_total), 1)
)

# --- Generate FIXED Report Difficulties (Outside Loop) ---
message("Generating fixed report difficulties...")
report_difficulty_lookup <- tibble(
  report_id = report_ids,
  report_difficulty_logit = rnorm(n_reports, 0, sd_difficulty)
)

# --- Generate FIXED Assignments for Reports (Outside Loop) ---
message("Generating fixed extractor assignments...")
sim_data_list_assigned <- vector("list", n_reports)
for (j in 1:n_reports) {
  selected_human_ids <- sample(human_coder_ids, n_human_coders_per_report, replace = FALSE)
  report_extractors <- c(selected_human_ids, llm_id)
  report_extractor_info <- extractor_lookup |>
    filter(extractor_id %in% report_extractors) |>
    mutate(report_id = report_ids[j])
  sim_data_list_assigned[[j]] <- report_extractor_info
}
sim_data_assigned <- bind_rows(sim_data_list_assigned)

# --- Calculate FIXED Actual Probabilities (Outside Loop) ---
# This integrates the report difficult and the accuracy of each extractor
message("Calculating fixed actual probabilities...")
sim_setup_data <- sim_data_assigned |>
  left_join(report_difficulty_lookup, by = "report_id") |>
  mutate(
    # Calculate actual probability for this extractor on this report
    logit_p_actual = logit_accuracy_baseline + report_difficulty_logit,
    p_actual = plogis(logit_p_actual)
  )

# --- Calculate the SINGLE FIXED True Average Difference (Outside Loop) ---
message("Calculating the single true average difference...")
avg_p_actual_llm_fixed <- mean(sim_setup_data$p_actual[sim_setup_data$is_llm == 1])
avg_p_actual_human_fixed <- mean(sim_setup_data$p_actual[sim_setup_data$is_llm == 0])
true_difference_fixed <- avg_p_actual_llm_fixed - avg_p_actual_human_fixed

message(paste("Fixed True Average Difference (LLM - Human):", round(true_difference_fixed, 4)))
# Not exactly 0.5, because not all humans extract same proportion of reports. 

sim_data_list <- vector("list", n_reports)

results <- data.frame()

for(i in 1:500){
  message(paste0("Simulation ", i))
  
  sim_data_long <- sim_setup_data |> 
    mutate(
      correct = rbinom(n(), 1, p_actual)
    )
  
  human_data_sim <- sim_data_long |> filter(is_llm == 0)
  true_mean_human_accuracy_sim <- sum(human_data_sim$p_actual) / nrow(human_data_sim)
  true_llm_accuracy <- sim_data_long |> filter(is_llm == 1)  |> 
    pull(p_actual) |> mean()
  
  true_difference_sim[i] <- true_llm_accuracy - true_mean_human_accuracy_sim
  
  
  # Fit Simple GLM Model (ignoring clustering structure here)
  model_fit <- glm(correct ~ is_llm, data = sim_data_long, family = binomial(link = "logit"))
  
  # Calculate difference using marginal effects. Uses delta method and cluster robust standard errors.
  comparison <- avg_comparisons(
    model_fit,
    variables = "is_llm",
    type = "response",
    comparison = "difference",
    vcov = ~report_id,
    #df = n_human_coders_total, # not -1 because llm is also cluster
    equivalence = equivalence_bounds
  )
  
  results <- bind_rows(
    comparison |> data.frame() |> select(estimate, conf.low, conf.high, p.value, p.value.noninf), 
    results)
}

# Calculate Type I error rate
type_i_error_rate <- mean(results$p.value.noninf < 0.05)
type_i_error_rate

# Not sure why the two don't match
cbind(results, true_difference_sim) |> 
  mutate(type_i_error = if_else(conf.low > true_difference_sim, 1, 0)) |> 
  pull(type_i_error) |> mean()

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
  geom_hline(yintercept = -non_inferiority_margin, color = "red") + #non-inferiority margin
  geom_point(aes(y = true_difference_sim, x = sim), color = "blue", size = 2) + #true difference
  theme_light() +
  labs(
    x = "Simulations",
    y  = "Difference (LLM - Humans)"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )













### IGNORE FOR THE TIME BEING #########


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

for(i in 1:50){
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
    df = 10 - 1,
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
  geom_point(aes(y = true_difference_sim, x = sim), color = "blue", size = 2) + #true difference
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

# results.m |> 
#   mutate(sim = row_number()) |> 
#   data.frame() |> 
#   ggplot(aes(x = sim, y = estimate)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
#   theme_light() +
#   labs(
#     x = "Simulations",
#     y  = "Difference (LLM - Humans)"
#   ) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank()
#   )


### Test if it works better if I fix the accuracies of the participants to a
### certain value and the proporion of reports they work on. Let's look at equal
### proportions first.

