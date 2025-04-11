library(tidyverse)
library(marginaleffects)

# In the first simulation I implemented the acccuracy of each coder is the only
# random varaible. The underlying accuracies of each coder, the llm, and the
# many reports each coder extract is the same in each simulation. Consinder 2nd
# version of simulation for that.

# In a first version of the simulation I actually had Type I error inflation
# when using cluster robus standard errors, because was data was INdependent,
# because I'm stupid and did no assign a difficult parameter for each report.
# This messed up the clustering formula.

# Null Hypthesis is true, LLM is actually slightly worse than humans.

set.seed(2)

llm_skill   <- 0.95 # Just more than 5% worse. How many times wouuld we say that llm is not more than 5% worse, if it actually is?

results <- data.frame()
results.ml <- data.frame()
true_difference_sim <- numeric()

n_human_coders_per_report <- 2 # Number of humans coding each report

sd_difficulty <- 1 # Standard deviation for report difficulty (logit scale)
correlation_difficulty <- -0.2 # Correlation between human and LLM difficulty

# NOTE: As the inherent human skill is very very close to one (see blow),
# increasing the SD of the difficulty (this is on the logit scale), will cause
# the overall accuracy to go down on the probability scale. Because large
# positive values of the difficulty will only increase the accuracy on the
# probability scale from e.g., 0.95 to 0.98, but large negative values will
# decrease it from 0.95 to much lower values. It's linear on the logit scale,
# but not linear on the probability scale.


n_human_coders_total <- 6 

# human skill and llm skill refer to the inate ability of the human / llm
# extractor this will be combined with the difficulty of the report to come up
# with a probability of a correct answer. From this bernoulli distribution will
# we will randomly draw in each iteration of the simulation

human_skill <- c(0.95, 0.95, 0.94, 0.93, 0.96, 0.97) #arithmetric mean = 0.95

non_inferiority_margin <- 0.05
equivalence_bounds <- c(-non_inferiority_margin, Inf) # Test H1: LLM - Human > -0.05
n_reports <- 3000

human_coder_ids <- paste0("human_", 1:n_human_coders_total)
report_ids <- paste0("report_", 1:n_reports)
llm_id <- "llm"

# --- Pre-calculate Lookups ---
extractor_lookup <- tibble(
  extractor_id = c(human_coder_ids, llm_id),
  skill = c(human_skill, llm_skill),
  logit_skill = qlogis(skill),
  is_llm = c(rep(0, n_human_coders_total), 1)
)

# --- Generate FIXED Report Difficulties (Outside Loop) ---

message("Generating fixed report difficulties...")
report_difficulty_lookup <- tibble(
  report_id = report_ids,
  report_difficulty_logit = rnorm(n_reports, 0, sd_difficulty)
) |> 
  
  # I'm adding a slight negative correlation between reports for humans and llm,
  # i.e., reports that are hard for llm are easier for humans and vice versa
  mutate(llm_difficulty_logit = 
           report_difficulty_logit*-correlation_difficulty + 
           rnorm(n_reports, 0, sd_difficulty))

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
    logit_p_human = logit_skill + report_difficulty_logit,
    logit_llm_skill = logit_skill + llm_difficulty_logit,
    p_human = plogis(logit_p_human),
    p_llm = plogis(logit_llm_skill),
    p_actual = if_else(is_llm == 0, p_human, p_llm)
  )

# --- Calculate the SINGLE FIXED True Average Difference (Outside Loop) ---

avg_p_actual_llm_fixed <- mean(sim_setup_data$p_llm[sim_setup_data$is_llm == 1])
message(paste0("The average llm accuracy is ", round(avg_p_actual_llm_fixed, digits = 3), " with an SD of ", round(sd(sim_setup_data$p_llm[sim_setup_data$is_llm == 1]), digits = 3)))

avg_p_actual_human_fixed <- mean(sim_setup_data$p_human[sim_setup_data$is_llm == 0])
message(paste0("The (weighted) average human accuracy is ", round(avg_p_actual_human_fixed, digits = 3), " with an SD of ", round(sd(sim_setup_data$p_human[sim_setup_data$is_llm == 0]), digits = 3)))

true_difference_fixed <- avg_p_actual_llm_fixed - avg_p_actual_human_fixed
message(paste("Fixed True Average Difference (LLM - Human):", round(true_difference_fixed, 4)))

# Not exactly the values specified above, because not all humans extract same proportion of reports. 

sim_data_list <- vector("list", n_reports)

results <- data.frame()
results_m <- data.frame()

for(i in 1:200){
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
  # model_m_fit <- glmer(correct ~ is_llm + (is_llm|report_id), data = sim_data_long, family = binomial(link = "logit"))
  
  # Calculate difference using marginal effects. Uses delta method and cluster robust standard errors.
  # NOTE: I'm setting the width of the confidence interval to 0.9 so the lower bound 'corresponds' to the one sided alpha of 0.05
  comparison <- avg_comparisons(
    model_fit,
    variables = "is_llm",
    type = "response",
    comparison = "difference",
    vcov = ~report_id,
    conf_level = 0.9,
    equivalence = equivalence_bounds # this calculates the non-inferioty p-value (see bound set at the top)
  )
  
  results <- bind_rows(
    comparison |> data.frame() |> select(estimate, conf.low, conf.high, p.value, p.value.noninf), 
    results)
}

# Calculate Type I error rate / power if the llm is as good as the human
type_i_error_rate <- mean(results$p.value.noninf < 0.05)
type_i_error_rate

# Not sure why the two don't match
cbind(results, true_difference_sim) |> 
  mutate(type_i_error = if_else(conf.low > -0.05, 1, 0)) |> 
  pull(type_i_error) |> mean()

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
