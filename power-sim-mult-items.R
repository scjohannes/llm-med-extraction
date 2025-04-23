library(tidyverse)
library(marginaleffects)

set.seed(122)

n_sims <- 300
llm_skill <- 0.899 # Use 0.899 to test type 1 error control # User 0.95 for power

results <- data.frame()
results.ml <- data.frame()
true_difference_sim <- numeric()

n_human_coders_per_report <- 2 # Number of humans coding each report

# Standard deviation for report difficulty (logit scale). Set to 0.5 to
# reproduce 90% power, Set to 1.5 to reproduce 70% power in the setting where we
# extract only one item per report, or 99% / 95% if 3 or more items per report.
sd_difficulty <- 0.5
correlation_difficulty <- -0.2 # Correlation between human and LLM difficulty

# NOTE: As the inherent human skill is very very close to one (see blow),
# increasing the SD of the difficulty (this is on the logit scale), will cause
# the overall accuracy to go down on the probability scale. Because large
# positive values of the difficulty will only increase the accuracy on the
# probability scale from e.g., 0.95 to 0.98, but large negative values will
# decrease it from 0.95 to much lower values. I.e., it's linear on the logit scale,
# but not linear on the probability scale.

n_human_coders_total <- 6

# Human skill and llm skill refer to the inate ability of the human / llm
# extractor this will be combined with the difficulty of the report to come up
# with a probability of a correct answer. From this bernoulli distribution will
# we will randomly draw in each iteration of the simulation. If sd_difficulty
# approaches 0, accuracy approaches skill.

human_skill <- c(0.95, 0.95, 0.94, 0.93, 0.96, 0.97) #arithmetric mean = 0.95

# How many items are extracted per report. We compare accuracy for each item
# (but take into consideration that they are clustered inside a report)
n_items_per_report <- 3

non_inferiority_margin <- 0.05
equivalence_bounds <- c(-non_inferiority_margin, Inf) # Test H1: LLM - Human > -0.05
n_reports <- 300

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

report_difficulty_lookup <- tibble(
  report_id = report_ids,
  report_difficulty_logit = rnorm(n_reports, 0, sd_difficulty)
) |>

  # I'm adding a slight negative correlation between reports for humans and llm,
  # i.e., reports that are hard for llm are easier for humans and vice versa
  mutate(
    llm_difficulty_logit = report_difficulty_logit *
      -correlation_difficulty +
      rnorm(n_reports, 0, sd_difficulty)
  )

# --- Generate FIXED Assignments for Reports (Deterministic Block Assignment per Group) ---
# --- Parameter Checks for this specific strategy ---
if (n_human_coders_total %% 2 != 0) {
  stop(
    "This assignment strategy requires an even number of total human coders."
  )
}
n_coders_per_group <- n_human_coders_total / 2
if (n_reports %% n_coders_per_group != 0) {
  stop(
    "This assignment strategy requires n_reports to be divisible by (n_human_coders_total / 2)."
  )
}

# --- Define Groups and Report Blocks ---
group_A_coders <- human_coder_ids[1:n_coders_per_group]
group_B_coders <- human_coder_ids[(n_coders_per_group + 1):n_human_coders_total]

reports_per_coder <- n_reports / n_coders_per_group # 300 / 3 = 100

# --- Function to generate assignments for one group ---
generate_group_block_assignments <- function(
  coder_group,
  all_report_ids,
  reports_per_coder_count
) {
  assignments_list <- vector("list", length(coder_group))
  report_start_index <- 1

  for (i in 1:length(coder_group)) {
    current_coder <- coder_group[i]
    report_end_index <- report_start_index + reports_per_coder_count - 1
    # Ensure we don't exceed the total number of reports
    report_end_index <- min(report_end_index, length(all_report_ids))
    current_report_ids <- all_report_ids[report_start_index:report_end_index]

    # Assign this block of reports to the current coder
    assignments_list[[i]] <- tibble(
      report_id = current_report_ids,
      extractor_id = current_coder
    )

    report_start_index <- report_end_index + 1
  }
  bind_rows(assignments_list)
}

# --- Generate Assignments for Both Groups ---
assignments_A <- generate_group_block_assignments(
  group_A_coders,
  report_ids,
  reports_per_coder
) |>
  mutate(extractor_id = sample(extractor_id))
assignments_B <- generate_group_block_assignments(
  group_B_coders,
  report_ids,
  reports_per_coder
) |>
  mutate(extractor_id = sample(extractor_id))

# Combine human assignments from both groups
human_assignments_long <- bind_rows(assignments_A, assignments_B)

# Create assignments for the LLM (codes all reports)
llm_assignments <- tibble(
  report_id = report_ids,
  extractor_id = llm_id
)

# Combine human and LLM assignments
all_assignments <- bind_rows(human_assignments_long, llm_assignments)

# Join with extractor lookup to get skill info
sim_data_assigned <- all_assignments |>
  left_join(extractor_lookup, by = "extractor_id") |>
  arrange(report_id, extractor_id) # Optional: arrange for easier viewing

# --- Calculate FIXED Actual Probabilities (Outside Loop) ---

# This integrates the report difficult and the accuracy of each extractor
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

sim_setup_data <- sim_setup_data |>
  uncount(n_items_per_report)

# --- Calculate the SINGLE FIXED True Average Difference (Outside Loop) ---

avg_p_actual_llm_fixed <- mean(sim_setup_data$p_llm[sim_setup_data$is_llm == 1])
message(paste0(
  "The average llm accuracy is ",
  round(avg_p_actual_llm_fixed, digits = 3),
  " with an SD of ",
  round(sd(sim_setup_data$p_llm[sim_setup_data$is_llm == 1]), digits = 3)
))

avg_p_actual_human_fixed <- mean(sim_setup_data$p_human[
  sim_setup_data$is_llm == 0
])
message(paste0(
  "The (weighted) average human accuracy is ",
  round(avg_p_actual_human_fixed, digits = 3),
  " with an SD of ",
  round(sd(sim_setup_data$p_human[sim_setup_data$is_llm == 0]), digits = 3)
))

true_difference_fixed <- avg_p_actual_llm_fixed - avg_p_actual_human_fixed
message(paste(
  "Fixed True Average Difference (LLM - Human):",
  round(true_difference_fixed, 4),
  ". In small sample sizes with large difficult SD on the logit scale, sampling error is large."
))

sim_data_list <- vector("list", n_reports)

results <- data.frame()
results_m <- data.frame()

for (i in 1:n_sims) {
  message(paste0("Simulation ", i))

  sim_data_long <- sim_setup_data |>
    mutate(
      correct = rbinom(n(), 1, p_actual)
    )

  human_data_sim <- sim_data_long |> filter(is_llm == 0)
  true_mean_human_accuracy_sim <- sum(human_data_sim$p_actual) /
    nrow(human_data_sim)
  true_llm_accuracy <- sim_data_long |>
    filter(is_llm == 1) |>
    pull(p_actual) |>
    mean()

  true_difference_sim[i] <- true_llm_accuracy - true_mean_human_accuracy_sim

  # Fit Simple GLM Model (ignoring clustering structure here)
  model_fit <- glm(
    correct ~ is_llm,
    data = sim_data_long,
    family = binomial(link = "logit")
  )

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
    comparison |>
      data.frame() |>
      select(estimate, conf.low, conf.high, p.value, p.value.noninf),
    results
  )
}

# Calculate Type I error rate / power if the llm is as good as the human
type_i_error_rate <- mean(results$p.value.noninf < 0.05)
type_i_error_rate

cbind(results, true_difference_sim) |>
  mutate(type_i_error = if_else(conf.low > -0.05, 1, 0)) |>
  pull(type_i_error) |>
  mean()

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
    y = "Difference (LLM - Humans)"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
