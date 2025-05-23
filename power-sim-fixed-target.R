library(tidyverse)

# --- Simulation Parameters ---
confidence <- 0.95
max_sample_size <- 2000 # Set a general max_n for simulations
n_sims <- 4000

# --- Scenario 1: Iterate over Target Lower Bounds (Fixed True Accuracy) ---
fixed_true_acc <- seq(0.91, 0.99, by = 0.01)
target_lb_range <- seq(0.90, 0.94, by = 0.02) # Range of target lower bounds

# Create the parameter grid
parameter_grid <- crossing(
  true_accuracy = fixed_true_acc,
  target_lower_bound = target_lb_range,
  n_participants = seq(200,1000, by = 20) # Renamed 'n' to 'n_trials' for clarity
) |>
  # Filter based on the condition
  filter(true_accuracy > target_lower_bound)


calculate_lower_bound <- function(n_success, n_participants){
  prop.test(n_success, n_participants, alternative = "greater", conf.level = 0.95)$conf.int[1]
}

set.seed(1234)

results <- parameter_grid |>
  # Expand the grid: create n_replications copies of each row, adding an ID
  uncount(n_sims, .id = "sim_id") |> 
  mutate(
    n_success = rbinom(n(), size = n_participants, prob = true_accuracy)) |> 
  mutate(conf_lower = map2_dbl(
    .x = n_success,
    .y = n_participants,
    ~calculate_lower_bound(
      n_success = .x, n_participants = .y
    ),
    .progress = TRUE
  ))

power_df <- results |> 
  mutate(
    above = if_else(conf_lower > target_lower_bound, 1, 0)
    ) |> 
  group_by(true_accuracy, target_lower_bound, n_participants) |> 
  summarise(
    power = mean(above),
  ) |> 
  ungroup()

power_df |> 
  mutate(target_lower_bound = factor(target_lower_bound)) |> 
  ggplot(aes(x = n_participants, y = power, color = target_lower_bound)) +
  geom_line() +
  facet_wrap(~ true_accuracy)
