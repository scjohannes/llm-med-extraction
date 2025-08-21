library(MedOnko)
library(tidyverse)
library(REDCapR)
# Note do not update MedOnko with new prompts / build until I've set a baseline
# for how the prompts before optimizing on the prompt optimization sample

data <- redcap_read(
  redcap_uri = Sys.getenv("redcap_url_fxdb"),
  token = Sys.getenv("llm_radiology_project_api"),
  raw_or_label = "label"
)$data

data <- data |>
  filter(training == "Yes") |>
  filter(redcap_event_name == "Ground Truth")

non_pet <- data |>
  select(ier_bk, imaging_report, body_region, pet_ct) |>
  filter(pet_ct == FALSE)

response_truth <- data |>
  filter(pet_ct == FALSE) |>
  select(ier_bk, response_to_trt_truth = response_to_trt)

ollama_response_non_pet <- non_pet |>
  mutate(
    ollama_response = map(
      .x = imaging_report,
      .f = extract_treatment_response_non_pet,
      model_options = list(temperature = 0, num_ctx = 10000),
      base_url = Sys.getenv("usb_ollama_api_2"),
      .progress = TRUE
    )
  )

mistral_small_response_non_pet <- non_pet |>
  mutate(
    ollama_response = map(
      .x = imaging_report,
      .f = extract_treatment_response_non_pet,
      model = "mistral-small:24b-instruct-2501-q4_K_M",
      model_options = list(temperature = 0, num_ctx = 10000),
      base_url = Sys.getenv("usb_ollama_api_2"),
      .progress = TRUE
    )
  )

deepseek_response_non_pet <-
  non_pet |>
  mutate(
    ollama_response = map(
      .x = imaging_report,
      .f = extract_treatment_response_non_pet,
      model = "deepseek-r1:70b",
      model_options = list(temperature = 0, num_ctx = 10000),
      base_url = Sys.getenv("usb_ollama_api"),
      .progress = TRUE
    )
  )

gpt_oss_response_non_pet <-
  non_pet |>
  mutate(
    ollama_response = map(
      .x = imaging_report,
      .f = extract_treatment_response_non_pet,
      model = "gpt-oss:120b",
      model_options = list(temperature = 0, num_ctx = 10000),
      base_url = Sys.getenv("usb_ollama_api"),
      .progress = TRUE
    )
  )


ollama_results <- ollama_response_non_pet |>
  unnest_wider(ollama_response) |>
  left_join(response_truth, by = "ier_bk") |>
  mutate(
    response_to_trt = tolower(response_to_trt),
    response_to_trt_truth = tolower(gsub(" ", "_", response_to_trt_truth)),
    correct = if_else(response_to_trt == response_to_trt_truth, 1, 0)
  )

mistral_results <- mistral_small_response_non_pet |>
  unnest_wider(ollama_response) |>
  left_join(response_truth, by = "ier_bk") |>
  mutate(
    response_to_trt = tolower(response_to_trt),
    response_to_trt_truth = tolower(gsub(" ", "_", response_to_trt_truth)),
    correct = if_else(response_to_trt == response_to_trt_truth, 1, 0)
  )
