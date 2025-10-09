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
  filter(training == "Yes", redcap_event_name == "Ground Truth") |>
  dplyr::slice_head(n = 5)  

models <- c("llama3.3:70b-instruct-q5_K_M", "mistral-small:24b-instruct-2501-q4_K_M", "rndcalcle01.qwen3:32b", "rndcalcle01.qwq:32b") 
base_url <- Sys.getenv("usb_ollama_api_2")
redcap_event_name <- c("llm_1_arm_1", "llm_2_arm_1", "llm_3_arm_1", "llm_4_arm_1")

extract_metastasis <- FALSE
extract_reponse <- TRUE
extract_diagnosis <- TRUE


# trt response -----------------------------------------------------
for (i in 1:length(models))  {
  model_i  <- models[i]
  event_i  <- redcap_event_name[i]
  
  message(sprintf("==> Modell %d/%d: %s  ->  Event %s",
                  i, length(models), model_i, event_i))
  
if (extract_reponse) {
  llm_response_trt <- data |>
    mutate(
      llm_extraction = map2(
        .x = imaging_report,
        .y = pet_ct,
        .f = ~ response_to_treatment_pipeline(
          model_options = list(temperature = 0, num_ctx = 10000),
          base_url = base_url,
          model = model_i,
          text = .x,
          pet_ct = .y
        ),
        .progress = TRUE
      )
    )

  llm_response_trt_formatted <- llm_response_trt |>
    select(ier_bk, imaging_report, pet_ct, llm_extraction) |>
    unnest_wider(llm_extraction) |>
    mutate(
      response_to_trt = case_when(
        response_to_trt == "PROGRESSION" ~ 1,
        response_to_trt == "STABLE_DISEASE" ~ 2,
        response_to_trt == "RESPONSE" ~ 3,
        response_to_trt == "NOT_APPLICABLE" ~ 4
      ),
      #response_to_trt_pet = case_when(
      #  response_to_trt_pet == "PROGRESSION" ~ 1,
      #  response_to_trt_pet == "STABLE_DISEASE" ~ 2,
      #  response_to_trt_pet == "PARTIAL_RESPONSE" ~ 3,
      #  response_to_trt_pet == "COMPLETE_RESPONSE" ~ 4,
      #  response_to_trt_pet == "NOT_APPLICABLE" ~ 5
      #),
      redcap_event_name = event_i
    ) |> 
    select(-path, -imaging_report) |> 
    mutate(pet_ct = if_else(pet_ct, 1, 0))

  # Check that we only upload to desired arm
  if (
    all(llm_response_trt_formatted$redcap_event_name == event_i
    ) ==
      TRUE
  ) {
    REDCapR::redcap_write(
      llm_response_trt_formatted,
      overwrite_with_blanks = FALSE,
      redcap_uri = Sys.getenv("redcap_url_fxdb"),
      token = Sys.getenv("llm_radiology_project_api")
    )
  } else {
    warning(
      "Data would have overwritten data from other redcap event. Not uploaded."
    )
  }
}

# Metastasis ----------------------------------------------------------
if (extract_metastasis) {
  metastasis_data <- data |>
    select(ier_bk, imaging_report, body_region) |>
    apply_metastasis_extraction(base_url = base_url, model = model)

  metastasis_for_redcap <- metastasis_data |>
    mutate(
      across(
        ends_with("metastasis"),
        ~ case_when(
          . == TRUE ~ 1,
          . == FALSE ~ 0,
          is.na(.) ~ NA_real_
        )
      ),
      redcap_event_name = redcap_event_name
    ) |>
    relocate(ier_bk)

  ## prepare for upload to redcap
  list_of_groups <- metastasis_for_redcap |>
    group_by(body_region) |>
    group_split()

  list_to_import <- list_of_groups |>
    map(~ .x |> select(where(~ !all(is.na(.)))))

  # Check that we only upload to llm_1_arm_1

  for (i in 1:length(list_to_import)) {
    if (
      all(list_to_import[[i]]$redcap_event_name == redcap_event_name) == TRUE
    ) {
      REDCapR::redcap_write(
        list_to_import[[i]],
        overwrite_with_blanks = FALSE,
        redcap_uri = Sys.getenv("redcap_url_fxdb"),
        token = Sys.getenv("llm_radiology_project_api")
      )
    } else {
      warning(
        "Data would have overwritten non-ground-ground data. Not uploaded."
      )
    }
  }
}

# Diagnosis -------------------------------------------------------------------
if (extract_diagnosis) {
  diagnosis_data <- data |>
    select(ier_bk, imaging_report, imaging_type) |> 
    head(n = 2)

  llm_diagnosis_data <- diagnosis_data |>
    mutate(
      llm_extraction = map(
        .x = imaging_report,
        .f = extract_primary_location,
        model = model_i,
        model_options = list(temperature = 0, num_ctx = 10000),
        base_url = base_url,
        .progress = TRUE
      )
    )

  llm_diagnosis_data <- llm_diagnosis_data |>
    unnest_wider(llm_extraction) |>
    mutate(
      primary_tumor = case_when(
        primary_tumor == "Primary Brain Tumor" ~ 1,
        primary_tumor == "Head and Neck Tumors" ~ 2,
        primary_tumor == "Thyroid Cancer" ~ 3,
        primary_tumor == "Lung Tumor (including Mesothelioma)" ~ 4,
        primary_tumor == "Breast Cancer" ~ 5,
        primary_tumor == "Gastric Cancer" ~ 6,
        primary_tumor == "Esophageal Cancer" ~ 7,
        primary_tumor == "Pancreatic Cancer" ~ 8,
        primary_tumor == "Cholangiocarcinoma" ~ 9,
        primary_tumor == "Hepatocellular Carcinoma" ~ 10,
        primary_tumor == "Small Bowel Cancer" ~ 11,
        primary_tumor == "Colorectal Cancer" ~ 12,
        primary_tumor == "Kidney Tumor" ~ 13,
        primary_tumor ==
          "Cancer of the Urinary Tract (including Ureter and Bladder)" ~
          14,
        primary_tumor == "Prostate Cancer" ~ 15,
        primary_tumor == "Gynecological Tumors" ~ 16,
        primary_tumor ==
          "Primary Skin Tumor (including Melanoma and Non-Melanoma)" ~
          17,
        primary_tumor == "Sarcoma" ~ 18,
        primary_tumor == "Lymphoma" ~ 19,
        primary_tumor == "Multiple Myeloma" ~ 20,
        primary_tumor == "Leukemia" ~ 21,
        primary_tumor == "Neuroendocrine Tumors" ~ 22,
        primary_tumor == "Testicular Cancer" ~ 23,
        primary_tumor == "Cancer of Unkown Primary" ~ 24,
        primary_tumor == "Other" ~ 25,
        primary_tumor == "Unclear" ~ 26,
        primary_tumor == "No Malignant Disease" ~ 27,
        primary_tumor == "Not Applicable" ~ 28,
        TRUE ~ NA_real_ # Handles any cases not explicitly matched
      )
    ) |>
    mutate(
      redcap_event_name = event_i,
      oncology_radiology_extraction_complete = 2
    )

  if (
    all(
      llm_diagnosis_data$redcap_event_name == event_i
    ) ==
      TRUE
  ) {
    REDCapR::redcap_write(
      llm_diagnosis_data,
      overwrite_with_blanks = FALSE,
      redcap_uri = Sys.getenv("redcap_url_fxdb"),
      token = Sys.getenv("llm_radiology_project_api")
    )
  } else {
    warning(
      "Data would have overwritten data from other redcap event. Not uploaded."
    )
  }
}
}
