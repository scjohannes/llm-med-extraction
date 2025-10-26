library(MedOnko)
library(tidyverse)
library(REDCapR)
library(ellmer)
# Note do not update MedOnko with new prompts / build until I've set a baseline
# for how the prompts before optimizing on the prompt optimization sample

data <- redcap_read(
  redcap_uri = Sys.getenv("redcap_url_fxdb"),
  token = Sys.getenv("llm_radiology_project_api"),
  raw_or_label = "label"
)$data

data <- data |>
  filter(training == "No", redcap_event_name == "Ground Truth")

models <- c(
  # "llama3.3:70b-instruct-q5_K_M",
  # "mistral-small:24b-instruct-2501-q4_K_M",
  # "qwen3:32b",
  # "qwq:32b",
  "gpt-oss:120b"
)
base_url <- Sys.getenv("usb_ollama_api_h200")
api_key <- Sys.getenv("OLLAMA_API_KEY_H200")
redcap_event_name <- c(
  # "llm_1_arm_1",
  # "llm_2_arm_1",
  # "llm_3_arm_1",
  # "llm_4_arm_1",
  "llm_5_arm_1"
)

extract_metastasis <- TRUE
extract_reponse <- FALSE
extract_diagnosis <- FALSE


# Logging setup
duration_log_path <- file.path(
  "output",
  "oncology",
  "llm-results",
  "model_iteration_durations.csv"
)
dir.create(dirname(duration_log_path), recursive = TRUE, showWarnings = FALSE)
if (!file.exists(duration_log_path)) {
  readr::write_csv(
    tibble(
      timestamp = as.POSIXct(character()),
      model = character(),
      event = character(),
      start_time = as.POSIXct(character()),
      end_time = as.POSIXct(character()),
      duration_secs = double(),
      status = character(),
      error_message = character()
    ),
    duration_log_path
  )
}

# trt response -----------------------------------------------------
for (i in 1:length(models)) {
  model_i <- models[i]
  event_i <- redcap_event_name[i]

  message(sprintf(
    "==> Modell %d/%d: %s  ->  Event %s",
    i,
    length(models),
    model_i,
    event_i
  ))

  start_time <- Sys.time()
  status <- "success"
  error_message <- NA_character_

  tryCatch(
    {
      if (extract_reponse) {
        llm_response_trt <- data |>
          mutate(
            llm_extraction = map2(
              .x = imaging_report,
              .y = pet_ct,
              .f = ~ response_to_treatment_pipeline(
                text = .x,
                pet_ct = .y,
                model = model_i,
                model_options = list(temperature = 0, num_ctx = 10000),
                base_url = base_url,
                api_key = api_key
              ),
              .progress = TRUE
            )
          )

        llm_response_trt_formatted <- llm_response_trt |>
          select(ier_bk, imaging_report, pet_ct, llm_extraction) |>
          unnest_wider(llm_extraction)

        if (!"response_to_trt" %in% names(llm_response_trt_formatted)) {
          llm_response_trt_formatted$response_to_trt <- NA_character_
        }
        if (!"response_to_trt_pet" %in% names(llm_response_trt_formatted)) {
          llm_response_trt_formatted$response_to_trt_pet <- NA_character_
        }

        llm_response_trt_formatted <- llm_response_trt_formatted |>
          mutate(
            response_to_trt = case_when(
              response_to_trt == "PROGRESSION" ~ 1,
              response_to_trt == "STABLE_DISEASE" ~ 2,
              response_to_trt == "RESPONSE" ~ 3,
              response_to_trt == "NOT_APPLICABLE" ~ 4,
              TRUE ~ NA_real_
            ),
            response_to_trt_pet = case_when(
              response_to_trt_pet == "PROGRESSION" ~ 1,
              response_to_trt_pet == "STABLE_DISEASE" ~ 2,
              response_to_trt_pet == "PARTIAL_RESPONSE" ~ 3,
              response_to_trt_pet == "COMPLETE_RESPONSE" ~ 4,
              response_to_trt_pet == "NOT_APPLICABLE" ~ 5,
              TRUE ~ NA_real_
            ),
            redcap_event_name = event_i
          ) |>
          select(-path, -imaging_report) |>
          mutate(
            pet_ct = if_else(pet_ct, 1, 0),
            no_tumor = if_else(no_tumor, 1, 0)
          )

        if (all(llm_response_trt_formatted$redcap_event_name == event_i)) {
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
          apply_metastasis_extraction(
            model = model_i,
            base_url = base_url,
            api_key = api_key
          )

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
            redcap_event_name = event_i
          ) |>
          relocate(ier_bk)

        list_of_groups <- metastasis_for_redcap |>
          group_by(body_region) |>
          group_split()

        list_to_import <- list_of_groups |>
          map(~ .x |> select(where(~ !all(is.na(.)))))

        for (j in 1:length(list_to_import)) {
          if (all(list_to_import[[j]]$redcap_event_name == event_i)) {
            REDCapR::redcap_write(
              list_to_import[[j]],
              overwrite_with_blanks = FALSE,
              redcap_uri = Sys.getenv("redcap_url_fxdb"),
              token = Sys.getenv("llm_radiology_project_api")
            )
          } else {
            warning(
              "Data would have overwritten non-ground-truth data. Not uploaded."
            )
          }
        }
      }

      # Diagnosis -------------------------------------------------------------------
      if (extract_diagnosis) {
        diagnosis_data <- data |>
          select(ier_bk, imaging_report, imaging_type)

        llm_diagnosis_data <- diagnosis_data |>
          mutate(
            llm_extraction = map(
              .x = imaging_report,
              .f = ~ extract_primary_location(
                text = .x,
                model = model_i,
                model_options = list(temperature = 0, num_ctx = 10000),
                base_url = base_url,
                api_key = api_key
              ),
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
                "Cancer of the Urinary Tract (including Ureter and Bladder)" ~ 14,
              primary_tumor == "Prostate Cancer" ~ 15,
              primary_tumor == "Gynecological Tumors" ~ 16,
              primary_tumor ==
                "Primary Skin Tumor (including Melanoma and Non-Melanoma)" ~ 17,
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
              TRUE ~ NA_real_
            )
          ) |>
          mutate(
            redcap_event_name = event_i,
            oncology_radiology_extraction_complete = 2
          )

        if (all(llm_diagnosis_data$redcap_event_name == event_i)) {
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
    },
    error = function(e) {
      status <<- "error"
      error_message <<- conditionMessage(e)
    },
    finally = {
      end_time <- Sys.time()
      log_row <- tibble(
        timestamp = Sys.time(),
        model = model_i,
        event = event_i,
        start_time = start_time,
        end_time = end_time,
        duration_secs = as.numeric(difftime(
          end_time,
          start_time,
          units = "secs"
        )),
        status = status,
        error_message = error_message
      )
      readr::write_csv(log_row, duration_log_path, append = TRUE)
      message(sprintf(
        "   Finished %s in %.1f sec (status: %s)",
        model_i,
        log_row$duration_secs,
        status
      ))
    }
  )
}
