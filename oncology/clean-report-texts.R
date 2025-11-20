library(tidyverse)
library(MedOnko)

source("functions/clean_imaging_report_text.R")

data_raw <- read_csv(
  "data/reports_2025-06-13.csv"
)
set.seed(1234)

training_sample <- sample(data_raw$IER_BK, size = 100, replace = FALSE)

# add body region and rename column to match REDCap
data <- data_raw |>
  mutate(
    IER_REPORT = map_chr(.x = IER_REPORT, .f = clean_imaging_report_text),
    training = if_else(IER_BK %in% training_sample, 1, 0)
  ) |>
  add_body_region() |>
  rename(
    ier_bk = IER_BK,
    imaging_report = IER_REPORT,
    imaging_type = IET,
    pet_ct = PET_CT
  ) |>
  select(-c(PAT_BK, IET_DICOM_MODALITY, IMRI_BEGIN_DATE_TS, IET_BK)) |>
  group_by(ier_bk) |>
  mutate(
    redcap_event_name = list(c(
      "extraction_1_arm_1",
      "extraction_2_arm_1",
      "ground_truth_arm_1"
    ))
  ) |>
  unnest(redcap_event_name) |>
  ungroup() |>
  data.frame()

REDCapR::redcap_write(
  data,
  token = Sys.getenv("llm_radiology_project_api"),
  redcap_uri = Sys.getenv("redcap_url_fxdb"),
  overwrite_with_blanks = FALSE
)
