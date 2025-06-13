library(tidyverse)

data <- read_csv(
  "C:/Users/schwenkej/OneDrive - usb.ch/proms-pred/llm-data-extraction/metastasis-response/data/radiology_reports_sample_2025-05-30.csv"
)

set.seed(1234)
# add body region and rename column to match REDCap
data <- data |>
  MedOnko::add_body_region() |>
  rename(
    ier_bk = IER_BK,
    imaging_report = IER_REPORT,
    imaging_type = IET,
    pet_ct = PET_CT
  ) |>
  select(-c(PAT_BK, IET_DICOM_MODALITY, IMRI_BEGIN_DATE_TS, IET_BK)) |>
  slice_sample(n = 20) |> # 20 reports for beta testing of REDCap functionality.
  group_by(ier_bk) |>
  mutate(
    redcap_event_name = list(c("extraction_1_arm_1", "extraction_2_arm_1"))
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
