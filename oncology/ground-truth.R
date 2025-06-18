# Johannes Schwenke - 2025-06-17

# Load packages
library(REDCapR)
library(tidyverse)

# Import data

data <- redcap_read(
  redcap_uri = Sys.getenv("redcap_fxdb_url"),
  token = Sys.getenv("llm_radio_api"),
  raw_or_label = "label"
)$data

wide_data <- data |>
  filter(
    redcap_event_name == "Extraction 1" |
      redcap_event_name == "Extraction 2"
  ) |>
  mutate(
    extraction = if_else(redcap_event_name == "Extraction 1", "A", "B")
  ) |>
  group_by(ier_bk) |>
  filter(all(oncology_radiology_extraction_complete == "Complete")) |>
  select(
    -c(
      redcap_event_name,
      extractor_id,
      model,
      prompt,
      justification,
      training,
      sample_complete,
      oncology_radiology_extraction_complete
    )
  ) |>
  pivot_wider(
    id_cols = c(
      ier_bk,
      imaging_report,
      imaging_type,
      body_region,
      pet_ct
    ),
    names_from = extraction,
    values_from = -c(
      ier_bk,
      imaging_report,
      imaging_type,
      body_region,
      pet_ct,
      extraction
    ),
    names_sep = "_" # To create new column names like 'oncology_radiology_extraction_complete_A', etc.
  ) |>
  ungroup()

base_names <- wide_data |>
  names() |>
  grep("_A$", x = _, value = TRUE) |>
  sub("_A$", "", x = _)

ground_truth <- wide_data |>
  reduce(
    base_names, # list to loop over
    .init = _, #place holder for wide_data
    ~ .x |> # accumulated results (take results from previous step and add)
      mutate(
        !!paste0(.y, "_ground_truth") := if_else(
          # current item
          .data[[paste0(.y, "_A")]] == .data[[paste0(.y, "_B")]],
          .data[[paste0(.y, "_A")]],
          "9999"
        )
      )
  )

# write_csv(ground_truth, file = "test.csv")
