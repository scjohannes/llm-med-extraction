# Johannes Schwenke - 2025-06-17

# Load packages
library(REDCapR)
library(redcap)
library(tidyverse)
library(glue)

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

training_indicator <- data |> select(ier_bk, training) |> distinct()


# --- Data where human reviewers agreed on everything

ground_truth_agreement <- ground_truth |>
  filter(if_all(everything(), ~ . != 9999 | is.na(.))) |>
  select(-ends_with("_A"), -ends_with("_B")) |>
  rename_with(~ str_remove(., "_ground_truth$")) |>
  mutate(
    redcap_event_name = "Ground Truth",
    oncology_radiology_extraction_complete = 2,
  ) |>
  left_join(training_indicator, by = "ier_bk") |>
  mutate(across(where(is.character), ~ na_if(., "NA")))

# automatically recode labels to raw
conn <- rconn(
  url = Sys.getenv("redcap_fxdb_url"),
  token = Sys.getenv("llm_radio_api")
)

project_dictionary <- meta_dictionary(conn)

raw_data <- recode_labels(
  x = ground_truth_agreement,
  conn = conn,
  dict = project_dictionary,
  convert_to = "values"
) |>
  mutate(pet_ct = if_else(pet_ct == TRUE, 1, 0))


# we need to split according to body_region and pet_ct, because we can't import
# NA values for some reason, so we have remove columns which are hidden
# depending on pet-ct and body_region

list_of_groups <- raw_data |>
  group_by(body_region, pet_ct) |>
  group_split()

list_to_import <- list_of_groups |>
  map(~ .x |> select(where(~ !all(is.na(.)))))

today <- Sys.Date()

for (i in 1:length(list_to_import)) {
  if (
    all(list_to_import[[i]]$redcap_event_name == "ground_truth_arm_1") == TRUE
  ) {
    REDCapR::redcap_write(
      list_to_import[[i]],
      overwrite_with_blanks = FALSE,
      redcap_uri = Sys.getenv("redcap_fxdb_url"),
      token = Sys.getenv("llm_radio_api")
    )
    write_csv(
      list_to_import[[i]],
      glue("./output/oncology/cons-ground-truth/auto-cons/{today}_{i}.csv")
    )
  } else {
    warning("Data would have overwritten non-ground-ground data. Not uploaded.")
  }
}
