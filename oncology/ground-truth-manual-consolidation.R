# Johannes Schwenke - 2025-06-17

# Load packages
library(REDCapR)
library(redcap)
library(tidyverse)
library(glue)

# --- Section for consolidated data by third reviewer ---
csv_files <- fs::dir_ls(
  path = "./data/oncology/manually-consolidated/",
  glob = "*.csv",
  recurse = TRUE
)

consolidated_labels <- map_dfr(csv_files, read_csv) |>
  select(-ends_with("_A"), -ends_with("_B")) |>
  rename_with(~ str_remove(., "_ground_truth$")) |>
  mutate(
    redcap_event_name = "Ground Truth",
    oncology_radiology_extraction_complete = 2,
  ) |>
  #dropping columns which are already completed in redcap
  mutate(response_to_trt_pet = as.character(response_to_trt_pet)) |>
  select(-imaging_report, -imaging_type, -other_organ_metastasis)

conn <- rconn(
  url = Sys.getenv("redcap_fxdb_url"),
  token = Sys.getenv("llm_radio_api")
)

project_dictionary <- meta_dictionary(conn)

consolidated_raw <- recode_labels(
  x = consolidated_labels,
  conn = conn,
  dict = project_dictionary,
  convert_to = "values"
) |>
  mutate(pet_ct = if_else(pet_ct == TRUE, 1, 0))

# we need to split according to body_region and pet_ct, because we can't import
# NA values for some reason

list_of_groups <- consolidated_raw |>
  group_by(body_region, pet_ct) |>
  group_split()

list_to_import <- list_of_groups |>
  map(~ .x |> select(where(~ !all(is.na(.)))))

today <- Sys.Date()

# Check that we only upload to ground truth arm

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
      glue("./output/oncology/cons-ground-truth/man-cons/{today}_{i}.csv")
    )
  } else {
    warning("Data would have overwritten non-ground-ground data. Not uploaded.")
  }
}
