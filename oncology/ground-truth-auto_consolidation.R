# Johannes Schwenke - 2025-06-17

# Load packages
library(REDCapR)
library(redcap)
library(tidyverse)
library(glue)
library(readr)
library(fs)

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


# Export data where human reviewers did not aggree
unconsolidated <- data |>
  filter(!(ier_bk %in% raw_data$ier_bk))

# Get list of already exported ier_bks and determine next chunk number
uncons_output_dir <- "./output/oncology/uncons-ground-truth"
existing_ier_bks <- c()
next_chunk_number <- 1

if (dir.exists(uncons_output_dir)) {
  existing_files <- dir_ls(
    uncons_output_dir,
    glob = "*/data_chunk_*.csv",
    recurse = TRUE
  )
  if (length(existing_files) > 0) {
    existing_ier_bks <- map_df(
      existing_files,
      ~ read_csv(.x, col_types = cols(ier_bk = col_character())) |>
        select(ier_bk)
    ) |>
      distinct(ier_bk) |>
      pull(ier_bk)

    last_chunk_number <- existing_files |>
      str_extract("data_chunk_(\\d{3})\\.csv") |>
      str_replace_all(c("data_chunk_" = "", "\\.csv" = "")) |>
      as.numeric() |>
      max()
    next_chunk_number <- last_chunk_number + 1
  }
}

unconsolidated <- unconsolidated |>
  filter(!(ier_bk %in% existing_ier_bks))

# Export data in chunks

# Split unconsolidated into chunks of 10
unconsolidated_chunks <- unconsolidated |>
  group_by((row_number() - 1) %/% 10) |>
  group_split()

# Write each chunk to a CSV file
for (i in seq_along(unconsolidated_chunks)) {
  chunk_number_padded <- str_pad(next_chunk_number + i - 1, 3, pad = "0")
  file_name <- glue("{uncons_output_dir}/data_chunk_{chunk_number_padded}.csv")
  write_csv(unconsolidated_chunks[[i]], file_name)
}
