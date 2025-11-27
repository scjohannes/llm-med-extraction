#' @importFrom ellmer chat_openai type_object type_string type_boolean
#' @importFrom glue glue
#' @importFrom dplyr select mutate across ends_with if_else relocate any_of
#' @importFrom tidyr ends_with
#' @importFrom REDCapR redcap_write
#' @importFrom rlang abort is_string is_scalar_integerish is_scalar_logical is_scalar_double
NULL

#' Extract Metastasis Data From Imaging Report by Body Region
#'
#' Groups imaging data by body region, applies a region-specific extraction of
#' metastasis function to the imaging report, and unnests the results.
#'
#' @param data A data frame containing at least 'body_region' and
#' 'imaging_report' columns.
#' @param ... Additional arguments to be passed down to the underlying
#'   region-specific extraction functions (e.g., `extract_head_metastasis`,
#'   `extract_abdomen_metastasis`). Examples include `model`, `model_options`, etc.
#'
#' @return A tibble with the original data plus columns resulting from the
#'   unnested extraction results. Groups where extraction function is missing
#'   or fails may return unmodified or partially processed.
#' @export
#'
#' @examples
#' \dontrun{
#'   processed_results <- apply_metastasis_extraction(
#'     data,
#'     model = "qwq:32b",
#'     api_key =
#'     model_options = list(temperature = 0)
#'   )
#' }
apply_metastasis_extraction <- function(data, ...) {
  colnames(data) <- tolower(colnames(data))

  # Check for necessary columns
  stopifnot(
    "Input 'data' must be a data frame" = is.data.frame(data),
    "Input 'data' must contain 'imaging_report' column" = "imaging_report" %in%
      names(data)
  )

  # Get the environment for function lookup (includes package namespace)
  calling_env <- rlang::current_env()

  # Capture the ... arguments to potentially use them outside map if needed,
  # though direct forwarding as below is often sufficient.
  # extra_args <- list(...)
  if (!("body_region" %in% names(data))) {
    if (!("iet_bk" %in% names(data))) {
      stop("'iet_bk' missing for join of body region")
    }
    data <- data |>
      dplyr::left_join(imaging_region_lookup, by = c("iet_bk" = "IET_BK"))
  }

  data_processed <- data |>
    dplyr::group_by(body_region) |>
    # Pass the ... from process_imaging_data into group_modify
    dplyr::group_modify(
      \(sub_data, key, ...) {
        # Capture passed ... in the lambda
        # sub_data: the subset of data for the current body_region
        # key: a tibble containing the grouping variable (body_region)
        # ...: additional arguments passed from process_imaging_data via group_modify

        current_region <- key$body_region
        func_name <- paste0("extract_", current_region, "_metastasis")

        # Check if the function exists in the package environment (or imports)
        if (
          nrow(sub_data) > 0 &&
            rlang::env_has(calling_env, func_name, inherit = TRUE)
        ) {
          # Retrieve the function
          extract_func <- rlang::env_get(calling_env, func_name, inherit = TRUE)

          if (!is.function(extract_func)) {
            warning(paste(
              "Object",
              func_name,
              "found but is not a function for body_region:",
              current_region,
              "- Skipping extraction."
            ))
            return(sub_data) # Return unmodified data for this group
          }

          # Apply the extraction function, passing the IMAGING_REPORT column value
          # AND any additional arguments captured by ... in the lambda
          sub_data <- sub_data |>
            dplyr::mutate(
              extraction = purrr::map(
                .x = imaging_report,
                .f = extract_func,
                ..., # Pass arguments from lambda's ... to extract_func
                .progress = FALSE # Progress per group might be noisy
              )
            ) |>
            # NEW: Normalize extraction results before unnesting
            # This is a safety net in case .coerce_fields didn't catch everything
            dplyr::mutate(
              extraction = purrr::map(extraction, function(ext_result) {
                if (!is.list(ext_result)) {
                  return(ext_result)
                }

                # Ensure all character fields are atomic, not lists
                for (field_name in names(ext_result)) {
                  field_val <- ext_result[[field_name]]

                  # Special handling for justification and other character fields
                  if (
                    field_name %in%
                      c("justification", "prompt") &&
                      is.list(field_val)
                  ) {
                    # Flatten list to character
                    flat <- unlist(
                      field_val,
                      recursive = TRUE,
                      use.names = FALSE
                    )
                    ext_result[[field_name]] <- if (length(flat) > 0) {
                      paste(as.character(flat), collapse = " ")
                    } else {
                      NA_character_
                    }
                  }

                  # General handling: if any field is a list when it shouldn't be
                  # (except for expected list fields if you have any)
                  if (
                    is.list(field_val) &&
                      length(field_val) == 1 &&
                      !is.list(field_val[[1]])
                  ) {
                    # Single-element list - extract the element
                    ext_result[[field_name]] <- field_val[[1]]
                  }
                }

                ext_result
              })
            ) |>
            tidyr::unnest_wider(extraction)
        } else if (nrow(sub_data) > 0) {
          warning(paste(
            "Extraction function",
            func_name,
            "not found for body_region:",
            current_region,
            "- Skipping extraction for this group."
          ))
        }

        # Return the processed (or original) data for this group
        sub_data
      },
      ...
    ) |> # This passes the ... arguments received by process_imaging_data down to the lambda
    dplyr::ungroup() # Remove the grouping structure

  return(data_processed)
}


# =========================
# COMMON SYSTEM PROMPT
# =========================

# This template contains the core instructions common to all functions.
# Placeholders {region_name} and {metastasis_questions} will be filled in.
.SYSTEM_PROMPT_TEMPLATE <- "
Assume you are a trained and experienced radiologist analyzing imaging scans (CT, MRI, X-Ray, or CT/PET) from cancer patients.
You will be given a report of the imaging exam of the {region_name} region(s) of a patient.
You are asked to extract whether this patient presently has metastasis mentioned in the report.
Please answer for each of the following separately:

{metastasis_questions}

**Interpreting PET-CT Findings:**
* Pay close attention to descriptions of lesions, particularly those identified as 'hypermetabolic', 'FDG-avid', having 'increased uptake', or showing a specific 'SUV' (Standardized Uptake Value). These indicate metabolic activity often associated with cancer, but require careful interpretation within the report's context.
* Radiologists may use terms like 'lesion', 'nodule', 'mass', 'uptake', or 'activity' instead of explicitly stating 'metastasis'. Your task is to determine if the description and context imply metastatic disease.

** Interpreting CT and MRI-Findings:
* Identify if the radiologist explicitly labels any finding as 'Metastaste','metastasenverdächtig', or similar.
* Assess if the radiologist describes lesions/nodules/masses with characteristics and then explicitly states they are suspicious for, concerning for, consistent with, likely, or typical of metastasis.
* Note if the radiologist offers a differential diagnosis; the likelihood assigned to metastasis is key (see TRUE/FALSE criteria).

**Answering TRUE/FALSE:**
* Answer with TRUE if there is:
    * A clear mention of metastasis in the specific location.
    * A description of a lesion where the radiologist expresses a *strong suspicion* or high likelihood of it representing metastasis (e.g., 'metastasenverdächtig', 'concerning for metastatic deposit', 'vereinbar mit Metastasierung', 'wahrscheinliche Metastasen').
* Answer with FALSE if:
    * Metastasis is mentioned but considered *less likely* than other possibilities (e.g., listed lower in the differential diagnosis: 'DD inflammation DD metastasis' -> FALSE; 'likely inflammatory' -> FALSE).
    * A hypermetabolic finding is clearly attributed to a non-malignant cause (e.g., inflammation, infection, post-surgical changes, physiological uptake).
    * There is no mention of findings suspicious for metastasis in that location.
    * The report explicitly states the lesion is *unlikely* to be metastasis
    * Do not consider any information about metastasis, which is from the case history ('Anamnese'). E.g., it might be mentioned that the patient has a metastasis of an organ that is not part of the current imaging exam. Do NOT consider this information. Only focus on what is seen in the current imaging exam.

**Distinguishing Primary Tumor vs. Metastasis:**
  *  Progression, recurrence, or residual disease of the known primary tumor at its original site is NOT metastasis for the purpose of this task.
  *  A hypermetabolic focus at the site of the known primary tumor should generally be considered related to the primary, UNLESS the report explicitly suggests it represents a metastatic deposit separate from the main primary mass or indicates metastatic spread within the same organ but distinct from the primary focus.
  *  If the radiologist describes only the primary tumor location and its characteristics (even if hypermetabolic), do NOT label this as a metastatic location.
  *  Keep in mind that progression of a primary tumor in this region is NOT a metastasis.
  *  The lesion of the primary tumour is NOT a metastasis. Pay attention to whether the radiologist labels the lesion as a metastasis or as the primary tumour.
  *  If the radiologist describes the location of the primary tumour only, do NOT use this as a location for metastasis, unless the patient has metastasis in the same location as the primary tumour.


You can find the report text below:
  
"

# =========================
# STANDARDIZED QUESTIONS
# =========================

# Define standardized questions here to ensure consistency across all functions
.Q_CNS <- "This patient has metastasis of the brain parenchyma."
.Q_MENINGEAL <- "This patient has meningeal metastasis."
.Q_BONE <- "This patient has bone metastasis."
.Q_LYMPH <- "This patient has lymph node metastasis."
.Q_SOFT_TISSUE <- "This patient has soft tissue metastasis."
.Q_LIVER <- "This patient has liver metastasis."
.Q_ADRENAL <- "This patient has adrenal metastasis."
.Q_KIDNEY <- "This patient has kidney metastasis."
.Q_SPLEEN <- "This patient has spleen metastasis."
.Q_PANCREAS <- "This patient has pancreatic metastasis."
.Q_PERITONEAL <- "This patient has peritoneal metastasis."
.Q_OVARIAN <- "This patient has ovarian metastasis."
.Q_LUNG <- "This patient has lung metastasis."
.Q_PLEURA <- "This patient has pleural metastasis."
.Q_OTHER_ORGAN <- "This patient has metastasis in any organ mentioned."
.Q_JUSTIFICATION <- "Provide justification based only on the text."


# =========================
# BASE EXTRACTOR
# =========================

#' Base function for extracting metastasis information (Internal)
#'
#' This internal helper function handles the common logic for interacting
#' with the LLM via the ellmer package.
#'
#' @param text Character string containing the radiology text.
#' @param type_definition An ellmer type definition (e.g., created by type_object).
#' @param system_prompt_template A template string for the system prompt. Should
#'   contain placeholders like `{region_name}` and `{metastasis_questions}`.
#' @param region_name The specific anatomical region (e.g., "head/brain", "abdomen").
#' @param metastasis_questions A formatted string listing the specific questions
#'   for the LLM based on the type_definition.
#' @param error_return_list A list structure with NA values corresponding to the
#'   `type_definition`, used as the return value in case of an error.
#' @param model Specifies the LLM to use.
#' @param model_options List of parameters to pass to the model.
#' @param base_url The base URL for the Ollama API.
#' @param api_key API key for authentication.
#' @return A list containing extracted metastasis information, model name, and prompt,
#'   or the `error_return_list` with model/prompt info on error.
.extract_metastasis_base <- function(
  text,
  type_definition,
  system_prompt_template,
  region_name,
  metastasis_questions,
  # error_return_list,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("Package 'glue' is required but not installed. Please install it.")
  }
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required but not installed. Please install it.")
  }

  # enforce typed NAs consistently in error_return_list
  error_return_list <- .type_def_to_error_list(type_definition)

  use_chat_json <- isTRUE(model_options$use_chat_json) ||
    !.supports_structured(model)

  prompt <- glue::glue(
    system_prompt_template,
    region_name = region_name,
    metastasis_questions = metastasis_questions,
    .trim = FALSE
  )

  # 2. Initialize chat with the model
  # Ensure ellmer is available
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required but not installed. Please install it.")
  }

  chat <- tryCatch(
    {
      ellmer::chat_openai(
        system_prompt = "You are a highly trained medical AI assistant helping users to extract the location of metastases from radiology reports.",
        base_url = base_url,
        api_key = api_key,
        model = model,
        api_args = model_options
      )
    },
    error = function(e_init) {
      warning(glue::glue(
        "Failed to initialize ellmer::chat_openai for model '{model}' at '{base_url}': {e_init$message}"
      ))
      return(NULL)
    }
  )

  if (is.null(chat)) {
    # Add model and prompt info to the error list before returning
    error_return_list$model <- model
    error_return_list$prompt <- prompt
    return(error_return_list)
  }

  # 3. Extract and return structured data
  # Model supports JSON
  if (!use_chat_json) {
    extracted_data <- tryCatch(
      {
        result <- chat$chat_structured(
          paste(prompt, text),
          type = type_definition
        )
        result$model <- model
        result$prompt <- prompt
        result
      },
      error = function(e_extract) {
        warning(glue::glue(
          "Error during API call or data extraction for model '{model}': {e_extract$message}"
        ))
        # Add model and prompt info to the error list
        error_return_list$model <- model
        error_return_list$prompt <- prompt
        error_return_list # Return the predefined NA list structure
      }
    )
  } else {
    # JSON-only fallback path
    extracted_data <- tryCatch(
      {
        .chat_json_extract(
          chat,
          text,
          .SYSTEM_PROMPT_TEMPLATE,
          region_name,
          metastasis_questions,
          type_definition,
          model
        )
      },
      error = function(e_extract) {
        warning(glue::glue(
          "Error during JSON-only chat extraction for model '{model}': {e_extract$message}"
        ))
        error_return_list$model <- model
        error_return_list$prompt <- prompt
        error_return_list
      }
    )
  }
  return(extracted_data)
}


# =========================
# REGION-SPECIFIC EXTRACTORS
# =========================

#' Extract Head Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about head metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted head metastasis information (cns_metastasis,
#'   bone_metastasis, meningeal_metastasis, justification), model name, and prompt used.
#'   Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "MRI Brain: Findings suggestive of multiple parenchymal metastases.
#'                No definite calvarial or meningeal involvement identified."
#' results <- extract_head_metastasis(report_text)
#' results
#' }
extract_head_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure for this region
  type_def <- ellmer::type_object(
    cns_metastasis = ellmer::type_boolean(.Q_CNS),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    meningeal_metastasis = ellmer::type_boolean(.Q_MENINGEAL),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_CNS} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_MENINGEAL} TRUE / FALSE
"
  )
  # Define the structure for error return
  # error_list <- list(
  #   cns_metastasis = NA,
  #   bone_metastasis = NA,
  #   meningeal_metastasis = NA,
  #   justification = NA_character_
  # )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "head/brain",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}

#' Extract Head and Neck Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about head and neck metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted head and neck metastasis information (cns_metastasis,
#'   meningeal_metastasis, lymph_node_metastasis, bone_metastasis,
#'   soft_tissue_metastasis, justification), model name, and prompt used.
#'   Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "CT Neck: Multiple enlarged necrotic cervical lymph nodes, suspicious
#'                for metastasis. No suspicious bone lesions."
#' results <- extract_head_neck_metastasis(report_text)
#' print(results)
#' }
extract_head_neck_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    cns_metastasis = ellmer::type_boolean(.Q_CNS),
    meningeal_metastasis = ellmer::type_boolean(.Q_MENINGEAL),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_CNS} TRUE / FALSE
- {.Q_MENINGEAL} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "head and neck",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}

#' Extract Abdomen Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about abdomen metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted abdomen metastasis information (liver_metastasis,
#'   adrenal_metastasis, kidney_metastasis, spleen_metastasis, pancreas_metastasis,
#'   peritoneal_metastasis, lymph_node_metastasis, bone_metastasis,
#'   soft_tissue_metastasis, justification), model name, and prompt used.
#'   Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "CT Abdomen: Multiple hypodense liver lesions consistent with
#'                metastases. No suspicious retroperitoneal nodes."
#' results <- extract_abdomen_metastasis(report_text)
#' results
#' }
extract_abdomen_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    liver_metastasis = ellmer::type_boolean(.Q_LIVER),
    adrenal_metastasis = ellmer::type_boolean(.Q_ADRENAL),
    kidney_metastasis = ellmer::type_boolean(.Q_KIDNEY),
    spleen_metastasis = ellmer::type_boolean(.Q_SPLEEN),
    pancreas_metastasis = ellmer::type_boolean(.Q_PANCREAS),
    peritoneal_metastasis = ellmer::type_boolean(.Q_PERITONEAL),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LIVER} TRUE / FALSE
- {.Q_ADRENAL} TRUE / FALSE
- {.Q_KIDNEY} TRUE / FALSE
- {.Q_SPLEEN} TRUE / FALSE
- {.Q_PANCREAS} TRUE / FALSE
- {.Q_PERITONEAL} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "abdomen",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}

#' Extract Abdomen and Pelvis Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about abdomen and pelvis metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted abdomen/pelvis metastasis information (liver_metastasis,
#'   adrenal_metastasis, kidney_metastasis, spleen_metastasis, pancreas_metastasis,
#'   peritoneal_metastasis, ovarian_metastasis, lymph_node_metastasis, bone_metastasis,
#'   soft_tissue_metastasis, justification), model name, and prompt used.
#'   Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "CT Abdomen/Pelvis: Liver mets noted. Also extensive peritoneal
#'                carcinomatosis throughout the abdomen and pelvis. Lytic lesion
#'                in the left iliac bone, suspicious for metastasis."
#' results <- extract_abdomen_pelvis_metastasis(report_text)
#' results
#' }
extract_abdomen_pelvis_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    liver_metastasis = ellmer::type_boolean(.Q_LIVER),
    adrenal_metastasis = ellmer::type_boolean(.Q_ADRENAL),
    kidney_metastasis = ellmer::type_boolean(.Q_KIDNEY),
    spleen_metastasis = ellmer::type_boolean(.Q_SPLEEN),
    pancreas_metastasis = ellmer::type_boolean(.Q_PANCREAS),
    peritoneal_metastasis = ellmer::type_boolean(.Q_PERITONEAL), # Using general peritoneal Q
    ovarian_metastasis = ellmer::type_boolean(.Q_OVARIAN),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LIVER} TRUE / FALSE
- {.Q_ADRENAL} TRUE / FALSE
- {.Q_KIDNEY} TRUE / FALSE
- {.Q_SPLEEN} TRUE / FALSE
- {.Q_PANCREAS} TRUE / FALSE
- {.Q_PERITONEAL} TRUE / FALSE
- {.Q_OVARIAN} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "abdomen and pelvis",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Abdomen, Pelvis and Lower Extremity Metastasis Information
#'
#' Processes radiology text to extract information about abdomen, pelvis, and lower extremity metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted metastasis information for the specified regions,
#'   model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "PET/CT: Hypermetabolic foci in the liver, peritoneum,
#'                and inguinal lymph nodes, consistent with widespread metastases."
#' results <- extract_abdomen_pelvis_lower_extremity_metastasis(report_text)
#' results
#' }
extract_abdomen_pelvis_lower_extremity_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    liver_metastasis = ellmer::type_boolean(.Q_LIVER),
    adrenal_metastasis = ellmer::type_boolean(.Q_ADRENAL),
    kidney_metastasis = ellmer::type_boolean(.Q_KIDNEY),
    spleen_metastasis = ellmer::type_boolean(.Q_SPLEEN),
    pancreas_metastasis = ellmer::type_boolean(.Q_PANCREAS),
    peritoneal_metastasis = ellmer::type_boolean(.Q_PERITONEAL),
    ovarian_metastasis = ellmer::type_boolean(.Q_OVARIAN),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LIVER} TRUE / FALSE
- {.Q_ADRENAL} TRUE / FALSE
- {.Q_KIDNEY} TRUE / FALSE
- {.Q_SPLEEN} TRUE / FALSE
- {.Q_PANCREAS} TRUE / FALSE
- {.Q_PERITONEAL} TRUE / FALSE
- {.Q_OVARIAN} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "abdomen, pelvis and lower extremities",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Lower Extremity Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about lower extremity metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted lower extremity metastasis information (lymph_node_metastasis,
#'   bone_metastasis, soft_tissue_metastasis, justification), model name, and prompt used.
#'   Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "MRI Right Femur: Large destructive lesion in the mid-shaft, highly
#'                suspicious for metastasis. Associated soft tissue component."
#' results <- extract_lower_extremity_metastasis(report_text)
#' results
#' }
extract_lower_extremity_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "lower extremities",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Neck Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about neck metastases using an LLM.
#' (Excludes head-specific metastases like CNS/meningeal).
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted neck metastasis information (lymph_node_metastasis,
#'   bone_metastasis, soft_tissue_metastasis, justification), model name, and prompt used.
#'   Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "CT Neck: Multiple enlarged necrotic cervical lymph nodes, suspicious
#'                for metastasis. No suspicious bone lesions in the cervical spine."
#' results <- extract_neck_metastasis(report_text)
#' results
#' }
extract_neck_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "neck",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Neck and Thorax Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about neck and thorax metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted neck/thorax metastasis information (lung_metastasis,
#'   lymph_node_metastasis, bone_metastasis, soft_tissue_metastasis, justification),
#'   model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "CT Chest/Neck: Multiple pulmonary nodules suspicious for metastases.
#'                Enlarged supraclavicular and mediastinal lymph nodes noted."
#' results <- extract_neck_thorax_metastasis(report_text)
#' results
#' }
extract_neck_thorax_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    lung_metastasis = ellmer::type_boolean(.Q_LUNG),
    pleural_metastasis = ellmer::type_boolean(.Q_PLEURA),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LUNG} TRUE / FALSE
- {.Q_PLEURA} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "neck and thorax",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Neck, Thorax, Abdomen and Pelvis Metastasis Information
#'
#' Processes radiology text to extract information about neck, thorax, abdomen, and pelvis metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted metastasis information for the specified regions,
#'   model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "PET/CT: Widespread metastatic disease involving lungs, liver,
#'                peritoneum, multiple lymph node stations (neck, chest, abdomen, pelvis),
#'                and bones (spine, pelvis)."
#' results <- extract_neck_thorax_abdomen_pelvis_metastasis(report_text)
#' results
#' }
extract_neck_thorax_abdomen_pelvis_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    lung_metastasis = ellmer::type_boolean(.Q_LUNG),
    pleural_metastasis = ellmer::type_boolean(.Q_PLEURA),
    liver_metastasis = ellmer::type_boolean(.Q_LIVER),
    adrenal_metastasis = ellmer::type_boolean(.Q_ADRENAL),
    kidney_metastasis = ellmer::type_boolean(.Q_KIDNEY),
    spleen_metastasis = ellmer::type_boolean(.Q_SPLEEN),
    pancreas_metastasis = ellmer::type_boolean(.Q_PANCREAS),
    peritoneal_metastasis = ellmer::type_boolean(.Q_PERITONEAL),
    ovarian_metastasis = ellmer::type_boolean(.Q_OVARIAN),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LUNG} TRUE / FALSE
- {.Q_PLEURA} TRUE / FALSE
- {.Q_LIVER} TRUE / FALSE
- {.Q_ADRENAL} TRUE / FALSE
- {.Q_KIDNEY} TRUE / FALSE
- {.Q_SPLEEN} TRUE / FALSE
- {.Q_PANCREAS} TRUE / FALSE
- {.Q_PERITONEAL} TRUE / FALSE
- {.Q_OVARIAN} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "neck, thorax, abdomen and pelvis",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Other Region Metastasis Information from Radiology Text
#'
#' Processes radiology text for non-standard/vague regions, focusing on common metastatic sites.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted metastasis information for 'other' regions (lymph_node_metastasis,
#'   bone_metastasis, soft_tissue_metastasis, other_organ_metastasis, justification),
#'   model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "Follow-up CT: Stable appearance of known liver and bone metastases.
#'                No new sites of disease identified."
#' results <- extract_other_metastasis(report_text)
#' results
#' }
extract_other_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure - focusing on common types
  type_def <- ellmer::type_object(
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    other_organ_metastasis = ellmer::type_boolean(.Q_OTHER_ORGAN),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
- {.Q_OTHER_ORGAN} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "various or unspecified body", # Generic region name
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Pelvis Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about pelvis metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted pelvis metastasis information (peritoneal_metastasis,
#'   ovarian_metastasis, lymph_node_metastasis, bone_metastasis, soft_tissue_metastasis,
#'   justification), model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "MRI Pelvis: Large metastatic deposit seen in the sacrum.
#'                No definite peritoneal or ovarian involvement."
#' results <- extract_pelvis_metastasis(report_text)
#' results
#' }
extract_pelvis_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    peritoneal_metastasis = ellmer::type_boolean(.Q_PERITONEAL),
    ovarian_metastasis = ellmer::type_boolean(.Q_OVARIAN),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_PERITONEAL} TRUE / FALSE
- {.Q_OVARIAN} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "pelvis",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Pelvis and Lower Extremity Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about pelvis and lower extremity metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted metastasis information for the specified regions,
#'   model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "CT Pelvis/LE: Metastatic lesions identified in the right iliac bone
#'                and proximal left femur. Enlarged inguinal lymph nodes present."
#' results <- extract_pelvis_lower_extremity_metastasis(report_text)
#' results
#' }
extract_pelvis_lower_extremity_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    peritoneal_metastasis = ellmer::type_boolean(.Q_PERITONEAL),
    ovarian_metastasis = ellmer::type_boolean(.Q_OVARIAN),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_PERITONEAL} TRUE / FALSE
- {.Q_OVARIAN} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "pelvis and lower extremities",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Spine Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about spine metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted spine metastasis information (bone_metastasis,
#'   lymph_node_metastasis, soft_tissue_metastasis, justification), model name, and prompt used.
#'   Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "MRI T-Spine: Multiple enhancing lesions throughout the thoracic
#'                vertebral bodies, characteristic of osseous metastases. Epidural
#'                soft tissue extension at T6-T7 causing cord compression."
#' results <- extract_spine_metastasis(report_text)
#' results
#' }
extract_spine_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH), # Assuming paravertebral nodes fit general lymph node Q
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE), # Assuming paraspinal/epidural fit general soft tissue Q
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_BONE} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "spine",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Spine and Pelvis Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about spine and pelvis metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted metastasis information for the specified regions,
#'   model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "MRI L-Spine/Pelvis: Extensive metastatic disease involving the
#'                lumbar spine and pelvic bones. No definite nodal or peritoneal mets."
#' results <- extract_spine_pelvis_metastasis(report_text)
#' results
#' }
extract_spine_pelvis_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    ovarian_metastasis = ellmer::type_boolean(.Q_OVARIAN),
    peritoneal_metastasis = ellmer::type_boolean(.Q_PERITONEAL),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_BONE} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
- {.Q_OVARIAN} TRUE / FALSE
- {.Q_PERITONEAL} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "spine and pelvis",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Thorax Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about thorax metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted thorax metastasis information (lung_metastasis,
#'   lymph_node_metastasis, bone_metastasis, soft_tissue_metastasis, justification),
#'   model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "CT Chest: Innumerable pulmonary metastases. Metastatic lesions
#'                also seen in the ribs and thoracic spine. Mediastinal nodes are enlarged."
#' results <- extract_thorax_metastasis(report_text)
#' results
#' }
extract_thorax_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    lung_metastasis = ellmer::type_boolean(.Q_LUNG),
    pleural_metastasis = ellmer::type_boolean(.Q_PLEURA),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LUNG} TRUE / FALSE
- {.Q_PLEURA} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "thorax",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Thorax and Abdomen Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about thorax and abdomen metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted metastasis information for the specified regions,
#'   model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "CT Chest/Abdomen: Lung and liver metastases are present.
#'                Adrenal nodule likely metastatic. No suspicious nodes."
#' results <- extract_thorax_abdomen_metastasis(report_text)
#' results
#' }
extract_thorax_abdomen_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    lung_metastasis = ellmer::type_boolean(.Q_LUNG),
    pleural_metastasis = ellmer::type_boolean(.Q_PLEURA),
    liver_metastasis = ellmer::type_boolean(.Q_LIVER),
    adrenal_metastasis = ellmer::type_boolean(.Q_ADRENAL),
    kidney_metastasis = ellmer::type_boolean(.Q_KIDNEY),
    spleen_metastasis = ellmer::type_boolean(.Q_SPLEEN),
    pancreas_metastasis = ellmer::type_boolean(.Q_PANCREAS),
    peritoneal_metastasis = ellmer::type_boolean(.Q_PERITONEAL),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LUNG} TRUE / FALSE
- {.Q_PLEURA} TRUE / FALSE
- {.Q_LIVER} TRUE / FALSE
- {.Q_ADRENAL} TRUE / FALSE
- {.Q_KIDNEY} TRUE / FALSE
- {.Q_SPLEEN} TRUE / FALSE
- {.Q_PANCREAS} TRUE / FALSE
- {.Q_PERITONEAL} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "thorax and abdomen",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Thorax, Abdomen and Pelvis Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about thorax, abdomen, and pelvis metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted metastasis information for the specified regions,
#'   model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "CT CAP: Multiple lung and liver mets. Extensive peritoneal
#'                carcinomatosis. Pelvic bone mets also noted."
#' results <- extract_thorax_abdomen_pelvis_metastasis(report_text)
#' results
#' }
extract_thorax_abdomen_pelvis_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    lung_metastasis = ellmer::type_boolean(.Q_LUNG),
    pleural_metastasis = ellmer::type_boolean(.Q_PLEURA),
    liver_metastasis = ellmer::type_boolean(.Q_LIVER),
    adrenal_metastasis = ellmer::type_boolean(.Q_ADRENAL),
    kidney_metastasis = ellmer::type_boolean(.Q_KIDNEY),
    spleen_metastasis = ellmer::type_boolean(.Q_SPLEEN),
    pancreas_metastasis = ellmer::type_boolean(.Q_PANCREAS),
    peritoneal_metastasis = ellmer::type_boolean(.Q_PERITONEAL),
    ovarian_metastasis = ellmer::type_boolean(.Q_OVARIAN),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LUNG} TRUE / FALSE
- {.Q_PLEURA} TRUE / FALSE
- {.Q_LIVER} TRUE / FALSE
- {.Q_ADRENAL} TRUE / FALSE
- {.Q_KIDNEY} TRUE / FALSE
- {.Q_SPLEEN} TRUE / FALSE
- {.Q_PANCREAS} TRUE / FALSE
- {.Q_PERITONEAL} TRUE / FALSE
- {.Q_OVARIAN} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "thorax, abdomen and pelvis",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Upper Extremity Metastasis Information from Radiology Text
#'
#' Processes radiology text to extract information about upper extremity metastases using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted upper extremity metastasis information (lymph_node_metastasis,
#'   bone_metastasis, soft_tissue_metastasis, justification), model name, and prompt used.
#'   Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "X-Ray Left Humerus: Permeative lesion in the diaphysis, concerning
#'                for metastasis. Axillary nodes not evaluated."
#' results <- extract_upper_extremity_metastasis(report_text)
#' results
#' }
extract_upper_extremity_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the specific structure
  type_def <- ellmer::type_object(
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "upper extremities",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}


#' Extract Whole Body Metastasis Information from Radiology Text
#'
#' Processes radiology text (likely from whole-body scans) to extract comprehensive metastasis information using an LLM.
#'
#' @inheritParams .extract_metastasis_base
#' @return A list containing extracted whole body metastasis information across multiple sites,
#'   model name, and prompt used. Returns NA for fields if an error occurs.
#' @export
#' @examples
#' \dontrun{ # Requires ellmer and a running Ollama instance
#' report_text <- "Whole Body PET/CT: Hypermetabolic lesions seen in lungs, liver,
#'                multiple bones (spine, ribs, pelvis), and mediastinal lymph nodes,
#'                consistent with widespread metastatic disease. Brain MRI recommended
#'                for further evaluation."
#' results <- extract_whole_body_metastasis(report_text)
#' results
#' }
extract_whole_body_metastasis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # Define the comprehensive structure
  type_def <- ellmer::type_object(
    cns_metastasis = ellmer::type_boolean(.Q_CNS),
    meningeal_metastasis = ellmer::type_boolean(.Q_MENINGEAL),
    lung_metastasis = ellmer::type_boolean(.Q_LUNG),
    pleural_metastasis = ellmer::type_boolean(.Q_PLEURA),
    liver_metastasis = ellmer::type_boolean(.Q_LIVER),
    adrenal_metastasis = ellmer::type_boolean(.Q_ADRENAL),
    kidney_metastasis = ellmer::type_boolean(.Q_KIDNEY),
    spleen_metastasis = ellmer::type_boolean(.Q_SPLEEN),
    pancreas_metastasis = ellmer::type_boolean(.Q_PANCREAS),
    peritoneal_metastasis = ellmer::type_boolean(.Q_PERITONEAL),
    ovarian_metastasis = ellmer::type_boolean(.Q_OVARIAN),
    lymph_node_metastasis = ellmer::type_boolean(.Q_LYMPH),
    bone_metastasis = ellmer::type_boolean(.Q_BONE),
    soft_tissue_metastasis = ellmer::type_boolean(.Q_SOFT_TISSUE),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  # Define the questions part of the prompt, matching type_def
  questions <- glue::glue(
    "
- {.Q_CNS} TRUE / FALSE
- {.Q_MENINGEAL} TRUE / FALSE
- {.Q_LUNG} TRUE / FALSE
- {.Q_PLEURA} TRUE / FALSE
- {.Q_LIVER} TRUE / FALSE
- {.Q_ADRENAL} TRUE / FALSE
- {.Q_KIDNEY} TRUE / FALSE
- {.Q_SPLEEN} TRUE / FALSE
- {.Q_PANCREAS} TRUE / FALSE
- {.Q_PERITONEAL} TRUE / FALSE
- {.Q_OVARIAN} TRUE / FALSE
- {.Q_LYMPH} TRUE / FALSE
- {.Q_BONE} TRUE / FALSE
- {.Q_SOFT_TISSUE} TRUE / FALSE
"
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "whole body",
    metastasis_questions = questions,
    # error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url,
    api_key = api_key
  )
}
