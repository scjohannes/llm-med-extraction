#' Internal: Run the full RANO/RECIST PET chain
#'
#' Orchestrates the "chained" RECIST-path calls for PET.
#'
#' @return A list with response_to_trt_pet, no_tumor, and justification.
#' @noRd
.run_recist_chain_pet <- function(
  text,
  model,
  model_options,
  base_url,
  api_key
) {
  # Step 1: Extract verbatim text
  # This function inits and returns the chat object
  chat_obj <- extract_rano_recist_verbatim(
    text,
    model,
    model_options,
    base_url,
    api_key
  )

  if (is.null(chat_obj)) {
    # Return the structure expected by the pipeline
    return(list(
      response_to_trt_pet = NA_character_,
      no_tumor = NA,
      justification = "Failed in verbatim extraction step."
    ))
  }

  # Step 2: Map to category (using the modified function)
  mapped_data <- map_rano_recist_verbatim_to_pet_category(chat_obj)

  # mapped_data already contains response_to_trt_pet, justification, and no_tumor
  return(mapped_data)
}

#' Internal: Run the full RANO/RECIST Non-PET chain
#'
#' Orchestrates the "chained" RECIST-path calls for Non-PET.
#'
#' @return A list with response_to_trt, no_tumor, and justification.
#' @noRd
.run_recist_chain_non_pet <- function(
  text,
  model,
  model_options,
  base_url,
  api_key
) {
  # Step 1: Extract verbatim text
  chat_obj <- extract_rano_recist_verbatim(
    text,
    model,
    model_options,
    base_url,
    api_key
  )

  if (is.null(chat_obj)) {
    return(list(
      response_to_trt = NA_character_,
      no_tumor = NA,
      justification = "Failed in verbatim extraction step."
    ))
  }

  # Step 2: Map to category (using the modified function)
  mapped_data <- map_rano_recist_verbatim_to_non_pet_category(chat_obj)

  # mapped_data already contains response_to_trt, justification, and no_tumor
  return(mapped_data)
}


#' Pipeline for Extracting Cancer Treatment Response from Imaging Reports
#'
#' @description
#' This function orchestrates a multi-step pipeline to analyze an imaging report
#' text and determine the patient's response to cancer treatment. It first assesses
#' if the report is a treatment response assessment ('in scope'). If so, it checks
#' for the presence of standardized criteria (RANO/RECIST). Based on these findings
#' and the imaging modality (PET-CT or non-PET), it follows different paths to
#' extract and classify the treatment response.
#'
#' @param text Character string. The full text of the imaging report.
#' @param pet_ct Numeric or Logical. Indicates if the report is a PET-CT scan (1 or TRUE)
#'   or not (0 or FALSE).
#' @param model Character string. The identifier for the LLM to be used.
#' @param strategy Character string. The prompt strategy to use.
#'   One of:
#'   - "auto" (default): Automatically selects "single_shot" for models
#'     like 'gpt-oss' and "chained" for others.
#'   - "single_shot": Forces the use of the consolidated 'v2' prompts.
#'   - "chained": Forces the use of the multi-step prompts.
#' @param model_options List. Arguments specific to the LLM API.
#' @param base_url Character string. The base URL for the LLM API service.
#' @param api_key Character string. API key for authentication.
#'
#' @return A list containing the final classification, the path taken through the
#'   pipeline, and raw intermediate results.
#' @export
#' @importFrom ellmer chat_openai type_object type_enum type_string type_boolean
#' @importFrom glue glue
response_to_treatment_pipeline <- function(
  text,
  pet_ct,
  model = .DEFAULT_MODEL,
  strategy = c("auto", "single_shot", "chained"),
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  # --- 1. Determine effective strategy ---
  strategy <- match.arg(strategy)

  effective_strategy <- if (strategy == "auto") {
    ifelse(.is_single_shot_model(model), "single_shot", "chained")
  } else {
    strategy
  }

  # --- 2. Step 1: In Scope? ---
  # This function is common to both strategies
  p1 <- assess_imaging_report_scope(
    text,
    model,
    model_options,
    base_url,
    api_key
  )

  # Standardize pet_ct to logical
  is_pet <- as.logical(pet_ct)

  if (
    is.null(p1) ||
      !is.list(p1) ||
      is.null(p1$response) ||
      is.na(p1$response$scope_assessment) ||
      p1$response$scope_assessment != "IN_SCOPE"
  ) {
    if (is_pet) {
      return(list(
        response_to_trt_pet = "NOT_APPLICABLE",
        no_tumor = NA,
        path = "scope->out"
      ))
    } else {
      return(list(
        response_to_trt = "NOT_APPLICABLE",
        no_tumor = NA,
        path = "scope->out"
      ))
    }
  }

  # --- 3. Step 2: Detect if RANO/RECIST present ---
  rano_recist <- extract_rano_recist(text)

  # --- PATH A: RECIST_FALSE (Free-text fallback) ---
  if (!rano_recist) {
    if (is_pet) {
      # PET, NO RECIST
      if (effective_strategy == "single_shot") {
        # --- FIX: Call internal function directly ---
        free <- extract_treatment_response_pet_v2(
          text = text,
          model = model,
          model_options = model_options,
          base_url = base_url,
          api_key = api_key
        )
        path_str <- "IN_SCOPE->PET->RECIST_FALSE->SINGLE_SHOT"
      } else {
        # --- FIX: Call internal function directly ---
        free <- extract_treatment_response_pet(
          text = text,
          model = model,
          model_options = model_options,
          base_url = base_url,
          api_key = api_key
        ) # The original chained fn
        path_str <- "IN_SCOPE->PET->RECIST_FALSE->CHAINED"
      }
      return(list(
        response_to_trt_pet = free$response_to_trt_pet,
        no_tumor = free$no_tumor,
        path = path_str
      ))
    } else {
      # NON-PET, NO RECIST
      if (effective_strategy == "single_shot") {
        # --- FIX: Call internal function directly ---
        free <- extract_treatment_response_non_pet_v2(
          text = text,
          model = model,
          model_options = model_options,
          base_url = base_url,
          api_key = api_key
        )
        path_str <- "IN_SCOPE->NON_PET->RECIST_FALSE->SINGLE_SHOT"
      } else {
        # --- FIX: Call internal function directly ---
        free <- extract_treatment_response_non_pet(
          text = text,
          model = model,
          model_options = model_options,
          base_url = base_url,
          api_key = api_key
        ) # The original chained fn
        path_str <- "IN_SCOPE->NON_PET->RECIST_FALSE->CHAINED"
      }
      return(list(
        response_to_trt = free$response_to_trt,
        no_tumor = free$no_tumor,
        path = path_str
      ))
    }
  }

  # --- PATH B: RECIST_TRUE ---
  if (rano_recist) {
    if (is_pet) {
      # PET, RECIST_TRUE
      if (effective_strategy == "single_shot") {
        # --- FIX: Call internal function directly ---
        mapped <- extract_treatment_response_pet_recist(
          text = text,
          model = model,
          model_options = model_options,
          base_url = base_url,
          api_key = api_key
        )
        path_str <- "IN_SCOPE->PET->RECIST_TRUE->SINGLE_SHOT"
      } else {
        # --- FIX: Call internal function directly ---
        mapped <- .run_recist_chain_pet(
          text = text,
          model = model,
          model_options = model_options,
          base_url = base_url,
          api_key = api_key
        ) # Our new chained helper
        path_str <- "IN_SCOPE->PET->RECIST_TRUE->CHAINED"
      }
      return(list(
        response_to_trt_pet = mapped$response_to_trt_pet,
        no_tumor = mapped$no_tumor,
        path = path_str
      ))
    } else {
      # NON-PET, RECIST_TRUE
      if (effective_strategy == "single_shot") {
        # --- FIX: Call internal function directly ---
        mapped <- extract_treatment_response_non_pet_recist(
          text = text,
          model = model,
          model_options = model_options,
          base_url = base_url,
          api_key = api_key
        )
        path_str <- "IN_SCOPE->NON_PET->RECIST_TRUE->SINGLE_SHOT"
      } else {
        # --- FIX: Call internal function directly ---
        mapped <- .run_recist_chain_non_pet(
          text = text,
          model = model,
          model_options = model_options,
          base_url = base_url,
          api_key = api_key
        ) # Our new chained helper
        path_str <- "IN_SCOPE->NON_PET->RECIST_TRUE->CHAINED"
      }
      return(list(
        response_to_trt = mapped$response_to_trt,
        no_tumor = mapped$no_tumor,
        path = path_str
      ))
    }
  }
}
