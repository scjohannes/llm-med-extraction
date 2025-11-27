#' Extract the primary location of a tumor from an imaging report text ad
#' verbatim.
#'
#' Function to process imaging report text and extract the primary tumor
#' location using a large language model.
#'
#' @param text Character string containing the imaging report text.
#' @param model Specifies the LLM to use for the data extraction.
#' @param base_url Specific the url of ollama instance.
#' @param api_key API key for authentication.
#' @param model_options List of parameters to pass to the model (default: list(num_ctx = 10000, temperature = 0))
#' @return A list containing the extracted diagnosis information.
#' @export
#' @importFrom ellmer chat_openai type_object
#' @importFrom glue glue
#'
extract_primary_location <- function(
  text,
  model = .DEFAULT_MODEL,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY"),
  model_options = list(num_ctx = 10000, temperature = 0)
) {
  question_template <- "
    Please extract the first oncology diagnosis from the <Anamnese> section of the following german radiology report ad verbatim. Don't extract anything else.
    The imaging report text is below this line:
    -------------------------------------------
    {text_placeholder}
  "
  current_prompt <- glue::glue(
    question_template,
    text_placeholder = text,
    .open = "{",
    .close = "}"
  )

  system_prompt <- "You are a highly trained medical AI assistant."

  tryCatch(
    {
      # Step 1: Initialize chat and get free text
      chat <- .init_chat(
        system_prompt = system_prompt,
        model = model,
        base_url = base_url,
        api_key = api_key,
        model_options = model_options
      )

      if (is.null(chat)) {
        stop("Failed to initialize chat for v2 location extraction.")
      }

      response_text <- chat$chat(current_prompt)

      # Step 2: Classify the free text
      category <- extract_hlt_cancer_diagnosis(
        response_text,
        model = model,
        base_url = base_url,
        api_key = api_key,
        model_options = model_options
      )

      if (is.na(category$diagnosis)) {
        stop("Failed to classify extracted diagnosis text.")
      }

      extracted_data <- list(
        primary_tumor = category$diagnosis,
        model = model,
        prompt = paste(
          "First prompt: ",
          question_template,
          "Second Prompt: ",
          category$prompt
        )
      )
      return(extracted_data)
    },
    error = function(e) {
      # Return NA for diagnosis in case of an error
      list(
        primary_tumor = e$message,
        model = model,
        prompt = question_template
      )
    }
  )
}


#' Extract high-level term, vague, Cancer Diagnosis Information from Medical Text
#'
#' Function to process medical text and extract the primary diagnosis
#' using a large language model.
#'
#' @param text Character string containing the medical text (e.g., EHR free text diagnosis)
#' @param model Specifies the LLM to use for the data extraction.
#' @param base_url Specific the url of ollama instance.
#' @param model_options List of parameters to pass to the model (default: list(num_ctx = 10000, temperature = 0))
#' @return A list containing the extracted diagnosis information
#' @export
#' @importFrom ellmer chat_ollama chat_openai type_object type_enum
#' @importFrom glue glue
#'
extract_hlt_cancer_diagnosis <- function(
  text,
  model = .DEFAULT_MODEL,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY"),
  model_options = list(num_ctx = 2048, temperature = 0)
) {
  diagnosis_options <- c(
    "Primary Brain Tumor",
    "Head and Neck Tumors",
    "Thyroid Cancer",
    "Lung Tumor (including Mesothelioma)",
    "Breast Cancer",
    "Gastric Cancer",
    "Esophageal Cancer",
    "Pancreatic Cancer",
    "Cholangiocarcinoma",
    "Hepatocellular Carcinoma",
    "Small Bowel Cancer",
    "Colorectal Cancer",
    "Kidney Tumor",
    "Cancer of the Urinary Tract (including Ureter and Bladder)",
    "Prostate Cancer",
    "Gynecological Tumors",
    "Primary Skin Tumor (including Melanoma and Non-Melanoma)",
    "Sarcoma",
    "Lymphoma",
    "Multiple Myeloma",
    "Leukemia",
    "Neuroendocrine Tumors",
    "Testicular Cancer",
    "Cancer of Unkown Primary",
    "Other",
    "Unclear",
    "No Malignant Disease",
    "Not Applicable"
  )

  # Create a comma-separated string of quoted options for the prompt
  diagnosis_options_string <- paste0(
    "'",
    diagnosis_options,
    "'",
    collapse = ", "
  )

  diagnosis_type <- ellmer::type_object(
    diagnosis = ellmer::type_enum(
      description = paste0(
        "Under which diagnosis category does the diagnosis from the text fall? Must be one of: ",
        diagnosis_options_string
      ),
      diagnosis_options
    )
  )

  question_template <- "
    You are a highly trained medical AI assistant.
    Your task is to read the provided medical text, which is a free text diagnosis from an Electronic Health Record (EHR).
    The input text will most likely be in German. Identify the primary diagnosis from the text.

    Assign the free text diagnosis to the best matching diagnosis category from the following list: [{diag_options_placeholder}].

    Choose 'Unclear' if the diagnosis is uncertain or not clearly stated.
    Choose 'No Malignant Disease' if the text indicates no malignancy.
    Prioritize the most specific diagnosis mentioned in the text.
    If the diagnosis is not clear and multiple differential diagnoses are listed (often abbrevaited with 'DD') choose the first differential diagnosis listed.

    Always provide the best matching category from the list only and do not provide explanations.

    The diagnosis text is below:
    {text_placeholder}
  "
  # Use glue to insert the options and the actual text
  current_prompt <- glue::glue(
    question_template,
    diag_options_placeholder = diagnosis_options_string,
    text_placeholder = text,
    .open = "{",
    .close = "}"
  )

  system_prompt <- "You are a highly trained medical AI assistant helping users to match free text diagnoses to a list of diagnose categories provided by the user."

  # --- REFACTORED PART ---
  # Single call to the robust helper
  extracted_data <- .call_ellmer_structured(
    system_prompt = system_prompt,
    user_prompt = current_prompt,
    type_definition = diagnosis_type,
    model = model,
    base_url = base_url,
    api_key = api_key,
    model_options = model_options
  )

  return(extracted_data)
}
