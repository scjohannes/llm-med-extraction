#' Determine if Text Contains a Cancer Diagnosis
#'
#' Internal function to process diagnosis text and determine if it contains
#' a cancer diagnosis using a large language model.
#'
#' @param text Character string containing the diagnosis text
#' @param model Specifies the LLM to use for the classification. Default: "llama3.3:70b-instruct-q5_K_M"
#' @param model_options List of parameters to pass to the model (default: list(num_ctx = 10000, temperature = 0))
#' @param base_url Specifies the url which points to the ollama instance.
#' @return Boolean: TRUE if cancer diagnosis, FALSE if not
#' @export
#' @importFrom ellmer chat_ollama chat_openai type_object type_boolean
#'

text <- "Anamnese Sturz Hand li und Hüfte  li Fragestellung Fraktur Beckenübersicht und Hüftgelenk links vom 18.01.2023 Handgelenk links vom 18.01.2023 Hand links vom 18.01.2023 Befund und Beurteilung Keine Vorbefunde. Beckenübersicht und Hüfte: Keine Frakturen. Artikulationen intakt. ISG Arthrose beidseits. Handgelenk und Hand links: Fraktur des Processus styloideus ulnaris mit Gelenkbeteiligung. Kleine Fragmente im Frakturspalt. Distale Radiusfraktur des Processus styloideus radii mit Gelenkbeteiligung. Proximale Gelenkflächen des Radiokarpalgelenks und des Ulnokarpalgelenks nicht intakt. Schrägfraktur des proximalen MC V mit geringer Einstauchung. Mutmasslich Intraartikuläre und mehrfragmentierte Fraktur."

.DEFAULT_MODEL = "llama3.3:70b-instruct-q5_K_M"

.DEFAULT_MODEL_OPTIONS =
  list(
    temperature = 0,
    num_ctx = 10000
  )

.DEFAULT_BASE_URL = "http://rndapollolp01.uhbs.ch:11434"

is_report_relevant <-
  function(
    text,
    model = .DEFAULT_MODEL,
    model_options = .DEFAULT_MODEL_OPTIONS
  ) {
    type_report_relevant_check <-
      ellmer::type_object(
        is_relevant = ellmer::type_boolean(
          "Image report relevant for evaluation of distal radius fractures?"
        )
      )
    
    question <- "
  Consider you are a medical expert in reading imaging reports written in German.
  
  Please, indicate whether the imaging report is relevant for the assessment of the diagnosis of a distal radius fracture OR for the evaluation of the outcome of an open reduction internal fixation procedure. 
  
  If you think the imaging report can be used, select TRUE, otherwise select FALSE.
  
  The imaging report starts below:
    
    "

    #   if (base_url == Sys.getenv("usb_ollama_api_2")) {
    #     chat <- ellmer::chat_openai(
    #     system_prompt = question,
    #     base_url = base_url,
    #     model = model,
    #     api_args = model_options,
    #     api_key = Sys.getenv("OLLAMA_API_KEY"),
    #   )
    # } else {
    # Initialize chat with the model
    chat <- ellmer::chat_ollama(
      system_prompt = "You are a highly trained medical AI assistant specialized in reading imaging reports in orthopedics and traumatology.",
      base_url = .DEFAULT_BASE_URL,
      model = model,
      api_args = model_options
    )
    # }

    prompt <- paste0(question, text)
    
    # Extract and return structured data
    tryCatch(
      {
        result <- chat$chat_structured(
          prompt,
          type = type_report_relevant_check
        )
        return(result$is_relevant)
      },
      error = function(e) {
        return(e$message)
      }
    )
  }

is_report_relevant(text)

is_cancer_diagnosis <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = .DEFAULT_BASE_URL
) {
  # Create the type definition for the structured output
  type_cancer_check <- ellmer::type_object(
    is_cancer = ellmer::type_boolean("Is the diagnosis is cancer-related?")
  )

  question <- "
  You are a medical expert specialized in oncology and experienced in analyzing German medical records.
  You will receive text on a single diagnosis of a patient in German.
  Your task is to determine whether the diagnosis is cancer-related or not.

  If the diagnosis contains any mention of cancer, malignancy, carcinoma, sarcoma,
  lymphoma, leukemia, or any other oncological condition, classify it as a cancer diagnosis.

  Respond with:
  - TRUE if the text contains a cancer diagnosis
  - FALSE if the text does not contain a cancer diagnosis or is unclear

  Be conservative in your assessment - only classify clear cancer diagnoses as TRUE.
  "

  if (base_url == Sys.getenv("usb_ollama_api_2")) {
    chat <- ellmer::chat_openai(
      system_prompt = question,
      base_url = base_url,
      model = model,
      api_args = model_options,
      api_key = Sys.getenv("OLLAMA_API_KEY"),
    )
  } else {
    # Initialize chat with the model
    chat <- ellmer::chat_ollama(
      system_prompt = question,
      base_url = base_url,
      model = model,
      api_args = model_options
    )
  }

  # Extract and return structured data
  tryCatch(
    {
      result <- chat$chat_structured(text, type = type_cancer_check)
      return(result$is_cancer)
    },
    error = function(e) {
      return(e$message)
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
  base_url = .DEFAULT_BASE_URL,
  #api_key = Sys.getenv("OLLAMA_API_KEY"),
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
    "No Malignant Disease"
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
      paste0(
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

    Always provide the best matching category from the list only and do not provide explanations. # Added from Python

    The diagnosis text is below:
    {text_placeholder}
  "
  # Use glue to insert the options and the actual text
  current_prompt <- glue::glue(
    question_template,
    diag_options_placeholder = diagnosis_options_string,
    text_placeholder = text,
    .open = "{",
    .close = "}" # Standard glue delimiters
  )

  if (base_url == Sys.getenv("usb_ollama_api_2")) {
    chat <- ellmer::chat_openai(
      system_prompt = "You are a highly trained medical AI assistant helping users to match free text diagnoses to a list of diagnose categories provided by the user.",
      base_url = base_url,
      api_key = Sys.getenv("OLLAMA_API_KEY"),
      model = model,
      api_args = model_options
    )
  } else {
    chat <- ellmer::chat_ollama(
      system_prompt = "You are a highly trained medical AI assistant helping users to match free text diagnoses to a list of diagnose categories provided by the user.",
      base_url = base_url,
      #api_key = Sys.getenv("OLLAMA_API_KEY"),
      model = model,
      api_args = model_options
    )
  }

  # Extract and return structured data
  tryCatch(
    {
      extracted_data <- chat$chat_structured(
        current_prompt,
        type = diagnosis_type
      )
      extracted_data$model <- model
      extracted_data$prompt <- question_template
      return(extracted_data)
    },
    error = function(e) {
      # Return NA for diagnosis in case of an error
      list(
        diagnosis = e$message,
        model = model,
        prompt = question_template
      )
    }
  )
}
