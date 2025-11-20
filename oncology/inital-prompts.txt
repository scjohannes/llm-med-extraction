# Note: These prompts are used inside an internal R package and might not be
# functional as standalone code.

# # METASTASIS EXTRACTION -------------------------------------------------------
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
#' @return A list containing extracted metastasis information, model name, and prompt,
#'   or the `error_return_list` with model/prompt info on error.
.extract_metastasis_base <- function(
  text,
  type_definition,
  system_prompt_template,
  region_name,
  metastasis_questions,
  error_return_list,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = .DEFAULT_BASE_URL
) {
  # 1. Construct the prompt
  # Ensure glue is available
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("Package 'glue' is required but not installed. Please install it.")
  }
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
      if (base_url == .OLLAMA_API_OPENAI_COMPATIBLE) {
        chat <- ellmer::chat_openai(
          system_prompt = "You are a highly trained medical AI assistant helping users to extract the location of metastases from radiology reports.",
          base_url = base_url,
          api_key = Sys.getenv("OLLAMA_API_KEY"),
          model = model,
          api_args = model_options
        )
      } else {
        chat <- ellmer::chat_ollama(
          system_prompt = "You are a highly trained medical AI assistant helping users to extract the location of metastases from radiology reports.",
          base_url = base_url,
          #api_key = Sys.getenv("OLLAMA_API_KEY"),
          model = model,
          api_args = model_options
        )
      }
    },
    error = function(e_init) {
      warning(glue::glue(
        "Failed to initialize ellmer::chat_ollama for model '{model}' at '{base_url}': {e_init$message}"
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

  return(extracted_data)
}

# --- Common System Prompt Template ---

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

**Distinguishing Primary Tumor vs. Metastasis:**
* Progression, recurrence, or residual disease *of the known primary tumor* at its original site is NOT metastasis for the purpose of this task.
* A hypermetabolic focus at the site of the known primary tumor should generally be considered related to the primary, UNLESS the report explicitly suggests it represents a *metastatic deposit* separate from the main primary mass or indicates metastatic spread *within the same organ* but distinct from the primary focus.
* If the radiologist describes *only* the primary tumor location and its characteristics (even if hypermetabolic), do NOT label this as a metastatic location.

Keep in mind that progression of a primary tumor in this region is NOT a metastasis.
The lesion of the primary tumour is NOT a metastasis. Pay attention to whether the radiologist labels the lesion as a metastasis or as the primary tumour.
Do not consider any information about metastasis, which is from the case history ('Anamnese'). E.g., it might be mentioned that the patient has a metastasis of an organ that is not part of the current imaging exam. Do NOT consider this information. Only focus on what is seen in the current imaging exam.

If the radiologist describes the location of the primary tumour only, do NOT use this as a location for metastasis, unless the patient has metastasis in the same location as the primary tumour.
Provide a justification for your answers based *only* on the provided text.

You can find the report text below:


"

# --- Fully Standardized Questions ---
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


# --- Exported Functions ---

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
  base_url = .DEFAULT_BASE_URL
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
  error_list <- list(
    cns_metastasis = NA,
    bone_metastasis = NA,
    meningeal_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "head/brain",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    cns_metastasis = NA,
    meningeal_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "head and neck",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    liver_metastasis = NA,
    adrenal_metastasis = NA,
    kidney_metastasis = NA,
    spleen_metastasis = NA,
    pancreas_metastasis = NA,
    peritoneal_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "abdomen",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    liver_metastasis = NA,
    adrenal_metastasis = NA,
    kidney_metastasis = NA,
    spleen_metastasis = NA,
    pancreas_metastasis = NA,
    peritoneal_metastasis = NA,
    ovarian_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "abdomen and pelvis",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    liver_metastasis = NA,
    adrenal_metastasis = NA,
    kidney_metastasis = NA,
    spleen_metastasis = NA,
    pancreas_metastasis = NA,
    peritoneal_metastasis = NA,
    ovarian_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "abdomen, pelvis and lower extremities",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "lower extremities",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "neck",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    lung_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "neck and thorax",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    lung_metastasis = NA,
    liver_metastasis = NA,
    adrenal_metastasis = NA,
    kidney_metastasis = NA,
    spleen_metastasis = NA,
    pancreas_metastasis = NA,
    peritoneal_metastasis = NA,
    ovarian_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "neck, thorax, abdomen and pelvis",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    other_organ_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "various or unspecified body", # Generic region name
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    peritoneal_metastasis = NA,
    ovarian_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "pelvis",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    peritoneal_metastasis = NA,
    ovarian_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "pelvis and lower extremities",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    bone_metastasis = NA,
    lymph_node_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "spine",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    bone_metastasis = NA,
    lymph_node_metastasis = NA,
    soft_tissue_metastasis = NA,
    ovarian_metastasis = NA,
    peritoneal_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "spine and pelvis",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    lung_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "thorax",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    lung_metastasis = NA,
    liver_metastasis = NA,
    adrenal_metastasis = NA,
    kidney_metastasis = NA,
    spleen_metastasis = NA,
    pancreas_metastasis = NA,
    peritoneal_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "thorax and abdomen",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    lung_metastasis = NA,
    liver_metastasis = NA,
    adrenal_metastasis = NA,
    kidney_metastasis = NA,
    spleen_metastasis = NA,
    pancreas_metastasis = NA,
    peritoneal_metastasis = NA,
    ovarian_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "thorax, abdomen and pelvis",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "upper extremities",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
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
  base_url = .DEFAULT_BASE_URL
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
  # Define the structure for error return
  error_list <- list(
    cns_metastasis = NA,
    meningeal_metastasis = NA,
    lung_metastasis = NA,
    liver_metastasis = NA,
    adrenal_metastasis = NA,
    kidney_metastasis = NA,
    spleen_metastasis = NA,
    pancreas_metastasis = NA,
    peritoneal_metastasis = NA,
    ovarian_metastasis = NA,
    lymph_node_metastasis = NA,
    bone_metastasis = NA,
    soft_tissue_metastasis = NA,
    justification = NA
  )

  # Call the base function
  .extract_metastasis_base(
    text = text,
    type_definition = type_def,
    system_prompt_template = .SYSTEM_PROMPT_TEMPLATE,
    region_name = "whole body",
    metastasis_questions = questions,
    error_return_list = error_list,
    model = model,
    model_options = model_options,
    base_url = base_url
  )
}

# # RESPONSE TO TREATMENT CLASSIFICATION --------------------------------------

#' @title Classify PET-CT Cancer Treatment Response via LLM
#'
#' @description
#' This function interfaces with a Large Language Model (LLM) to analyze the
#' text of a German PET-CT radiology report and classify the patient's cancer
#' treatment response. The classification strictly follows predefined criteria
#' based on changes in metabolic activity (FDG uptake), as detailed in the
#' provided system prompt. It utilizes the `ellmer` package for LLM communication.
#'
#' @details
#' The function's core classification logic is driven by a prompt. Metabolic
#' findings (PET) are prioritized over anatomical findings (CT).
#'
#' The function handles the construction of the API request, sends it to an
#' Ollama compatible LLM service, and parses the structured response
#' according to a predefined `type_definition`. It includes error handling for
#' API initialization and data extraction failures.
#'
#' @param text Character string. The full text of the German PET-CT report.
#' @param model Character string. The identifier for the LLM model to be used.
#' Defaults to the value of `.DEFAULT_MODEL`.
#' @param model_options List. Arguments specific to the LLM API
#'   (e.g., temperature, num_ctx, passed as `api_args`). Defaults to the
#'   value of `.DEFAULT_MODEL_OPTIONS`.
#' @param base_url Character string. The base URL for the LLM API service.
#'   Defaults to the value of `.DEFAULT_BASE_URL`.
#'
#' @return A list.
#'   \strong{On success:}
#'   \itemize{
#'     \item \code{response_to_trt}: Character string. The classified treatment response.
#'       One of: "COMPLETE_RESPONSE", "PARTIAL_RESPONSE", "STABLE_DISEASE",
#'       "PROGRESSION", "NOT_APPLICABLE".
#'     \item \code{justification}: Character string. The LLM's justification for the
#'       classification.
#'     \item \code{model}: Character string. The name of the model used.
#'     \item \code{prompt}: Character string. The full system prompt sent to the model.
#'   }
#'   \strong{On failure} (e.g., API connection or data extraction error):
#'   \itemize{
#'     \item \code{response_to_trt}: \code{NA_character_}.
#'     \item \code{model}: Character string. The name of the model that was attempted.
#'     \item \code{prompt}: Character string. The system prompt that was attempted.
#'     (The \code{justification} field will be absent in this case.)
#'   }
#'   Appropriate warnings are issued by the function in case of errors.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure the `ellmer` package and its dependencies (like `glue`) are installed.
#' # Environment variables (e.g., OLLAMA_API_KEY for certain base_urls) may need to be set.
#'
#' report_text_example <- paste(
#'   "Klinische Angaben: Zervixkarzinom unter Therapie.",
#'   "Vergleich: Voruntersuchung vom 01.01.2023.",
#'   "Befund: Komplette metabolische Remission aller zuvor",
#'   "stoffwechselaktiven Läsionen. Kein Nachweis pathologischer",
#'   "Mehranreicherung mehr. Beurteilung: Komplette metabolische Remission."
#' )
#'
#' result <- extract_treatment_response_pet(
#'   text = report_text_example
#' )
#' }
extract_treatment_response_pet <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = .DEFAULT_BASE_URL
) {
  instructions <- "
**FIRST TASK:** Analyze the provided German PET-CT report text and classify the patient's cancer status based *strictly* on described changes in metabolic activity (FDG uptake). 
Ignore changes in size (CT findings) unless metabolic information is entirely absent.

**INPUT:** Full text of a German PET-CT report.

**OUTPUT:** Classify the report into ONE of the following categories:
1.  `COMPLETE_RESPONSE`
2.  `PARTIAL_RESPONSE`
3.  `STABLE_DISEASE`
4.  `PROGRESSION`
5.  `NOT_APPLICABLE`

**CLASSIFICATION CRITERIA (Focus ONLY on Metabolic Activity):**

1.  **`COMPLETE_RESPONSE` (CR - Komplette Metabolische Remission)**:
    * **General:** Report indicates *complete resolution* of abnormal metabolic activity in all known lesions, down to background levels. No FDG-avid malignant lesions remain.
    * **Solid Tumors:** Look for terms explicitly stating complete metabolic response (e.g., '**komplette metabolische Remission**,' '**kein pathologischer Hypermetabolismus mehr nachweisbar**,' 'vollständige Normalisierung des Stoffwechsels'). This also includes COMPLETE surgical resection of the tumour and, if existing, its metastases.
    * **Lymphoma/Blood Cancer:** Look for **'Deauville Score 1,' 'Deauville Score 2,' or 'Deauville Score 3'** as the final assessment for *all* originally involved sites, AND absence of new FDG-avid lesions. Terms like '**komplette metabolische Remission**.'
    * *Note:* Residual non-avid findings on CT do not preclude CR.

2.  **`PARTIAL_RESPONSE` (PR - Partielle Metabolische Remission)**:
    * **General:** Report indicates a *significant decrease* in metabolic activity (FDG uptake/SUV) in known lesions compared to the prior scan, but some residual abnormal uptake persists (above background levels).
    * **Solid Tumors:** Look for terms indicating partial metabolic response (e.g., '**partielle metabolische Remission**,' '**deutliche Abnahme/Reduktion der Stoffwechselaktivität/des SUV**,' 'deutliche Regredienz des Hypermetabolismus,' but *not* complete resolution). This also includes INcomplete surgical resection of the tumour or its metastases.
    * **Lymphoma/Blood Cancer:** May be indicated by a shift to a lower **Deauville Score (e.g., from 5 to 4)**, or descriptions like '**deutliche Abnahme der metabolischen Aktivität**' but uptake still clearly above background/liver (i.e., still DS 4 or 5 but improved).  *Use this category if response is clear but doesn't meet CMR criteria (DS 1-3).*
    * *Note:* Significant metabolic decrease defines PMR, even if size is stable or slightly increased.

3.  **`STABLE_DISEASE` (SD - Stabile Metabolische Erkrankung)**:
    * **General:** Report indicates no significant *change* (neither significant decrease nor increase) in metabolic activity (FDG uptake/SUV values) in known lesions compared to the prior scan.
    * **Solid Tumors:** Look for terms like '**stabile metabolische Aktivität**,' 'unveränderter SUV,' '**keine signifikante Änderung des Hypermetabolismus**,' 'stabiler Befund.'
    * **Lymphoma/Blood Cancer:** Often indicated by a persistent **'Deauville Score 4' or 'Deauville Score 5'** *without* mention of significant change compared to prior or new lesions. Look for terms like '**persistierende metabolische Aktivität**,' '**stabile Erkrankung**.'

4.  **`PROGRESSION` (PD - Progrediente Metabolische Erkrankung)**:
    * **General:** Report indicates a significant *increase* in metabolic activity (FDG uptake/SUV) in existing lesions OR, critically, the appearance of **new FDG-avid lesions** consistent with malignancy.
    * **Solid Tumors:** Look for terms indicating metabolic progression (e.g., '**metabolische Progression**,' '**Zunahme der Stoffwechselaktivität/des SUV**,' 'progredienter Hypermetabolismus'). **Crucially, look for 'neue Herde,' 'neue Läsionen,' 'neue stoffwechselaktive Läsionen.'**
    * **Lymphoma/Blood Cancer:** Look for **'Deauville Score 4' or 'Deauville Score 5'** *with* context indicating worsening compared to prior scan, OR **any mention of 'neue FDG-avide Läsionen,' 'neue Herde'** (often implicitly DS 5). Also terms like '**Progression**,' 'progrediente Erkrankung.'
    * *Note:* New metabolically active lesions automatically mean `PROGRESSION`. Increased metabolic activity in existing lesions also means `PROGRESSION`, even if size decreases.

5.  **`NOT_APPLICABLE`**:
    * **Indication:** The scan is explicitly identified as a baseline/staging scan OR the clinical context/findings clearly indicate the scan was performed for reasons unrelated to treatment response assessment.
    * **Look For:** Terms like **'Baseline,' 'Staging,' 'Erststaging,' 'Ausgangsbefund,' 'Primärdiagnostik.'** Also look for clinical indications (Fragestellung) or findings sections focused entirely on other issues (e.g., 'Ausschluss Infekt,' 'Entzündungssuche') without reference to tumor response. Absence of a relevant comparison scan for response assessment.

**PRIORITIZATION:**
* Metabolic findings (PET) override anatomical findings (CT).
* The 'Beurteilung' (Impression/Conclusion) section often contains the final summary, but verify against the 'Befund' (Findings) and 'Vergleich' (Comparison) sections.
* The presence of new, metabolically active lesions consistent with malignancy is the strongest indicator for `PROGRESSION`.
* Explicit mention of Deauville Scores (for lymphoma) is a primary classification driver. Distinguish carefully between DS 1-3 (CMR), and DS 4/5 (can be PMR, SMD, or PMD depending on comparison/new lesions).

**Second TASK:**

If you have chosen `COMPLETE_RESPONSE` or `NOT_APPLICABLE` you are also asked to indicate whether the patient is completely tumour free, in other words, there is not evidence of any tumour ('Kein Tumornachweis').
If you have chosen `PARTIAL_RESPONSE` or `STABLE_DISEASE` or `PROGRESSION` always answer with false.

**INPUT:** Full text of a German PET-CT report.

**OUTPUT:** Classify the report into ONE of the following categories:

- There is no evidence of a tumour according to this imaging report. TRUE / FALSE


Below this line starts the radiology report:
  ---------------------------------------
"

  text <- paste(instructions, text)

  type_definition <- ellmer::type_object(
    response_to_trt_pet = ellmer::type_enum(
      "Classify the report into ONE of the following categories. Must be one of ['COMPLETE_RESPONSE', 'PARTIAL_RESPONSE', 'STABLE_DISEASE', 'PROGRESSION', 'NOT_APPLICABLE']",
      c(
        "COMPLETE_RESPONSE",
        "PARTIAL_RESPONSE",
        "STABLE_DISEASE",
        "PROGRESSION",
        "NOT_APPLICABLE"
      )
    ),
    no_tumour = ellmer::type_boolean(
      "There is no evidence for a tumour according to the report. The patient is completely tumour free."
    ),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  error_return_list <- list(
    response_to_trt = NA
  )

  chat <- tryCatch(
    if (base_url == .OLLAMA_API_OPENAI_COMPATIBLE) {
      chat <- ellmer::chat_openai(
        system_prompt = "You are a highly trained medical AI assistant helping users to extract information on treatment response from radiology reports.",
        base_url = base_url,
        api_key = Sys.getenv("OLLAMA_API_KEY"),
        model = model,
        api_args = model_options
      )
    } else {
      chat <- ellmer::chat_ollama(
        system_prompt = "You are a highly trained medical AI assistant helping users to extract information on treatment response from radiology reports.",
        base_url = base_url,
        model = model,
        api_args = model_options
      )
    },
    error = function(e_init) {
      warning(glue::glue(
        "Failed to initialize ellmer::chat_ollama for model '{model}' at '{base_url}': {e_init$message}"
      ))
      return(NULL)
    }
  )

  if (is.null(chat)) {
    # Add model and prompt info to the error list before returning
    error_return_list$model <- model
    error_return_list$prompt <- instructions
    return(error_return_list)
  }

  # 3. Extract and return structured data
  extracted_data <- tryCatch(
    {
      result <- chat$chat_structured(text, type = type_definition)
      result$model <- model
      result$prompt <- instructions
      result
    },
    error = function(e_extract) {
      warning(glue::glue(
        "Error during API call or data extraction for model '{model}': {e_extract$message}"
      ))
      # Add model and prompt info to the error list
      error_return_list$model <- model
      error_return_list$prompt <- instructions
      error_return_list # Return the predefined NA list structure
    }
  )

  return(extracted_data)
}

#' @title Classify CT/MRI Cancer Treatment Response via LLM
#'
#' @description
#' This function interfaces with a Large Language Model (LLM) to analyze the
#' text of a German CT or MRI radiology report and classify the patient's cancer
#' treatment response. The classification strictly follows predefined criteria
#' based on anatomical/morphological changes (e.g., lesion size, number),
#' as detailed in an internal system prompt. It utilizes the `ellmer` package
#' for LLM communication.
#'
#' @details
#' The function's core classification logic is driven by an internal `instructions`
#' string, which serves as the detailed system prompt. This prompt guides the LLM
#' to classify reports based on anatomical findings into one of four categories:
#' "RESPONSE", "STABLE_DISEASE", "PROGRESSION", or "NOT_APPLICABLE".
#'
#' @param text Character string. The full text of the German CT or MRI report.
#' @param model Character string. The identifier for the LLM model to be used.
#'   Defaults to the value of `.DEFAULT_MODEL`.
#' @param model_options List. Arguments specific to the LLM API
#'   (e.g., temperature, num_ctx, passed as `api_args`). Defaults to the
#'   value of `.DEFAULT_MODEL_OPTIONS`.
#' @param base_url Character string. The base URL for the LLM API service.
#'   Defaults to the value of `.DEFAULT_BASE_URL`.
#'
#' @return A list.
#'   \strong{On success:}
#'   \itemize{
#'     \item \code{response_to_trt}: Character string. The classified treatment response.
#'       One of: "RESPONSE", "STABLE_DISEASE", "PROGRESSION", "NOT_APPLICABLE".
#'     \item \code{justification}: Character string. The LLM's justification for the
#'       classification.
#'     \item \code{model}: Character string. The name of the model used.
#'     \item \code{prompt}: Character string. The full internal `instructions` string
#'       (system prompt for classification) that was sent to the model.
#'       (Assumes the function code sets this to the internal `instructions` variable).
#'   }
#'   \strong{On failure} (e.g., API connection or data extraction error):
#'   \itemize{
#'     \item \code{response_to_trt}: \code{NA_character_}.
#'     \item \code{model}: Character string. The name of the model that was attempted.
#'     \item \code{prompt}: Character string. The internal `instructions` string
#'       (system prompt for classification) that was attempted.
#'       (Assumes the function code sets this to the internal `instructions` variable;
#'       the \code{justification} field will be absent in this case.)
#'   }
#'   Appropriate warnings are issued by the function in case of errors.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure the `ellmer` package and its dependencies (like `glue`) are installed.
#' # Ensure .DEFAULT_MODEL, .DEFAULT_MODEL_OPTIONS, .DEFAULT_BASE_URL are defined,
#' # or pass appropriate values as arguments.
#' # Environment variables (e.g., OLLAMA_API_KEY for certain base_urls) may need to be set.
#'
#' report_text_example_ct <- paste(
#'   "Klinische Angaben: Kolonkarzinom unter Chemotherapie.",
#'   "Vergleich: CT Thorax/Abdomen vom 02.02.2024.",
#'   "Befund: Signifikante Größenregredienz der bekannten Lebermetastasen.",
#'   "Keine neuen suspekten Läsionen nachweisbar.",
#'   "Beurteilung: Gutes partielles Ansprechen auf die Therapie."
#' )
#'
#' result <- extract_treatment_response_non_pet(
#'   text = report_text_example_ct
#' )
#' }
extract_treatment_response_non_pet <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = .DEFAULT_BASE_URL
) {
  instructions <- "
**TASK:** Analyze the provided German CT or MRI report text and classify the patient's cancer status based *strictly* on described anatomical/morphological changes in lesions (size, number, characteristics).

**INPUT:** Full text of a German CT or MRI report.

**OUTPUT:** Classify the report into ONE of the following categories:
1.  `RESPONSE`
2.  `STABLE_DISEASE`
3.  `PROGRESSION`
4.  `NOT_APPLICABLE`

**CLASSIFICATION CRITERIA (Focus ONLY on Anatomical/Morphological Changes):**

1.  **`RESPONSE` (R - Ansprechen / Remission)**:
    * **General:** Report indicates either *complete disappearance* of all known/target lesions OR a *significant decrease* in the size (e.g., diameter, volume) of known/target lesions compared to the prior scan. No new lesions are present, and no unequivocal progression of non-target lesions. This category covers both complete and substantial partial responses. Surgical resection of the primary tumour or metastases should also be coded as response.
    * **Look For:** Terms explicitly stating complete or partial response, or significant size reduction/disappearance (e.g., '**komplette Remission**,' '**partielle Remission**,' '**deutliche Größenabnahme der Läsionen**,' '**signifikante Regredienz**,' '**vollständige Rückbildung aller Läsionen**,' '**kein Tumornachweis mehr**,' 'Läsionen nicht mehr abgrenzbar/nachweisbar,' 'vollständige narbige Ausheilung,' '**größenregredient**,' 'deutliche Verkleinerung,' 'gutes Ansprechen'). Comparison to a previous scan must confirm disappearance or significant reduction.

2.  **`STABLE_DISEASE` (SD - Stabile Erkrankung)**:
    * **General:** Report indicates no significant *change* (neither sufficient reduction for `RESPONSE` nor sufficient increase/new lesions for `PROGRESSION`) in the size or characteristics of known lesions compared to the prior scan. No new lesions.
    * **Look For:** Terms like '**stabile Erkrankung**,' '**unveränderter Befund**,' '**konstante Läsionsgröße**,' '**keine signifikante Größenänderung**,' 'Befundstabilität,' 'keine wesentliche Dynamik.'

3.  **`PROGRESSION` (PD - Progrediente Erkrankung / Progression)**:
    * **General:** Report indicates a significant *increase* in the size of existing lesions, OR, critically, the appearance of **new lesions** consistent with malignancy, OR unequivocal progression of non-target lesions.
    * **Look For:** Terms indicating progression (e.g., '**progrediente Erkrankung**,' '**Progression**,' '**Größenzunahme der Läsionen**,' '**Befundverschlechterung**,' 'größenprogredient').
    * **Crucially, look for any mention of 'neue Läsionen,' 'neue Herde,' 'neu aufgetretene Metastasen/Filiae,' 'neu abgrenzbare Läsionen.'** The appearance of new lesions typically signifies progression regardless of the behavior of previously known lesions.

4.  **`NOT_APPLICABLE`**:
    * **Indication:** The scan is explicitly identified as a baseline/staging scan OR the clinical context/findings clearly indicate the scan was performed for reasons unrelated to treatment response assessment OR there is no relevant prior scan for comparison.
    * **Look For:** Terms like '**Baseline**,' '**Erststaging**,' '**Ausgangsbefund**,' '**Primärdiagnostik**.' Also, if the 'Fragestellung' (clinical question) or findings are unrelated to tumor response (e.g., 'Trauma-Folgen'). Look for phrases like '**keine Voruntersuchung zum Vergleich**' or 'Erstuntersuchung.'

**PRIORITIZATION & IMPORTANT NOTES:**
* Changes in lesion size and the presence/absence of new lesions are the primary drivers for classification.
* The 'Beurteilung' (Impression/Conclusion) section often summarizes the overall oncological status but verify against the 'Befund' (Findings) and especially the 'Vergleich' (Comparison to previous scan) sections.
* **The appearance of new, unequivocal malignant lesions is the strongest indicator for `PROGRESSION`.**
* If a report describes both decreasing size in some lesions but new lesions elsewhere, this is should be classified as `PROGRESSION`.
* Terms like 'leicht,' 'geringfügig,' 'tendentiell' (slight, minor, tendentially) for size changes might often fall into `STABLE_DISEASE` unless clearly meeting criteria for `RESPONSE` or `PROGRESSION`, or if new lesions are present.
* If specific measurement criteria (like RECIST) are mentioned and interpreted by the radiologist (e.g., 'entsprechend einer partiellen Remission nach RECIST 1.1'), give strong weight to that interpretation falling under `RESPONSE`.

**Second TASK:**

If you have chosen `RESPONSE` or `NOT_APPLICABLE` you are also asked to indicate whether the patient is completely tumour free, in other words, there is not evidence of any tumour ('Kein Tumornachweis').
If you have chosen `STABLE_DISEASE` or `PROGRESSION` always answer with false.
**INPUT:** Full text of a German PET-CT report.

**OUTPUT:** Classify the report into ONE of the following categories:

- There is no evidence of a tumour according to this imaging report. TRUE / FALSE

Below this line starts the radiology report:
  ---------------------------------------

"

  text <- paste(instructions, text)

  type_definition <- ellmer::type_object(
    response_to_trt = ellmer::type_enum(
      "Classify the report into ONE of the following categories. Must be one of ['RESPONSE', 'STABLE_DISEASE', 'PROGRESSION', 'NOT_APPLICABLE']",
      c(
        "RESPONSE",
        "STABLE_DISEASE",
        "PROGRESSION",
        "NOT_APPLICABLE"
      )
    ),
    no_tumour = ellmer::type_boolean(
      "There is no evidence for a tumour according to the report. The patient is completely tumour free."
    ),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  error_return_list <- list(
    response_to_trt = NA
  )

  chat <- tryCatch(
    if (base_url == .OLLAMA_API_OPENAI_COMPATIBLE) {
      chat <- ellmer::chat_openai(
        system_prompt = "You are a highly trained medical AI assistant helping users to extract information on treatment response from radiology reports.",
        base_url = base_url,
        api_key = Sys.getenv("OLLAMA_API_KEY"),
        model = model,
        api_args = model_options
      )
    } else {
      chat <- ellmer::chat_ollama(
        system_prompt = "You are a highly trained medical AI assistant helping users to extract information on treatment response from radiology reports.",
        base_url = base_url,
        model = model,
        api_args = model_options
      )
    },
    error = function(e_init) {
      warning(glue::glue(
        "Failed to initialize ellmer::chat_ollama for model '{model}' at '{base_url}': {e_init$message}"
      ))
      return(NULL)
    }
  )

  if (is.null(chat)) {
    # Add model and prompt info to the error list before returning
    error_return_list$model <- model
    error_return_list$prompt <- instructions
    return(error_return_list)
  }

  # 3. Extract and return structured data
  extracted_data <- tryCatch(
    {
      result <- chat$chat_structured(text, type = type_definition)
      result$model <- model
      result$prompt <- instructions
      result
    },
    error = function(e_extract) {
      warning(glue::glue(
        "Error during API call or data extraction for model '{model}': {e_extract$message}"
      ))
      # Add model and prompt info to the error list
      error_return_list$model <- model
      error_return_list$prompt <- instructions
      error_return_list # Return the predefined NA list structure
    }
  )

  return(extracted_data)
}

# # PRIMARY LOCATION OF THE TUMOR CLASSIFICATION --------------------------------------

#' Extract the primary location of a tumor from an imaging report text.
#'
#' Function to process imaging report text and extract the primary tumor
#' location
#' using a large language model.
#'
#' @param text Character string containing the imaging report text.
#' @param model Specifies the LLM to use for the data extraction.
#' @param base_url Specific the url of ollama instance.
#' @param model_options List of parameters to pass to the model (default: list(num_ctx = 10000, temperature = 0))
#' @return A list containing the extracted diagnosis information
#' @export
#' @importFrom ellmer chat_ollama chat_openai type_object type_enum
#' @importFrom glue glue
#'
extract_primary_location <- function(
  text,
  model = .DEFAULT_MODEL,
  base_url = .DEFAULT_BASE_URL,
  model_options = list(num_ctx = 10000, temperature = 0)
) {
  location_options <- c(
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
  location_options_string <- paste0(
    "'",
    location_options,
    "'",
    collapse = ", "
  )

  primary_tumor_type <- ellmer::type_object(
    primary_tumor = ellmer::type_enum(
      paste0(
        "What is the primary location of the tumor from the radiology report? Must be one of: ",
        location_options_string
      ),
      location_options
    )
  )

  question_template <- "
    You will be given a report of the imaging exam a cancer patient. You are asked to extract the location of the primary tumor of this patient.
    
    * Extract the location (organ) of the primary tumor from the imaging exam report.
    * Often the primary location is directly mentioned in the 'Anamnese' section of the report. The primary location of the tumor and its name in the 'Anamnese' section can be treated as the same thing.
    * It is possible that the primary location of the tumor is unclear or not mentioned, but we know location of metastases. In that case choose 'Unclear'.
    * Do not use the location of metastasis to make a guess about the location of the primary tumor.
    * Choose 'Not Applicable' if the the clinical context/findings clearly indicate the scan was performed for reasons unrelated to the cancer.

    Assign the location from the imaging report to the best matching category from the following list: [{loc_options_placeholder}].

    Always provide the best matching category from the list only and do not provide explanations. # Added from Python

    The imaging report text is below:
    {text_placeholder}
  "
  # Use glue to insert the options and the actual text
  current_prompt <- glue::glue(
    question_template,
    loc_options_placeholder = location_options_string,
    text_placeholder = text,
    .open = "{",
    .close = "}" # Standard glue delimiters
  )

  if (base_url == "https://rndcalcle02.uhbs.ch/ollama/v1") {
    chat <- ellmer::chat_openai(
      system_prompt = "You are a highly trained medical AI assistant.",
      base_url = base_url,
      api_key = Sys.getenv("OLLAMA_API_KEY"),
      model = model,
      api_args = model_options
    )
  } else {
    chat <- ellmer::chat_ollama(
      system_prompt = "You are a highly trained medical AI assistant.",
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
        type = primary_tumor_type
      )
      extracted_data$model <- model
      extracted_data$prompt <- question_template
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

# # TUMOR FREE CLASSIFICATION ---------------------------------

#' @title Determine Radiological Tumor Presence via LLM
#'
#' @description
#' This function interfaces with a Large Language Model (LLM) to analyze a
#' radiology report and determine if there is radiologically visible tumor
#' within the currently imaged region. It uses the `ellmer` package for
#' LLM communication.
#'
#' @param text Character string. The full text of the radiology report.
#' @param model Character string. The identifier for the LLM model to be used.
#'   Defaults to the value of `.DEFAULT_MODEL`.
#' @param model_options List. Arguments specific to the LLM API
#'   (e.g., temperature, num_ctx, passed as `api_args`). Defaults to the
#'   value of `.DEFAULT_MODEL_OPTIONS`.
#' @param base_url Character string. The base URL for the LLM API service.
#'   Defaults to the value of `.DEFAULT_BASE_URL`.
#'
#' @return A list.
#'   \strong{On success:}
#'   \itemize{
#'     \item \code{radiological_tumor_presence}: Logical. `TRUE` if tumor is
#'           radiologically present, `FALSE` otherwise.
#'     \item \code{justification}: Character string. The LLM's justification.
#'     \item \code{model}: Character string. The name of the model used.
#'     \item \code{prompt}: Character string. The full system prompt sent to the model.
#'   }
#'   \strong{On failure} (e.g., API connection or data extraction error):
#'   \itemize{
#'     \item \code{radiological_tumor_presence}: \code{NA}.
#'     \item \code{model}: Character string. The name of the model that was attempted.
#'     \item \code{prompt}: Character string. The system prompt that was attempted.
#'       (The \code{justification} field will be absent in this case.)
#'   }
#'   Appropriate warnings are issued by the function in case of errors.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' report_text_example <- paste(
#'   "Klinische Angaben: Patient mit bekannter Tumorerkrankung.",
#'   "Befund: Kein Tumornachweis im aktuell untersuchten Bereich.",
#'   "Beurteilung: Komplette Remission."
#' )
#' result <- extract_radiological_tumor_presence(
#'   text = report_text_example
#' )
#' }
extract_radiological_tumor_presence <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = .DEFAULT_BASE_URL
) {
  instructions <- "
**TASK:** Analyze the provided German radiology report to determine if there is
radiologically visible tumor within the currently imaged region.

**IMPORTANT LOGIC:**
    *   Return `TRUE` if there is any evidence of radiologically visible tumor
        within the imaged region.
    *   Return `FALSE` if there is no evidence of any tumor ('Kein Tumornachweis')
        within the imaged region.
    *   Crucially, disregard any information regarding metastasis derived from
        the patient's case history ('Anamnese'); focus exclusively on findings
        from the current imaging exam.

**INPUT:** Full text of a German radiology report.

**OUTPUT:** A boolean value indicating radiological tumor presence.

Below this line starts the radiology report:
---------------------------------------
"
  text <- paste(instructions, text)

  type_definition <- ellmer::type_object(
    radiological_tumor_presence = ellmer::type_boolean(
      "Is there radiologically visible tumor within the currently imaged region based on the provided logic?"
    ),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )

  error_return_list <- list(
    radiological_tumor_presence = NA
  )

  chat <- tryCatch(
    if (base_url == .OLLAMA_API_OPENAI_COMPATIBLE) {
      chat <- ellmer::chat_openai(
        system_prompt = "You are a highly trained medical AI assistant helping users to extract information from radiology reports.",
        base_url = base_url,
        api_key = Sys.getenv("OLLAMA_API_KEY"),
        model = model,
        api_args = model_options
      )
    } else {
      chat <- ellmer::chat_ollama(
        system_prompt = "You are a highly trained medical AI assistant helping users to extract information from radiology reports.",
        base_url = base_url,
        model = model,
        api_args = model_options
      )
    },
    error = function(e_init) {
      warning(glue::glue(
        "Failed to initialize ellmer::chat_ollama for model '{model}' at '{base_url}': {e_init$message}"
      ))
      return(NULL)
    }
  )

  if (is.null(chat)) {
    error_return_list$model <- model
    error_return_list$prompt <- instructions
    return(error_return_list)
  }

  extracted_data <- tryCatch(
    {
      result <- chat$chat_structured(text, type = type_definition)
      result$model <- model
      result$prompt <- instructions
      result
    },
    error = function(e_extract) {
      warning(glue::glue(
        "Error during API call or data extraction for model '{model}': {e_extract$message}"
      ))
      error_return_list$model <- model
      error_return_list$prompt <- instructions
      error_return_list
    }
  )

  return(extracted_data)
}
