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

  if (base_url == Sys.getenv(usb_ollama_api_2)) {
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
