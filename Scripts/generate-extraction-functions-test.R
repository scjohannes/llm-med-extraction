# Ensure required packages are installed:
# install.packages(c("xml2", "rvest", "glue", "stringr"))

library(xml2)
library(rvest)
library(glue)
library(stringr)

#' Clean HTML content and extract plain text.
#'
#' @param html_content A character string containing HTML.
#' @return A character string with HTML tags removed, newlines and tabs normalized.
clean_html_to_text <- function(html_content) {
  if (is.na(html_content) || html_content == "") {
    return("")
  }
  text_content <- tryCatch(
    {
      # Wrap in a div to handle fragments and ensure proper parsing
      read_html(paste0("<div>", html_content, "</div>")) %>%
        html_text(trim = TRUE)
    },
    error = function(e) {
      # Fallback if HTML parsing fails, strip tags manually (basic)
      gsub("<[^>]+>", " ", html_content) # Replace tags with space
    }
  )

  text_content <- str_replace_all(text_content, "\\s+", " ") # Normalize whitespace
  text_content <- str_replace_all(text_content, "&lt;", "<") # Decode common HTML entities
  text_content <- str_replace_all(text_content, "&gt;", ">")
  text_content <- str_replace_all(text_content, "&amp;", "&")
  text_content <- str_replace_all(text_content, "&quot;", "\"")
  text_content <- str_replace_all(text_content, "&#039;", "'")

  return(trimws(text_content))
}

#' Generate an R function string for LLM-based data extraction from REDCap metadata.
#'
#' @param item_node An xml_node for an <ItemDef> from REDCap XML.
#' @param doc The full xml_document for lookups (e.g., CodeLists).
#' @param ns_map A named character vector of namespaces used in the document.
#' @param odm_prefix The namespace prefix for the ODM URI.
#' @param redcap_prefix The namespace prefix for the REDCap URI.
#' @return A character string containing the R function definition, or NULL if generation fails.
generate_r_function_for_item <- function(
  item_node,
  doc,
  ns_map,
  odm_prefix,
  redcap_prefix
) {
  redcap_variable_name <- xml_attr(item_node, "Variable")
  field_type <- xml_attr(item_node, "FieldType")
  item_oid <- xml_attr(item_node, "OID") # OID is not namespaced in this XML schema for ItemDef

  # --- Get Question Text ---
  question_node_xpath <- paste0(
    ".//",
    odm_prefix,
    ":Question/",
    odm_prefix,
    ":TranslatedText"
  )
  question_node <- xml_find_first(item_node, question_node_xpath)
  question_text <- if (!is.na(question_node))
    clean_html_to_text(xml_text(question_node)) else ""

  # --- Get Section Header (Prompt Instructions) ---
  section_header_html <- xml_attr(
    item_node,
    "SectionHeader"
  )
  prompt_instructions <- clean_html_to_text(section_header_html)

  if (prompt_instructions == "" && question_text != "") {
    warning(paste(
      "SectionHeader is empty for item:",
      redcap_variable_name,
      "- Using Question text as primary prompt instruction. Review if this is intended."
    ))
    prompt_instructions <- question_text
  } else if (prompt_instructions == "") {
    warning(paste(
      "SectionHeader and Question text are empty for item:",
      redcap_variable_name,
      "- Prompt might be ineffective."
    ))
  }

  # --- Get Choices for enum types ---
  choices_vector <- NULL
  choices_string_for_prompt <- ""
  ellmer_type_definition <- ""
  enum_field_types <- c("radio", "yesno", "select", "dropdown")

  if (field_type %in% enum_field_types) {
    code_list_ref_node_xpath <- paste0(".//", odm_prefix, ":CodeListRef")
    code_list_ref_node <- xml_find_first(
      item_node,
      code_list_ref_node_xpath,
      ns = ns_map
    )
    if (!is.na(code_list_ref_node)) {
      code_list_oid <- xml_attr(code_list_ref_node, "CodeListOID") # Not namespaced
      code_list_node_xpath <- paste0(
        "//",
        odm_prefix,
        ":CodeList[@OID='",
        code_list_oid,
        "']"
      )
      code_list_node <- xml_find_first(doc, code_list_node_xpath, ns = ns_map)
      if (!is.na(code_list_node)) {
        choice_items_xpath <- paste0(".//", odm_prefix, ":CodeListItem")
        choice_items <- xml_find_all(
          code_list_node,
          choice_items_xpath,
          ns = ns_map
        )
        choices_list <- lapply(choice_items, function(ci) {
          label_node_xpath <- paste0(
            ".//",
            odm_prefix,
            ":Decode/",
            odm_prefix,
            ":TranslatedText"
          )
          label_node <- xml_find_first(ci, label_node_xpath, ns = ns_map)
          label <- if (!is.na(label_node))
            clean_html_to_text(xml_text(label_node)) else ""
          return(label)
        })
        choices_vector <- unlist(choices_list)

        if (length(choices_vector) > 0) {
          choices_string_for_prompt <- paste0(
            "'",
            choices_vector,
            "'",
            collapse = ", "
          )
          r_choices_vector_str <- paste0(
            "c(",
            paste0('"', choices_vector, '"', collapse = ", "),
            ")"
          )

          ellmer_type_definition <- glue::glue(
            "ellmer::type_object(\n",
            "    {redcap_variable_name} = ellmer::type_enum(\n",
            "      paste0(\n",
            "        \"Categorize '{redcap_variable_name}' based on the text. Must be one of: \",\n",
            "        {deparse(choices_string_for_prompt)}\n",
            "      ),\n",
            "      {r_choices_vector_str}\n",
            "    )\n",
            "  )",
            .open = "{",
            .close = "}"
          )
        } else {
          warning(paste(
            "No choices found for enum field:",
            redcap_variable_name,
            "OID:",
            item_oid
          ))
          return(NULL)
        }
      } else {
        warning(paste(
          "CodeList not found for OID:",
          code_list_oid,
          "for item:",
          redcap_variable_name
        ))
        return(NULL)
      }
    } else {
      warning(paste(
        "CodeListRef not found for enum field:",
        redcap_variable_name
      ))
      return(NULL)
    }
  } else {
    warning(paste0(
      "Field type '",
      field_type,
      "' for variable '",
      redcap_variable_name,
      "' is not currently handled for structured enum extraction. Skipping."
    ))
    return(NULL)
  }

  # --- Create Function Name ---
  sane_var_name_for_func <- str_to_title(gsub("_", " ", redcap_variable_name))
  sane_var_name_for_func <- gsub("\\s+", "", sane_var_name_for_func)
  sane_var_name_for_func <- make.names(sane_var_name_for_func)
  function_name <- paste0("extract", sane_var_name_for_func, "Info")

  # --- Assemble the R function string ---
  r_function_string <- glue::glue(
    "#' Extract {redcap_variable_name} Information from Text\n",
    "#'\n",
    "#' Auto-generated function to process text and extract the {redcap_variable_name}\n",
    "#' using a large language model, based on REDCap metadata.\n",
    "#' The prompt instructions are primarily derived from the REDCap field's Section Header.\n",
    "#'\n",
    "#' @param source_text Character string containing the text to analyze (e.g., imaging report).\n",
    "#' @param model Specifies the LLM to use (default: .DEFAULT_MODEL).\n",
    "#' @param base_url Specifies the URL of the LLM instance (default: .DEFAULT_BASE_URL).\n",
    "#' @param model_options List of parameters for the model (default: list(num_ctx = 2048, temperature = 0)).\n",
    "#' @return A list containing the extracted {redcap_variable_name} and other metadata.\n",
    "#' @export\n",
    "#' @importFrom ellmer chat_ollama chat_openai type_object type_enum type_string\n",
    "#' @importFrom glue glue\n",
    "{function_name} <- function(\n",
    "  source_text,\n",
    "  model = .DEFAULT_MODEL,\n",
    "  base_url = .DEFAULT_BASE_URL,\n",
    "  model_options = list(num_ctx = 2048, temperature = 0)\n",
    ") {{\n",
    "  target_field_options_string <- {deparse(choices_string_for_prompt)}\n",
    "  target_type_structure <- {ellmer_type_definition}\n",
    "  prompt_instructions_text <- {deparse(prompt_instructions)}\n",
    "  field_specific_question_text <- {deparse(question_text)}\n",
    "\n",
    "  question_template <- \"\n",
    "    You are a highly trained AI assistant specializing in extracting structured information from text.\n",
    "    Your task is to carefully read the provided text and extract specific information based on the user's request.\n",
    "\n",
    "    CONTEXT AND INSTRUCTIONS (derived from the data capture form design):\n",
    "    {{{{prompt_instructions_placeholder}}}}\n", # Escaped {{ for glue
    "\n",
    "    SPECIFIC QUESTION TO ANSWER (derived from the data capture form field label):\n",
    "    {{{{field_question_placeholder}}}}\n", # Escaped {{ for glue
    "\n",
    "    Based on the text, determine the value for '{redcap_variable_name}'.\n",
    "    You MUST choose your answer for '{redcap_variable_name}' from the following predefined categories only: [{{{{options_placeholder}}}}].\n", # Escaped {{
    "    Provide ONLY the chosen category for '{redcap_variable_name}'. Do not include the field name, explanations, or any other text in your response.\n",
    "\n",
    "    The source text to analyze is below:\n",
    "    --- BEGIN TEXT ---\n",
    "    {{{{text_placeholder}}}}\n", # Escaped {{ for glue
    "    --- END TEXT ---\n",
    "  \"\n",
    "\n",
    "  current_prompt <- glue::glue(\n",
    "    question_template,\n",
    "    prompt_instructions_placeholder = prompt_instructions_text,\n",
    "    field_question_placeholder = field_specific_question_text,\n",
    "    options_placeholder = target_field_options_string,\n",
    "    text_placeholder = source_text,\n",
    "    .open = \"{{{{\",\n", # Use double braces for glue's own placeholders
    "    .close = \"}}}}\"\n",
    "  )\n",
    "\n",
    "  chat <- ellmer::chat_ollama( \n",
    "      system_prompt = \"You are an AI assistant helping users to match free text to a list of categories provided by the user.\",\n",
    "      base_url = base_url,\n",
    "      model = model,\n",
    "      api_args = model_options\n",
    "  )\n",
    "\n",
    "  tryCatch(\n",
    "    {{\n",
    "      extracted_data <- chat$chat_structured(\n",
    "        current_prompt,\n",
    "        type = target_type_structure\n",
    "      )\n",
    "      extracted_data$model_used <- model\n",
    "      # extracted_data$prompt_template <- question_template \n", # Can be very long
    "      # extracted_data$full_prompt_sent_to_llm <- current_prompt \n", # Can be very long
    "      return(extracted_data)\n",
    "    }},\n",
    "    error = function(e) {{\n",
    "      list(\n",
    "        {redcap_variable_name} = paste(\"Error during extraction:\", e$message),\n",
    "        model_used = model,\n",
    "        # prompt_template = question_template,\n",
    "        # full_prompt_sent_to_llm = current_prompt,\n",
    "        error_details = e\n",
    "      )\n",
    "    }}\n",
    "  )\n",
    "}}\n",
    .open = "{",
    .close = "}"
  )
  return(r_function_string)
}


#' Main function to parse REDCap XML and generate R functions for LLM extraction.
#'
#' @param xml_file_path Path to the REDCap XML metadata file.
#' @param llm_action_tag The action tag used to identify fields for LLM processing (default: "@LLM").
#' @return A list of character strings, where each string is an R function definition.
#' @export
generate_llm_functions_from_redcap_xml <- function(
  xml_file_path,
  llm_action_tag = "@LLM"
) {
  if (!file.exists(xml_file_path)) {
    stop("XML file not found at: ", xml_file_path)
  }
  doc <- read_xml(xml_file_path)
  ns_map <- xml_ns(doc)

  odm_uri <- "http://www.cdisc.org/ns/odm/v1.3"
  redcap_uri <- "https://projectredcap.org"

  odm_prefix <- names(ns_map)[which(ns_map == odm_uri)]
  if (length(odm_prefix) == 0) {
    # Attempt to find a likely candidate if not explicitly prefixed but present as default
    # This heuristic might need adjustment based on how xml2 handles truly default namespaces
    # Often, xml2 assigns 'd1' to the first encountered default namespace.
    # For this specific XML, 'd1' is explicitly associated with odm_uri in xml_ns(doc) output.
    stop(paste(
      "ODM namespace URI '",
      odm_uri,
      "' not found with an explicit prefix in the parsed namespaces. Check XML.",
      sep = ""
    ))
  }
  odm_prefix <- odm_prefix[1]

  redcap_prefix <- names(ns_map)[which(ns_map == redcap_uri)]
  if (length(redcap_prefix) == 0) {
    stop(paste(
      "REDCap namespace URI '",
      redcap_uri,
      "' not found with an explicit prefix. Check XML.",
      sep = ""
    ))
  }
  redcap_prefix <- redcap_prefix[1]

  cat(paste(
    "Using ODM prefix:",
    odm_prefix,
    "\nUsing REDCap prefix:",
    redcap_prefix,
    "\n"
  ))

  item_defs_xpath <- paste0("//", odm_prefix, ":ItemDef")
  item_defs <- xml_find_all(doc, item_defs_xpath, ns = ns_map)

  if (length(item_defs) == 0) {
    message(
      "No ItemDef elements found. Check XML structure and namespace prefixes."
    )
    return(list())
  } else {
    message(paste("Found", length(item_defs), "ItemDef elements in total."))
  }

  generated_function_strings <- list()

  for (item_node in item_defs) {
    field_annotation <- xml_attr(
      item_node,
      "FieldAnnotation"
    )
    current_var_name <- xml_attr(item_node, "Variable") # For logging

    if (
      !is.na(field_annotation) &&
        grepl(llm_action_tag, field_annotation, fixed = TRUE)
    ) {
      cat(
        "Processing field marked with '",
        llm_action_tag,
        "': ",
        current_var_name,
        "\n",
        sep = ""
      )

      field_type <- xml_attr(item_node, "FieldType")
      supported_enum_types <- c("radio", "yesno", "select", "dropdown")

      if (field_type %in% supported_enum_types) {
        func_string <- generate_r_function_for_item(
          item_node,
          doc,
          ns_map,
          odm_prefix,
          redcap_prefix
        )
        if (!is.null(func_string)) {
          generated_function_strings[[
            length(generated_function_strings) + 1
          ]] <- func_string
        }
      } else {
        warning(paste0(
          "Skipping LLM processing for field '",
          current_var_name,
          "' as its type '",
          field_type,
          "' is not one of the supported enum types (radio, yesno, select, dropdown)."
        ))
      }
    }
  }

  if (length(generated_function_strings) == 0) {
    cat(
      "No fields suitable for function generation were found with the '",
      llm_action_tag,
      "' tag and supported field type.\n",
      sep = ""
    )
  } else {
    cat(
      "\nSuccessfully generated ",
      length(generated_function_strings),
      " R function(s).\n",
      sep = ""
    )
  }

  return(generated_function_strings)
}

# --- HOW TO USE --- (Instructions remain the same)
# 1. Save this entire script as an R file (e.g., "redcap_llm_generator_v2.R").
# 2. Source it: source("redcap_llm_generator_v2.R")
# 3. Define .DEFAULT_MODEL and .DEFAULT_BASE_URL if needed.
#    .DEFAULT_MODEL <- "mistral:latest"
#    .DEFAULT_BASE_URL <- "http://localhost:11434"
#
# 4. Call the main function:
my_xml_file <- "C:/Users/jomas/Downloads/LLMRadiologyReportBe_2025-05-23_1300.REDCap.xml"
list_of_function_definitions <- generate_llm_functions_from_redcap_xml(
  my_xml_file
)
#
# 5. Make functions usable (write to file and source, or eval).
#    if (length(list_of_function_definitions) > 0) {
#      writeLines(unlist(list_of_function_definitions), "my_generated_llm_extractors.R")
#      source("my_generated_llm_extractors.R")
#    }
# 6. Call a generated function.
