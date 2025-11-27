# =========================
# === Internal Helpers  ===
# =========================

# Structured-output capability gate (auto-fallback for gpt-oss)
.supports_structured <- function(model) {
  if (is.null(model)) {
    return(TRUE)
  }
  !grepl("^gpt-oss(:|$)", tolower(model))
}

#' Helper to convert an ellmer::TypeObject to a typed NA list
#' @noRd
.type_def_to_error_list <- function(type_def) {
  if (!requireNamespace("S7", quietly = TRUE)) {
    stop("S7 package is required.")
  }
  if (!S7::S7_inherits(type_def, ellmer::TypeObject)) {
    stop("Input must be an ellmer::TypeObject")
  }

  props <- type_def@properties
  lapply(props, function(prop) {
    if (S7::S7_inherits(prop, ellmer::TypeEnum)) {
      # Enums are string-based
      NA_character_
    } else if (S7::S7_inherits(prop, ellmer::TypeBasic)) {
      # Handle basic types
      switch(
        prop@type,
        "boolean" = NA, # logical NA
        "string" = NA_character_,
        "integer" = NA_integer_,
        "number" = NA_real_,
        NA # default logical NA
      )
    } else if (S7::S7_inherits(prop, ellmer::TypeArray)) {
      list() # Use an empty list as a placeholder for an array
    } else if (S7::S7_inherits(prop, ellmer::TypeObject)) {
      list() # Use an empty list as a placeholder for an object
    } else {
      # Fallback for TypeJsonSchema or other unknown types
      NA # logical NA
    }
  })
}

#' Helper to convert an ellmer::TypeObject to a JSON template list
#' @noRd
.type_def_to_template_list <- function(type_def) {
  if (!requireNamespace("S7", quietly = TRUE)) {
    stop("S7 package is required.")
  }
  if (!S7::S7_inherits(type_def, ellmer::TypeObject)) {
    stop("Input must be an ellmer::TypeObject")
  }

  props <- type_def@properties
  lapply(props, function(prop) {
    if (S7::S7_inherits(prop, ellmer::TypeEnum)) {
      # Enums are string-based
      ""
    } else if (S7::S7_inherits(prop, ellmer::TypeBasic)) {
      # Handle basic types
      switch(
        prop@type,
        "boolean" = FALSE,
        "string" = "",
        "integer" = 0,
        "number" = 0.0,
        "" # default
      )
    } else if (S7::S7_inherits(prop, ellmer::TypeArray)) {
      c() # Will serialize to []
    } else if (S7::S7_inherits(prop, ellmer::TypeObject)) {
      list() # Will serialize to {}
    } else {
      # Fallback for TypeJsonSchema or unknown types
      ""
    }
  })
}

# Best-effort JSON repair: keep from first "{" to last "}", strip code fences
.repair_json <- function(x) {
  if (!is.character(x) || length(x) == 0) {
    return(x)
  }
  x <- x[[1L]]
  start <- regexpr("\\{", x, perl = TRUE)
  if (start == -1L) {
    return(x)
  }
  ends <- gregexpr("\\}", x, perl = TRUE)[[1]]
  if (length(ends) == 0L || ends[1] == -1L) {
    return(x)
  }
  x2 <- substr(x, start, max(ends))
  x2 <- gsub("^\\s*```(?:json)?\\s*|\\s*```\\s*$", "", x2)
  x2
}

# Coerce parsed object to expected names/types using the NA-typed prototype
.coerce_fields <- function(obj, example_list) {
  out <- example_list
  for (nm in names(example_list)) {
    if (!is.null(obj[[nm]])) out[[nm]] <- obj[[nm]]
  }
  for (nm in names(out)) {
    proto <- example_list[[nm]]
    if (is.logical(proto)) {
      # Handle logical fields
      val <- out[[nm]]
      if (is.list(val)) {
        # If it's a list, try to extract first element
        val <- if (length(val) > 0) val[[1]] else NA
      }
      out[[nm]] <- as.logical(val)
      if (length(out[[nm]]) == 0) out[[nm]] <- NA
    } else if (is.character(proto)) {
      val <- out[[nm]]
      if (is.list(val)) {
        # Recursively flatten nested lists and collapse into a single string
        flat <- unlist(val, recursive = TRUE, use.names = FALSE)
        if (length(flat) > 0) {
          # Convert all elements to character and collapse with space
          out[[nm]] <- paste(as.character(flat), collapse = " ")
        } else {
          # Empty list becomes NA
          out[[nm]] <- NA_character_
        }
      } else {
        # Normal conversion
        out[[nm]] <- as.character(val)
        if (length(out[[nm]]) == 0) out[[nm]] <- NA_character_
      }
    } else if (is.numeric(proto)) {
      # Handle numeric fields
      val <- out[[nm]]
      if (is.list(val)) {
        # If it's a list, try to extract first element
        val <- if (length(val) > 0) val[[1]] else NA
      }
      out[[nm]] <- suppressWarnings(as.numeric(val))
      if (length(out[[nm]]) == 0) out[[nm]] <- NA_real_
    }
  }
  out
}

# JSON-only chat extractor (uses type_definition)
.chat_json_extract <- function(
  chat,
  text,
  system_prompt_template,
  region_name,
  metastasis_questions,
  type_definition,
  model_name
) {
  # Generate template and error list from the single source of truth
  template_list <- .type_def_to_template_list(type_definition)
  error_list <- .type_def_to_error_list(type_definition)
  json_tmpl <- jsonlite::toJSON(
    template_list,
    auto_unbox = TRUE,
    pretty = FALSE
  )

  user_prompt <- glue::glue(
    "{system_prompt_template}

Return exactly ONE JSON object that matches the schema of this template:
{json_tmpl}

Your task and rules:
- Start your response with \"{{\" and end with \"}}\" (no markdown code fences).
- Output ONLY a single JSON object. No explanations.
- Use TRUE/FALSE booleans (not strings) for yes/no fields.
- If unsure, prefer false over hallucination.
- Do not include keys not present in the template.

Questions to answer (TRUE / FALSE):
{metastasis_questions}

Text to extract from:
{text}

JSON:
"
  )

  # BUG FIX: Safely call chat$chat() and check the return
  turn <- chat$chat(user_prompt)

  if (is.list(turn)) {
    # Path 1: Standard (list-like object)
    if (is.null(turn$content)) {
      stop(paste0(
        "API call returned a list-like object, but '$content' is NULL. Model may have returned an empty response."
      ))
    }
    raw <- turn$content
  } else if (is.character(turn) && length(turn) == 1) {
    # Path 2: gpt-oss is returning the raw JSON string directly.
    raw <- turn
  } else {
    # This is a real error (NULL, numeric, etc.)
    if (is.null(turn)) {
      stop(
        "API call failed: chat$chat() returned NULL. Check model connection, URL, and API key."
      )
    } else {
      stop(paste0(
        "API call failed: chat$chat() returned an unexpected data type: ",
        class(turn)[1]
      ))
    }
  }

  # Continue as before
  fixed <- .repair_json(raw)
  if (!jsonlite::validate(fixed)) {
    stop(paste("Model returned invalid JSON after repair:", fixed))
  }

  obj <- jsonlite::fromJSON(fixed, simplifyVector = TRUE)

  res <- .coerce_fields(obj, error_list)
  res$model <- model_name
  res$prompt <- user_prompt
  res
}

#' Generic helper for JSON-in-prompt fallback
#' @noRd
.chat_json_fallback <- function(
  chat_obj,
  prompt_text,
  type_definition
) {
  # Generate template and error list from the type definition
  template_list <- .type_def_to_template_list(type_definition)
  error_list <- .type_def_to_error_list(type_definition) # For coercion
  json_tmpl <- jsonlite::toJSON(
    template_list,
    auto_unbox = TRUE,
    pretty = FALSE
  )

  # Construct the new prompt
  json_fallback_prompt <- glue::glue(
    "
{prompt_text}

---
**TASK:**
You MUST respond with a single, valid JSON object that adheres to the following template.
Do NOT provide any text, explanations, or markdown fences before or after the JSON.
Your entire response must start with `{{` and end with `}}`.

**JSON Template:**
{json_tmpl}

**JSON Output:**
"
  )

  turn <- chat_obj$chat(json_fallback_prompt)

  if (is.list(turn)) {
    if (is.null(turn$content)) {
      stop(paste0(
        "API call returned a list-like object, but '$content' is NULL. Model may have returned an empty response."
      ))
    }
    raw <- turn$content
  } else if (is.character(turn) && length(turn) == 1) {
    raw <- turn
  } else {
    if (is.null(turn)) {
      stop(
        "API call failed: chat$chat() returned NULL. Check model connection, URL, and API key."
      )
    } else {
      stop(paste0(
        "API call failed: chat$chat() returned an unexpected data type: ",
        class(turn)[1]
      ))
    }
  }

  fixed <- .repair_json(raw)
  if (!jsonlite::validate(fixed)) {
    stop(paste("Model returned invalid JSON after repair:", fixed))
  }

  obj <- jsonlite::fromJSON(fixed, simplifyVector = TRUE)

  res <- .coerce_fields(obj, error_list)

  return(res)
}

#' Internal wrapper for robust ellmer structured calls
#'
#' @param system_prompt The system prompt string.
#' @param user_prompt The user prompt string (e.g., instructions + text).
#' @param type_definition The ellmer::type_object definition for the expected output.
#' @param model Model name.
#' @param base_url API base URL.
#' @param api_key API key.
#' @param model_options Model options list.
#'
#' @return A list containing the parsed data or an NA-filled error list.
#' @noRd
.call_ellmer_structured <- function(
  system_prompt,
  user_prompt,
  type_definition,
  model = .DEFAULT_MODEL,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY"),
  model_options = .DEFAULT_MODEL_OPTIONS
) {
  # 1. Generate the error list first, so we can return it on any failure
  error_list <- .type_def_to_error_list(type_definition)
  error_list$model <- model
  error_list$prompt <- user_prompt # Return the prompt that failed

  # 2. Initialize chat object
  chat <- tryCatch(
    {
      ellmer::chat_openai(
        system_prompt = system_prompt,
        base_url = base_url,
        api_key = api_key,
        model = model,
        api_args = model_options
      )
    },
    error = function(e_init) {
      warning(glue::glue(
        "Failed to initialize ellmer chat for model '{model}': {e_init$message}"
      ))
      return(NULL)
    }
  )

  if (is.null(chat)) {
    return(error_list)
  }

  # 3. Determine if we need to use the JSON-in-prompt fallback
  use_chat_json <- isTRUE(model_options$use_chat_json) ||
    !.supports_structured(model)

  # 4. Extract structured data
  extracted_data <- tryCatch(
    {
      if (use_chat_json) {
        .chat_json_fallback(
          chat_obj = chat,
          prompt_text = user_prompt,
          type_definition = type_definition
        )
      } else {
        chat$chat_structured(
          user_prompt,
          type = type_definition
        )
      }
    },
    error = function(e_struct) {
      warning(glue::glue(
        "Error during structured data extraction for model '{model}': {e_struct$message}"
      ))
      # Return the NA-filled list on error
      return(NULL) # Will be caught by the next check
    }
  )

  if (is.null(extracted_data)) {
    return(error_list)
  }

  # 5. Success: add metadata and return
  extracted_data$model <- model
  extracted_data$prompt <- user_prompt

  # Ensure all keys from the error list are present
  final_out <- error_list
  for (nm in names(extracted_data)) {
    if (nm %in% names(final_out)) {
      final_out[[nm]] <- extracted_data[[nm]]
    }
  }

  return(final_out)
}

#' Check if a model requires the single-shot (v2) prompt strategy
#' @noRd
.is_single_shot_model <- function(model) {
  if (is.null(model)) {
    return(FALSE)
  }
  # This is the inverse of .supports_structured
  grepl("^gpt-oss(:|$)", tolower(model))
}

#' Robustly initialize an ellmer chat object
#'
#' @return A chat object on success, or NULL on failure.
#' @noRd
.init_chat <- function(system_prompt, model, base_url, api_key, model_options) {
  tryCatch(
    {
      ellmer::chat_openai(
        system_prompt = system_prompt,
        base_url = base_url,
        api_key = api_key,
        model = model,
        api_args = model_options
      )
    },
    error = function(e_init) {
      warning(glue::glue(
        "Failed to initialize ellmer chat for model '{model}': {e_init$message}"
      ))
      return(NULL)
    }
  )
}

#' Robustly call chat_structured with fallback
#'
#' Takes an *existing* chat object and performs a structured call.
#' @return Parsed list on success, or NULL on failure.
#' @noRd
.chat_structured_with_fallback <- function(
  chat_obj,
  prompt_text,
  type_definition
) {
  # Get model info from chat object
  provider <- chat_obj$get_provider()
  model_name <- provider@model
  model_opts <- provider@extra_args

  # Determine fallback
  use_chat_json <- isTRUE(model_opts$use_chat_json) ||
    !.supports_structured(model_name)

  # Call the appropriate method
  response <- tryCatch(
    {
      if (use_chat_json) {
        .chat_json_fallback(
          chat_obj = chat_obj,
          prompt_text = prompt_text,
          type_definition = type_definition
        )
      } else {
        chat_obj$chat_structured(prompt_text, type = type_definition)
      }
    },
    error = function(e_struct) {
      warning(glue::glue(
        "Error during structured data extraction for model '{model_name}': {e_struct$message}"
      ))
      return(NULL)
    }
  )

  return(response)
}
