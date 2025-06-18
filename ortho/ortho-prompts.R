library(ellmer)

text <- "Anamnese Sturz Hand li und Hüfte  li Fragestellung Fraktur Beckenübersicht und Hüftgelenk links vom 18.01.2023 Handgelenk links vom 18.01.2023 Hand links vom 18.01.2023 Befund und Beurteilung Keine Vorbefunde. Beckenübersicht und Hüfte: Keine Frakturen. Artikulationen intakt. ISG Arthrose beidseits. Handgelenk und Hand links: Fraktur des Processus styloideus ulnaris mit Gelenkbeteiligung. Kleine Fragmente im Frakturspalt. Distale Radiusfraktur des Processus styloideus radii mit Gelenkbeteiligung. Proximale Gelenkflächen des Radiokarpalgelenks und des Ulnokarpalgelenks nicht intakt. Schrägfraktur des proximalen MC V mit geringer Einstauchung. Mutmasslich Intraartikuläre und mehrfragmentierte Fraktur."

.DEFAULT_MODEL = "llama3.3:70b-instruct-q5_K_M"

.DEFAULT_MODEL_OPTIONS =
  list(
    temperature = 0,
    num_ctx = 10000
  )

.DEFAULT_BASE_URL = "http://rndapollolp01.uhbs.ch:11434"

#' Determine if imaging report is relevant for evaluation of distal radius
#'
#' @param text Character string containing the imaging report text
#' @param model Specifies the LLM to use for the classification. Default: "llama3.3:70b-instruct-q5_K_M"
#' @param model_options List of parameters to pass to the model (default: list(num_ctx = 10000, temperature = 0))
#' @param base_url Specifies the url which points to the ollama instance.
#' @return Boolean: TRUE if cancer diagnosis, FALSE if not
#' @export
#' @importFrom ellmer chat_ollama chat_openai type_object type_boolean
#'
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
    chat <-
      ellmer::chat_ollama(
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
        if (result$is_relevant) {
          return(chat) #return the chat object to move on to next question
        } else {
          return(result$is_relevant)
        }
      },
      error = function(e) {
        return(e$message)
      }
    )
  }


fx_description <-
  function(
    text,
    model = .DEFAULT_MODEL,
    model_options = .DEFAULT_MODEL_OPTIONS
  ) {
    question <-
      "Fracture description instructions
      Based on what is available in the imaging report, you should specify each of the following variables, which cover the patient anamnesis and diagnostic fracture features description:
      
      Monotrauma / Multiple injuries / Not available
      This variable describes the fracture context. The task is the following:
      Monotrauma: You should classify the imaging report in this group if a clear localized trauma is identified for the occurrence of the fracture (e.g. patient falls on the forearm, including radius, ulna or wrist)
      Multiple injuries: You should classify the imaging report in this group if the fracture is part of a broader injury mechanism (either due to a systemic bone disease) or fractures are observed at multiple other locations (e.g. forearm fracture + lower extremity fracture...)
      Not available: You should classify the imaging report in this group if the fracture context cannot be accurately identified based on available data
      
      Specify fracture etiology
      This variable describes the fracture etiology. The task is the following:
      Primary fracture: You should classify the imaging report in this group if the patients shows up at the radiology department with occurrence of a primary fracture
      Revision external ORIF: You should classify the imaging report in this group if the patient shows up at the radiology department with an already treated fracture with an open reduciton an internal fixation (ORIF), treatment can be locally and therefore patient shows up for a revision ORIF or outside and comes for a second opinion
      Not available: You should classify the imaging report in this group if the fracture etiology and presentation at the radiology report cannot be accurately identified based on available data
      
      Open fracture
      This variable describes whether the fracture is an open fracture.
      Yes: You should classify imaging report in this group if the fracture is an open fracture.
      No: You should classify imaging report in this group if the fracture is not an open fracture.
      Not available: You should clasify imaging report in this group if the fracture type cannot be accurately identified based on available data."

    type_fx_desc <-
      ellmer::type_object(
        multiple_inj = ellmer::type_enum(
          description = "Select the most accurate fracture context. Must be one of ['Monotrauma', 'Multiple injuries', 'Not available']",
          values = c(
            "Monotrauma",
            "Multiple injuries",
            "Not available"
          )
        ),
        fracture_etio = ellmer::type_enum(
          description = "Select the most fracture etiology. Must be one of ['Primary fracture', 'Revision external ORIF', 'Not available']",
          values = c(
            "Primary fracture",
            "Revision external ORIF",
            "Not available"
          )
        ),
        fracture_open = ellmer::type_enum(
          description = "Is this an open fracture? Must be one of ['Yes', 'No', 'Not available']",
          values = c(
            "Yes",
            "No",
            "Not available"
          )
        )
      )

    # Initialize chat with the model
    chat <-
      ellmer::chat_ollama(
        system_prompt = "You are a highly trained medical AI assistant specialized in reading imaging reports in orthopedics and traumatology.",
        base_url = .DEFAULT_BASE_URL,
        model = model,
        api_args = model_options
      )

    prompt <- paste0(question, text)

    # Extract and return structured data
    tryCatch(
      {
        result <- chat$chat_structured(prompt, type = type_fx_desc)
        return(result)
      },
      error = function(e) {
        return(e$message)
      }
    )
  }


fx_description_object <-
  function(
    chat_input
  ) {
    question <-
      "Fracture description instructions
      Based on what is available in the imaging report, you should specify each of the following variables, which cover the patient anamnesis and diagnostic fracture features description:
      
      Monotrauma / Multiple injuries / Not available
      This variable describes the fracture context. The task is the following:
      Monotrauma: You should classify the imaging report in this group if a clear localized trauma is identified for the occurrence of the fracture (e.g. patient falls on the forearm, including radius, ulna or wrist)
      Multiple injuries: You should classify the imaging report in this group if the fracture is part of a broader injury mechanism (either due to a systemic bone disease) or fractures are observed at multiple other locations (e.g. forearm fracture + lower extremity fracture...)
      Not available: You should classify the imaging report in this group if the fracture context cannot be accurately identified based on available data
      
      Specify fracture etiology
      This variable describes the fracture etiology. The task is the following:
      Primary fracture: You should classify the imaging report in this group if the patients shows up at the radiology department with occurrence of a primary fracture
      Revision external ORIF: You should classify the imaging report in this group if the patient shows up at the radiology department with an already treated fracture with an open reduciton an internal fixation (ORIF), treatment can be locally and therefore patient shows up for a revision ORIF or outside and comes for a second opinion
      Not available: You should classify the imaging report in this group if the fracture etiology and presentation at the radiology report cannot be accurately identified based on available data
      
      Open fracture
      This variable describes whether the fracture is an open fracture.
      Yes: You should classify imaging report in this group if the fracture is an open fracture. Only answer yes if the radiologist mentions that the fracture appears to be an open fracture.
      No: You should classify imaging report in this group if the fracture is not an open fracture. If the radiologist does not mention anything related to an open fracture, you can assume the fracture to be closed.
      Not available: You should clasify imaging report in this group if the fracture type cannot be accurately identified based on available data."

    type_fx_desc <-
      ellmer::type_object(
        multiple_inj = ellmer::type_enum(
          description = "Select the most accurate fracture context. Must be one of ['Monotrauma', 'Multiple injuries', 'Not available']",
          values = c(
            "Monotrauma",
            "Multiple injuries",
            "Not available"
          )
        ),
        fracture_etio = ellmer::type_enum(
          description = "Select the most fracture etiology. Must be one of ['Primary fracture', 'Revision external ORIF', 'Not available']",
          values = c(
            "Primary fracture",
            "Revision external ORIF",
            "Not available"
          )
        ),
        fracture_open = ellmer::type_enum(
          description = "Is this an open fracture? Must be one of ['Yes', 'No', 'Not available']",
          values = c(
            "Yes",
            "No",
            "Not available"
          )
        )
      )

    prompt <- paste0(question)

    # Extract and return structured data
    tryCatch(
      {
        result <- chat_input$chat_structured(prompt, type = type_fx_desc)
        return(result)
      },
      error = function(e) {
        return(e$message)
      }
    )
  }
