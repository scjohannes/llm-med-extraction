#' Assess if a Report's Purpose is Treatment Response (Internal)
#'
#' @return A list containing the chat object and the structured response.
#' @noRd
assess_imaging_report_scope <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  instructions <- "
Analyze the provided German imaging report text and determine if its primary purpose is to assess response to a treatment of the cancer (i.e., a follow-up after treatment, surveillance after remission, or response assessment scan).

**INPUT:** Full text of a German imaging report.

**OUTPUT:** Classify the report's purpose into ONE of the following two categories:
1.  `IN_SCOPE`: The report is a follow-up, monitoring, or treatment response assessment scan. If the patient was treated in the past and we are interested in remission or relapse, this is also in scope.
2.  `OUT_OF_SCOPE`: The report is for an initial diagnosis or initial staging (`Baselinestaging`), the exam didn't take place, the scan was done to rule out infection or anything else unrelated, or the scan was clearly done before any anti-cancer treatment including surgery was begun. 


Below this line starts the radiology report:
---------------------------------------
"
  prompt_text <- paste(instructions, text)

  type_definition <- ellmer::type_object(
    scope_assessment = ellmer::type_enum(
      description = "Classify if the report assesses treatment response. Must be one of ['IN_SCOPE', 'OUT_OF_SCOPE']",
      values = c("IN_SCOPE", "OUT_OF_SCOPE")
    ),
    justification = ellmer::type_string(
      description = "Provide a brief justification for the classification, citing key phrases from the report."
    )
  )

  system_prompt <- "You are a highly trained medical AI assistant helping users classify the purpose of radiology reports."

  # Step 1: Initialize chat
  chat <- .init_chat(
    system_prompt = system_prompt,
    model = model,
    base_url = base_url,
    api_key = api_key,
    model_options = model_options
  )

  if (is.null(chat)) {
    return(NULL) # Pipeline will handle this
  }

  # Step 2: Call structured
  response_data <- .chat_structured_with_fallback(
    chat_obj = chat,
    prompt_text = prompt_text,
    type_definition = type_definition
  )

  # Fill with NAs on failure
  if (is.null(response_data)) {
    response_data <- .type_def_to_error_list(type_definition)
  }

  return(list(
    chat = chat,
    response = response_data
  ))
}

#' Extract Verbatim RANO/RECIST Overall Assessment (Internal)
#'
#' @return The ellmer chat object after the extraction prompt has been run, or NULL on failure.
#' @noRd
extract_rano_recist_verbatim <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  instructions <- "
Your are given a german radiology report for a cancer patiient. 
Response to treatment was classified using RANO or RECIST. 
Extract ad verbatim the sentence in which the radiologist does the OVERALL reponse classification (Gesamtauswertung) according to RANO or RECIST.
Below this line starts the radiology report:
---------------------------------------
"
  full_prompt <- paste(instructions, text)
  system_prompt <- "You are a medical AI assistant."

  chat <- .init_chat(
    system_prompt = system_prompt,
    model = model,
    base_url = base_url,
    api_key = api_key,
    model_options = model_options
  )

  if (is.null(chat)) {
    return(NULL)
  }

  # Make the simple chat call
  tryCatch(
    {
      chat$chat(full_prompt)
      return(chat) # Return the chat object on success
    },
    error = function(e) {
      warning(glue::glue(
        "Error during verbatim RANO/RECIST extraction: {e$message}"
      ))
      return(NULL)
    }
  )
}


#' Map Verbatim Assessment to PET Category (Internal)
#'
#' @param chat An ellmer chat object.
#' @return The structured response from the LLM.
#' @noRd
map_rano_recist_verbatim_to_pet_category <- function(
  chat
) {
  instructions <-
    "
Now map verbatim reponse to one PET-CT metabolic response category.
Must be one of: ['COMPLETE_RESPONSE','PARTIAL_RESPONSE','STABLE_DISEASE','PROGRESSION']
MAPPING HINTS:
- 'Complete Response', 'Komplette Remission' -> COMPLETE_RESPONSE
- 'Partial Response', 'Partielle Remission' -> PARTIAL_RESPONSE
- 'Stable Disease', 'Stabile Erkrankung'     -> STABLE_DISEASE
- 'Progression', 'Progrediente Erkrankung'   -> PROGRESSION

Additionally, determine if the patient is completely tumour free.
- Set `no_tumor` to `TRUE` ONLY IF your classification is `COMPLETE_RESPONSE` AND the full report text (from the previous turn) indicates the patient is completely tumour free ('Kein Tumornachweis').
- Set `no_tumor` to `FALSE` in all other cases.
  "

  type_definition <- ellmer::type_object(
    response_to_trt_pet = ellmer::type_enum(
      description = "PET response category. Must be one of: ['COMPLETE_RESPONSE','PARTIAL_RESPONSE','STABLE_DISEASE','PROGRESSION']",
      values = c(
        "COMPLETE_RESPONSE",
        "PARTIAL_RESPONSE",
        "STABLE_DISEASE",
        "PROGRESSION"
      )
    ),
    justification = ellmer::type_string("Short rationale."),
    no_tumor = ellmer::type_boolean(
      description = "TRUE if COMPLETE_RESPONSE and no tumor evidence, FALSE otherwise."
    )
  )

  response <- .chat_structured_with_fallback(
    chat_obj = chat,
    prompt_text = instructions,
    type_definition = type_definition
  )

  if (is.null(response)) {
    response <- .type_def_to_error_list(type_definition)
  }

  return(response)
}

#' Map Verbatim Assessment to Non-PET Category (Internal)
#'
#' @param chat An ellmer chat object.
#' @return The structured response from the LLM.
#' @noRd
map_rano_recist_verbatim_to_non_pet_category <- function(
  chat
) {
  instructions <-
    "
Now map the verbatim response to one OVERALL response category.
Must be one of: ['RESPONSE','STABLE_DISEASE','PROGRESSION'].
MAPPING HINTS:
- 'Complete Response', 'Komplette Remission' -> RESPONSE
- 'Partial Response', 'Partielle Remission' -> RESPONSE
- 'Stable Disease', 'Stabile Erkrankung'     -> STABLE_DISEASE
- 'Progression', 'Progrediente Erkrankung'   -> PROGRESSION
    
Additionally, determine if the patient is completely tumour free.
- Set `no_tumor` to `TRUE` ONLY IF your classification is `RESPONSE` AND the full report text (from the previous turn) indicates the patient is completely tumour free ('Kein Tumornachweis').
- Set `no_tumor` to `FALSE` in all other cases.
"

  type_definition <- ellmer::type_object(
    response_to_trt = ellmer::type_enum(
      description = "Response category. Must be one of: ['RESPONSE', 'STABLE_DISEASE','PROGRESSION']",
      values = c("RESPONSE", "STABLE_DISEASE", "PROGRESSION")
    ),
    justification = ellmer::type_string("Short rationale."),
    no_tumor = ellmer::type_boolean(
      description = "TRUE if RESPONSE and no tumor evidence, FALSE otherwise."
    )
  )

  response <- .chat_structured_with_fallback(
    chat_obj = chat,
    prompt_text = instructions,
    type_definition = type_definition
  )

  if (is.null(response)) {
    response <- .type_def_to_error_list(type_definition)
  }

  return(response)
}


#' @title Classify PET-CT Cancer Treatment Response via LLM (Chained)
#' @export
extract_treatment_response_pet <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  instructions_analysis <- "
<first task>
Analyze the provided German PET-CT report text and classify the patient's cancer status based *strictly* on described changes in metabolic activity (FDG uptake).
Ignore changes in size (CT findings) unless metabolic information is entirely absent.

**INPUT:** Full text of a German PET-CT report.

**OUTPUT:** Classify the report into ONE of the following categories:
1.  `COMPLETE_RESPONSE`
2.  `PARTIAL_RESPONSE`
3.  `STABLE_DISEASE`
4.  `PROGRESSION`

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

**PRIORITIZATION:**
* Metabolic findings (PET) override anatomical findings (CT).
* The 'Beurteilung' (Impression/Conclusion) section often contains the final summary, but verify against the 'Befund' (Findings) and 'Vergleich' (Comparison) sections.
* The presence of new, metabolically active lesions consistent with malignancy is the strongest indicator for `PROGRESSION`.
* Explicit mention of Deauville Scores (for lymphoma) is a primary classification driver. Distinguish carefully between DS 1-3 (CMR), and DS 4/5 (can be PMR, SMD, or PMD depending on comparison/new lesions).

Below this line starts the radiology report:
  ---------------------------------------

</first task>
"
  instructions_mapping <- "
<second task>
Now please map your final decision to the following STRUCTURE and return ONLY the structured object:
- response_to_trt_pet: one of ['COMPLETE_RESPONSE','PARTIAL_RESPONSE','STABLE_DISEASE','PROGRESSION']
- justification: short 1-2 sentence rationale.
</second task>
"
  instructions_tumorfree <- "
<third task>
If you have chosen `COMPLETE_RESPONSE` you are also asked to indicate whether the patient is completely tumour free, in other words, there is not evidence of any tumour ('Kein Tumornachweis').
If you have chosen `PARTIAL_RESPONSE` or `STABLE_DISEASE` or `PROGRESSION` always answer with false.
**OUTPUT:**
- no_tumor: TRUE / FALSE
</third task>
"
  prompt_text <- paste(instructions_analysis, text, sep = "\n\n")

  type_definition_response <- ellmer::type_object(
    response_to_trt_pet = ellmer::type_enum(
      description = "One of ['COMPLETE_RESPONSE','PARTIAL_RESPONSE','STABLE_DISEASE','PROGRESSION'].",
      values = c(
        "COMPLETE_RESPONSE",
        "PARTIAL_RESPONSE",
        "STABLE_DISEASE",
        "PROGRESSION"
      )
    ),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )
  type_definition_tumorfree <- ellmer::type_object(
    no_tumor = ellmer::type_boolean(
      "There is no evidence of a tumour according to the report."
    )
  )

  system_prompt <- "You are a highly trained medical AI assistant helping users to extract information on treatment response from radiology reports."

  error_return_list <- list(
    response_to_trt_pet = NA_character_,
    no_tumor = NA,
    model = model,
    prompt = paste(
      instructions_analysis,
      instructions_mapping,
      instructions_tumorfree,
      sep = "\n\n"
    )
  )

  # Step 1: Initialize chat
  chat <- .init_chat(
    system_prompt = system_prompt,
    model = model,
    base_url = base_url,
    api_key = api_key,
    model_options = model_options
  )
  if (is.null(chat)) {
    return(error_return_list)
  }

  # Step 2: Free text call
  first_step <- tryCatch(
    {
      chat$chat(prompt_text)
      TRUE
    },
    error = function(e_first) {
      warning(glue::glue(
        "Error during initial free-form chat for model '{model}': {e_first$message}"
      ))
      FALSE
    }
  )
  if (!first_step) {
    return(error_return_list)
  }

  # Step 3: Map response
  resp <- .chat_structured_with_fallback(
    chat_obj = chat,
    prompt_text = instructions_mapping,
    type_definition = type_definition_response
  )
  if (is.null(resp)) {
    return(error_return_list)
  }

  # Step 4: Map tumor-free
  tumorfree <- .chat_structured_with_fallback(
    chat_obj = chat,
    prompt_text = instructions_tumorfree,
    type_definition = type_definition_tumorfree
  )
  # We allow tumorfree to be null and just set no_tumor to NA

  # 4) Output
  out <- list(
    response_to_trt_pet = resp$response_to_trt_pet,
    no_tumor = tumorfree$no_tumor,
    model = model,
    justification = resp$justification,
    prompt = paste(
      instructions_analysis,
      instructions_mapping,
      instructions_tumorfree,
      sep = "\n\n"
    )
  )
  return(out)
}


#' @title Classify CT/MRI Cancer Treatment Response via LLM (Chained)
#' @export
extract_treatment_response_non_pet <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  instructions_analysis <- "
<first task>
**TASK:** Analyze the provided German CT or MRI report text and classify the patient's cancer status.

**INPUT:** Full text of a German CT or MRI report.

**OUTPUT:** Classify the report into ONE of the following categories:
1.  `RESPONSE`
2.  `STABLE_DISEASE`
3.  `PROGRESSION`

<guiding principles>
The core logic depends on the dynamic described in the report's comparison to a prior scan: is the overall tumor burden shrinking, stable, or growing?

  * RESPONSE (R): The direct result of successful treatment. The report describes the elimination (Complete Response) or significant reduction (Partial Response) of tumor burden, OR it confirms the continued absence of disease after a prior complete response.
  * STABLE DISEASE (SD): A state of equilibrium. This applies only when a measurable tumor is present and the report describes it as not having significantly changed in size since the last scan.
  * PROGRESSION (PD): The cancer is growing. The report describes that existing tumors are bigger or, critically, that new tumors have appeared.

</guiding principles>


<Classifcation Criteria>

1.  **`RESPONSE` (R - Ansprechen / Remission)**:
    * **General:** Report indicates either *complete disappearance* of all known/target lesions OR a *significant decrease* in the size (e.g., diameter, volume) of known/target lesions compared to the prior scan. No new lesions are present, and no unequivocal progression of non-target lesions. This category covers both complete and substantial partial responses. A scan performed to confirm the outcome of a recent surgery that successfully removed all visible disease is also classified as RESPONSE for that event.
    * **Look For:** Terms explicitly stating complete or partial response, or significant size reduction/disappearance (e.g., '**komplette Remission**,' '**partielle Remission**,' '**deutliche Größenabnahme der Läsionen**,' '**signifikante Regredienz**,' '**vollständige Rückbildung aller Läsionen**,' '**kein Tumornachweis mehr**,' 'Läsionen nicht mehr abgrenzbar/nachweisbar,' 'vollständige narbige Ausheilung,' '**größenregredient**,' 'deutliche Verkleinerung,' 'gutes Ansprechen'). Comparison to a previous scan must confirm disappearance or significant reduction.

2.  **`STABLE_DISEASE` (SD - Stabile Erkrankung)**:
    * **General:** Report indicates no significant *change* (neither sufficient reduction for `RESPONSE` nor sufficient increase/new lesions for `PROGRESSION`) in the size or characteristics of known lesions compared to the prior scan.
    * **Look For:** Terms like '**stabile Erkrankung**,' '**unveränderter Befund**,' '**konstante Läsionsgröße**,' '**keine signifikante Größenänderung**,' 'Befundstabilität,' 'keine wesentliche Dynamik.'

3.  **`PROGRESSION` (PD - Progrediente Erkrankung / Progression)**:
    * **General:** Report indicates a significant *increase* in the size of existing lesions, OR, critically, the appearance of **new lesions** consistent with malignancy, OR unequivocal progression of non-target lesions.
    * **Look For:** Terms indicating progression (e.g., '**progrediente Erkrankung**,' '**Progression**,' '**Größenzunahme der Läsionen**,' '**Befundverschlechterung**,' 'größenprogredient').
    * **Crucially, look for any mention of 'neue Läsionen,' 'neue Herde,' 'neu aufgetretene Metastasen/Filiae,' 'neu abgrenzbare Läsionen.'** The appearance of new lesions typically signifies progression regardless of the behavior of previously known lesions. The identification of a recurrence (e.g., 'Rezidiv', 'Tumorrezidiv', 'Lokalrezidiv') after a period of remission or after surgical resection is a definitive sign of progression.

</Classifcation Criteria>

<Examples for Classification>
Use these self-contained examples to guide classification logic.

Example 1

* SCENARIO: A patient undergoes chemotherapy for multiple lung metastases.
* REPORT FINDING: 'Compared to the prior study, all previously described lung metastases have resolved.'
* CLASSIFICATION: RESPONSE
* REASONING: The report describes the complete disappearance of the tumor, which is a Complete Response.

Example 2

* SCENARIO: A patient who previously had a complete response has a routine 6-month follow-up scan.
* REPORT FINDING: 'No evidence of tumor recurrence. Post-operative changes are stable compared to the prior examination.'
* CLASSIFICATION: RESPONSE
* REASONING: The key finding is the continued absence of disease. It cannot be Stable Disease because there is no measurable tumor. This confirms a continued state of remission.

Example 3

* SCENARIO: A patient is being treated for a 5 cm liver metastasis.
* REPORT FINDING: 'The known liver metastasis has decreased in size and now measures 2 cm in diameter. This represents a partial response to therapy.'
* CLASSIFICATION: RESPONSE
* REASONING: The report explicitly describes a significant reduction in tumor size.

Example 4

* SCENARIO: A patient who previously had a partial response (tumor shrank to 2 cm) has a follow-up scan 3 months later.
* REPORT FINDING: 'The 2 cm liver metastasis is unchanged in size compared to the study from 3 months ago.'
* CLASSIFICATION: STABLE DISEASE
* REASONING: The most recent event described is stability. The classification is based on the finding in the most recent interval, not the entire treatment history.

Example 5

* SCENARIO: A patient with a primary tumor and a single metastasis undergoes surgery to remove the primary tumor. This is the first scan after the operation.
* REPORT FINDING: 'Status post-resection of the primary tumor in the colon. The known liver metastasis is unchanged.'
* CLASSIFICATION: RESPONSE
* REASONING: The scan documents the result of the major therapeutic intervention (surgery), which successfully removed a large part of the tumor burden.

Example 6

* SCENARIO: A patient from the previous example has another follow-up scan 6 months later.
* REPORT FINDING: 'The known liver metastasis remains stable in size compared to the prior post-operative scan. No new lesions are seen.'
* CLASSIFICATION: STABLE DISEASE
* REASONING: The focus is now on the dynamic of the remaining, measurable tumor. Since it is unchanged, the current state is stable.

Example 7

* SCENARIO: A patient is on active treatment for known lesions.
* REPORT FINDING: 'The known liver lesions are stable, but a new 1 cm lesion is now visible in the spleen, suspicious for a metastasis.'
* CLASSIFICATION: PROGRESSION
* REASONING: The appearance of a new lesion is the strongest indicator of progression and overrides stability elsewhere.

Example 8

* SCENARIO: A patient is on active treatment for known lesions in the liver and spleen.
* REPORT FINDING: 'The known liver lesions have decreased in size, but the lesion of the spleen has significantly increased in size.'
* CLASSIFICATION: PROGRESSION
* REASONING: The growth of an existing lesion overrides decrease elsewhere.

Example 9

* SCENARIO: A patient with astrocytoma had resection of the primary tumor and has a post-op scan.
* REPORT FINDING: 'No evidence for tumor-lesions.'
* CLASSIFICATION: STABLE DISEASE
* REASONING: The key finding is the classification of the radiologist as stable disease. This overrides anything else.


</Examples for Classification>


<Final Rules>
* Prioritize Overriding Information: Pay close attention to the entire report, including addenda, corrections, or later findings (e.g., 'Zusatzbefund', 'Nachtrag', 'Korrigendum'). Information in these sections may provide critical context that overrides or re-interprets the main assessment. The final classification must be based on the most complete and final context provided.
* The appearance of a new lesion characterized as likely malignant is a strong signal for PROGRESSION.
* If the radiologist suggests a non-malignant cause (e.g., infection, inflammation) as more likely, it should not be classified as progression based on that finding alone.
* No Tumor ≠ Stable Disease: A state of 'no detectable tumor' is RESPONSE, UNLESS the radiologist explicitly categorizes the finding as STABLE DISEASE.
* Focus on the Most Recent Change: The classification must reflect the dynamic described in the current report (e.g., 'stable compared to last scan,' 'shrinking since last scan'). The primary classification comes from the most recent interval change.
</Final Rules>

Below this line starts the radiology report:
  ---------------------------------------

</first task>
"
  instructions_mapping <- "
<second task>
  Now please map your final decision to the following STRUCTURE and return ONLY the structured object:
    - extract_treatment_response_non_pet: one of ['RESPONSE','STABLE_DISEASE','PROGRESSION']
    - justification: short 1-2 sentence rationale.
</second task>
    "
  instructions_tumorfree <- "
<third task>
If you have chosen `RESPONSE` you are also asked to indicate whether the patient is completely tumour free, in other words, there is not evidence of any tumour ('Kein Tumornachweis').
If you have chosen `STABLE_DISEASE` or `PROGRESSION` always answer with false.
**OUTPUT:**
- no_tumor: TRUE / FALSE
</third task>
"
  prompt_text <- paste(instructions_analysis, text, sep = "\n\n")

  type_definition_response <- ellmer::type_object(
    response_to_trt = ellmer::type_enum(
      description = "Classify the report into ONE of the following categories. Must be one of ['RESPONSE', 'STABLE_DISEASE', 'PROGRESSION']",
      values = c("RESPONSE", "STABLE_DISEASE", "PROGRESSION")
    ),
    justification = ellmer::type_string(.Q_JUSTIFICATION)
  )
  type_definition_tumorfree <- ellmer::type_object(
    no_tumor = ellmer::type_boolean(
      "There is no evidence for a tumor according to the report."
    )
  )

  system_prompt <- "You are a highly trained medical AI assistant helping users to extract information on treatment response from radiology reports."

  error_return_list <- list(
    response_to_trt = NA_character_,
    no_tumor = NA,
    model = model,
    prompt = paste(
      instructions_analysis,
      instructions_mapping,
      instructions_tumorfree,
      sep = "\n\n"
    )
  )

  # Step 1: Initialize chat
  chat <- .init_chat(
    system_prompt = system_prompt,
    model = model,
    base_url = base_url,
    api_key = api_key,
    model_options = model_options
  )
  if (is.null(chat)) {
    return(error_return_list)
  }

  # Step 2: Free text call
  first_step <- tryCatch(
    {
      chat$chat(prompt_text)
      TRUE
    },
    error = function(e_first) {
      warning(glue::glue(
        "Error during initial free-form chat for model '{model}': {e_first$message}"
      ))
      FALSE
    }
  )
  if (!first_step) {
    return(error_return_list)
  }

  # Step 3: Map response
  resp <- .chat_structured_with_fallback(
    chat_obj = chat,
    prompt_text = instructions_mapping,
    type_definition = type_definition_response
  )
  if (is.null(resp)) {
    return(error_return_list)
  }

  # Step 4: Map tumor-free
  tumorfree <- .chat_structured_with_fallback(
    chat_obj = chat,
    prompt_text = instructions_tumorfree,
    type_definition = type_definition_tumorfree
  )

  # 4) Output
  out <- list(
    response_to_trt = resp$response_to_trt,
    no_tumor = if (is.null(tumorfree)) NA else tumorfree$no_tumor,
    model = model,
    justification = resp$justification,
    prompt = paste(
      instructions_analysis,
      instructions_mapping,
      instructions_tumorfree,
      sep = "\n\n"
    )
  )
  return(out)
}
