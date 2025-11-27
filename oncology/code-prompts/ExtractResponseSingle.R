#' @title Classify CT/MRI Cancer Treatment Response (Single-Shot)
#'
extract_treatment_response_non_pet_v2 <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  instructions_full <- "
<analysis_task>
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
</analysis_task>

<extraction_task>
Now, provide your final answer as a single, complete JSON object.
Do NOT provide any text or explanations before or after the JSON.

**JSON Fields to Extract:**
1.  `response_to_trt`: Classify the report into ONE of: ['RESPONSE', 'STABLE_DISEASE', 'PROGRESSION'].
2.  `justification`: Provide a short 1-2 sentence rationale for your classification.
3.  `no_tumor`:
    - Set to `TRUE` ONLY IF you classified as `RESPONSE` AND the report indicates the patient is completely tumour free ('Kein Tumornachweis').
    - Set to `FALSE` in all other cases (i.e., if classification is `STABLE_DISEASE`, `PROGRESSION`, or `RESPONSE` with residual tumor).
</extraction_task>

Below this line starts the radiology report:
  ---------------------------------------
"
  prompt_text <- paste(instructions_full, text, sep = "\n\n")

  type_definition_full <- ellmer::type_object(
    response_to_trt = ellmer::type_enum(
      description = "Classify the report into ONE of ['RESPONSE', 'STABLE_DISEASE', 'PROGRESSION']",
      values = c("RESPONSE", "STABLE_DISEASE", "PROGRESSION")
    ),
    justification = ellmer::type_string(.Q_JUSTIFICATION),
    no_tumor = ellmer::type_boolean(
      "TRUE if no evidence for a tumour, FALSE otherwise."
    )
  )

  system_prompt <- "You are a highly trained medical AI assistant helping users to extract information on treatment response from radiology reports."

  extracted_data <- .call_ellmer_structured(
    system_prompt = system_prompt,
    user_prompt = prompt_text,
    type_definition = type_definition_full,
    model = model,
    base_url = base_url,
    api_key = api_key,
    model_options = model_options
  )

  extracted_data$prompt <- instructions_full # Use template for brevity
  return(extracted_data)
}

#' @title Classify PET-CT Cancer Treatment Response (Single-Shot)
#'
extract_treatment_response_pet_v2 <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  instructions_full <- "
<analysis_task>
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

</analysis_task>

<extraction_task>
Now, provide your final answer as a single, complete JSON object.
Do NOT provide any text or explanations before or after the JSON.

**JSON Fields to Extract:**
1.  `response_to_trt_pet`: Classify the report into ONE of: ['COMPLETE_RESPONSE', 'PARTIAL_RESPONSE', 'STABLE_DISEASE', 'PROGRESSION'].
2.  `justification`: Provide a short 1-2 sentence rationale for your classification.
3.  `no_tumor`:
    - Set to `TRUE` ONLY IF you classified as `COMPLETE_RESPONSE` AND the report indicates the patient is completely tumour free ('Kein Tumornachweis').
    - Set to `FALSE` in all other cases.
</extraction_task>

Below this line starts the radiology report:
  ---------------------------------------
"
  prompt_text <- paste(instructions_full, text, sep = "\n\n")

  type_definition_full <- ellmer::type_object(
    response_to_trt_pet = ellmer::type_enum(
      description = "One of ['COMPLETE_RESPONSE','PARTIAL_RESPONSE','STABLE_DISEASE','PROGRESSION']",
      values = c(
        "COMPLETE_RESPONSE",
        "PARTIAL_RESPONSE",
        "STABLE_DISEASE",
        "PROGRESSION"
      )
    ),
    justification = ellmer::type_string(.Q_JUSTIFICATION),
    no_tumor = ellmer::type_boolean(
      "TRUE if no evidence for a tumour, FALSE otherwise."
    )
  )

  system_prompt <- "You are a highly trained medical AI assistant..."

  extracted_data <- .call_ellmer_structured(
    system_prompt = system_prompt,
    user_prompt = prompt_text,
    type_definition = type_definition_full,
    model = model,
    base_url = base_url,
    api_key = api_key,
    model_options = model_options
  )

  extracted_data$prompt <- instructions_full # Use template for brevity
  return(extracted_data)
}


#' @title Classify PET-CT Cancer Treatment Response (RANO/RECIST, Single-Shot)
#' @noRd
extract_treatment_response_pet_recist <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  instructions_full <- "
<analysis_task>
**TASK:** Analyze the provided German PET-CT report, which uses RANO/RECIST criteria, and classify the patient's cancer status.

**INPUT:** Full text of a German PET-CT report.

**Step 1:** First, find the single sentence in the report that states the OVERALL response classification (Gesamtauswertung) according to RANO or RECIST.

**Step 2:** Second, map this verbatim sentence to ONE of the following categories: ['COMPLETE_RESPONSE', 'PARTIAL_RESPONSE', 'STABLE_DISEASE', 'PROGRESSION'].
MAPPING HINTS:
- 'Complete Response', 'Komplette Remission' -> COMPLETE_RESPONSE
- 'Partial Response', 'Partielle Remission' -> PARTIAL_RESPONSE
- 'Stable Disease', 'Stabile Erkrankung'    -> STABLE_DISEASE
- 'Progression', 'Progrediente Erkrankung'  -> PROGRESSION
</analysis_task>

<extraction_task>
Now, provide your final answer as a single, complete JSON object.
Do NOT provide any text or explanations before or after the JSON.

**JSON Fields to Extract:**
1.  `response_to_trt_pet`: The category you chose in Step 2.
2.  `justification`: The verbatim RANO/RECIST sentence you found in Step 1 (or a short rationale if not found).
3.  `no_tumor`:
    - Set to `TRUE` ONLY IF your classification is `COMPLETE_RESPONSE` AND the full report text indicates the patient is completely tumour free ('Kein Tumornachweis').
    - Set to `FALSE` in all other cases.
</extraction_task>

Below this line starts the radiology report:
  ---------------------------------------
"
  prompt_text <- paste(instructions_full, text, sep = "\n\n")

  type_definition_full <- ellmer::type_object(
    response_to_trt_pet = ellmer::type_enum(
      description = "PET response category. Must be one of: ['COMPLETE_RESPONSE','PARTIAL_RESPONSE','STABLE_DISEASE','PROGRESSION']",
      values = c(
        "COMPLETE_RESPONSE",
        "PARTIAL_RESPONSE",
        "STABLE_DISEASE",
        "PROGRESSION"
      )
    ),
    justification = ellmer::type_string("The verbatim RANO/RECIST sentence."),
    no_tumor = ellmer::type_boolean(
      "TRUE if no evidence for a tumour, FALSE otherwise."
    )
  )

  system_prompt <- "You are a highly trained medical AI assistant..."

  extracted_data <- .call_ellmer_structured(
    system_prompt = system_prompt,
    user_prompt = prompt_text,
    type_definition = type_definition_full,
    model = model,
    base_url = base_url,
    api_key = api_key,
    model_options = model_options
  )

  extracted_data$prompt <- instructions_full # Use template for brevity
  return(extracted_data)
}

#' @title Classify CT/MRI Cancer Treatment Response (RANO/RECIST, Single-Shot)
#' @noRd
extract_treatment_response_non_pet_recist <- function(
  text,
  model = .DEFAULT_MODEL,
  model_options = .DEFAULT_MODEL_OPTIONS,
  base_url = Sys.getenv("usb_ollama_api_2"),
  api_key = Sys.getenv("OLLAMA_API_KEY")
) {
  instructions_full <- "
<analysis_task>
**TASK:** Analyze the provided German CT or MRI report, which uses RANO/RECIST criteria, and classify the patient's cancer status.

**INPUT:** Full text of a German CT or MRI report.

**Step 1:** First, find the single sentence in the report that states the OVERALL response classification (Gesamtauswertung) according to RANO or RECIST.

**Step 2:** Second, map this verbatim sentence to ONE of the following categories: ['RESPONSE', 'STABLE_DISEASE', 'PROGRESSION', 'NOT_APPLICABLE'].
MAPPING HINTS:
- 'Complete Response', 'Komplette Remission' -> RESPONSE
- 'Partial Response', 'Partielle Remission' -> RESPONSE
- 'Stable Disease', 'Stabile Erkrankung'    -> STABLE_DISEASE
- 'Progression', 'Progrediente Erkrankung'  -> PROGRESSION
</analysis_task>

<extraction_task>
Now, provide your final answer as a single, complete JSON object.
Do NOT provide any text or explanations before or after the JSON.

**JSON Fields to Extract:**
1.  `response_to_trt`: The category you chose in Step 2.
2.  `justification`: The verbatim RANO/RECIST sentence you found in Step 1 (or a short rationale if not found).
3.  `no_tumor`:
    - Set to `TRUE` ONLY IF your classification is `RESPONSE` AND the full report text indicates the patient is completely tumour free ('Kein Tumornachweis').
    - Set to `FALSE` in all other cases (i.e., if classification is `STABLE_DISEASE`, `PROGRESSION`, `NOT_APPLICABLE`, or `RESPONSE` with residual tumor).
</extraction_task>

Below this line starts the radiology report:
  ---------------------------------------
"
  prompt_text <- paste(instructions_full, text, sep = "\n\n")

  type_definition_full <- ellmer::type_object(
    response_to_trt = ellmer::type_enum(
      description = "Response category. Must be one of: ['RESPONSE', 'STABLE_DISEASE','PROGRESSION']",
      values = c("RESPONSE", "STABLE_DISEASE", "PROGRESSION")
    ),
    justification = ellmer::type_string("The verbatim RANO/RECIST sentence."),
    no_tumor = ellmer::type_boolean(
      "TRUE if no evidence for a tumour, FALSE otherwise."
    )
  )

  system_prompt <- "You are a highly trained medical AI assistant..."

  extracted_data <- .call_ellmer_structured(
    system_prompt = system_prompt,
    user_prompt = prompt_text,
    type_definition = type_definition_full,
    model = model,
    base_url = base_url,
    api_key = api_key,
    model_options = model_options
  )

  extracted_data$prompt <- instructions_full # Use template for brevity
  return(extracted_data)
}
