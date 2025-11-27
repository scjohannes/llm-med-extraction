#' Check for RANO/RECIST Criteria in Text (Internal)
#'
#' @description
#' Internal helper function for the `response_to_treatment_pipeline`. It
#' performs a case-insensitive search for common imaging classification systems
#' used in oncology (RANO, RECIST, iRECIST).
#'
#' @param text_input A character vector where each element is a string to be checked.
#'
#' @return A logical vector of the same length as `text_input`. Each element is
#'   `TRUE` if a match is found in the corresponding input string, and `FALSE` otherwise.
#' @noRd
extract_rano_recist <- function(
  text_input
) {
  # Input validation
  if (!is.character(text_input)) {
    stop("Error: 'text_input' must be a character vector.")
  }

  # The outer sapply iterates through each string provided in the text_input vector
  sapply(
    text_input,
    function(single_string) {
      # Immediately return FALSE for NA or empty strings
      if (is.na(single_string) || nchar(trimws(single_string)) == 0) {
        return(FALSE)
      }

      # Use agrepl for fuzzy matching for each term.
      found_rano <- grepl(
        " RANO",
        single_string,
        ignore.case = TRUE
      )

      found_recist <- grepl(
        "RECIST",
        single_string,
        ignore.case = TRUE
      )

      found_irecist <- grepl(
        "iRECIST",
        single_string,
        ignore.case = TRUE
      )

      found_irecist <- grepl(
        "iRECIST",
        single_string,
        ignore.case = TRUE
      )

      found_percist <- grepl(
        "PERCIST",
        single_string,
        ignore.case = TRUE
      )

      # Return TRUE if any of the terms were found (logical OR)
      return(found_rano || found_recist || found_irecist || found_percist)
    },
    USE.NAMES = FALSE
  ) # USE.NAMES = FALSE ensures an unnamed vector is returned
}
