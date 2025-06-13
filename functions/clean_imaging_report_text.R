clean_imaging_report_text <- function(text) {
  # Vector of patterns to remove
  patterns_to_remove <- c(
    "defchp",
    "defpap",
    "listtable",
    "listoverridetable",
    "generator",
    "DevExpress",
    "cf0",
    "cf2"
  )

  # Start with the input text
  cleaned <- text

  # Handle "Office File API" (case insensitive)
  cleaned <- gsub("(?i)Office File API", "", cleaned, perl = TRUE)

  # Handle version number pattern xx.x.x.x
  cleaned <- gsub("\\d{2}\\.\\d{1}\\.\\d{1}\\.\\d{1}", "", cleaned)

  # Remove each pattern individually
  for (pattern in patterns_to_remove) {
    cleaned <- gsub(pattern, "", cleaned, fixed = TRUE)
  }

  # Special replacements with <br>
  cleaned <- gsub("Anamnesepar", "<b>Anamnese</b><br>", cleaned)
  cleaned <- gsub("Anamnese\n", "<b>Anamnese</b><br>", cleaned)
  cleaned <- gsub("Fragestellungpar", "<b>Fragestellung</b><br>", cleaned)
  cleaned <- gsub("Fragestellung\n", "<b>Fragestellung</b><br>", cleaned)
  cleaned <- gsub("Befundpar", "Befund<br>", cleaned)
  cleaned <- gsub("Befund\n", "<b>Befund</b><br>", cleaned)
  cleaned <- gsub("Beurteilungpar", "Beurteilung<br>", cleaned)
  cleaned <- gsub("Beurteilung\n", "<b>Beurteilung</b><br>", cleaned)
  cleaned <- gsub("parpar", "<br><br>", cleaned)

  # Convert \n to HTML line break
  cleaned <- gsub("\n\\s*\\?", "<br> •", cleaned)
  cleaned <- gsub("<br>\\s*\\?", "<br> •", cleaned)
  cleaned <- gsub("\n", "<br>", cleaned)
  cleaned <- gsub("- \t", "- ", cleaned)

  # Trim any extra whitespace and remove any double spaces that might have been created
  cleaned <- trimws(cleaned)

  return(cleaned)
}
