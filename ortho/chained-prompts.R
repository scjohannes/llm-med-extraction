# Johannes Schwenke, Thomas Stojanov - 2025-18-6

# Setup
library(ellmer)
library(tidyverse)

source("ortho/ortho-prompts.R")

# --- Example Usage (requires the definitions of is_report_relevant and fx_description_object) ---

# Example 1: Relevant report
relevant_report_text <- "Röntgenaufnahme des rechten Handgelenks: Dislozierte Fraktur der distalen Radiusmetaphyse nach einem Sturz auf den Unterarm. Keine offenen Weichteilverletzungen sichtbar. Keine weiteren Frakturen erwähnt."
processed_relevant_report <- process_radial_xray(text = relevant_report_text)

# Example 2: Non-relevant report
non_relevant_report_text <- "MRT der Lendenwirbelsäule: Degenerative Veränderungen L4/L5 ohne spinale Stenose."
processed_non_relevant_report <- process_radial_xray(
  text = non_relevant_report_text
)
