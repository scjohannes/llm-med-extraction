# Johannes Schwenke, Thomas Stojanov - 2025-18-6

# Setup
library(ellmer)
library(tidyverse)

source("ortho/ortho-prompts.R")


result <- is_report_relevant(text)


fx_description(text)

chat <- is_report_relevant(text)
fx_description_object(chat)
