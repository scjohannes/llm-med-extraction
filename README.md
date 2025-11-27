# LLM-Med-Extraction: Structured Data Extraction from Imaging Reports

This repository contains the analysis code, prompt engineering logic, and data processing pipelines associated with the manuscript:

**"Evaluation of Structured Data Extraction by Large Language Model from Imaging Reports of Cancer Patients"**

*Authors: Lea P. Passweg\*, Johannes M. Schwenke\*, Christof M. Schönenberger, et al.*

## Important Note on Reproducibility

**Please read this before attempting to run the code.**

The scripts in this repository (specifically in `oncology/scripts/`) rely on an internal R package named `MedOnko`. This package handles: 1. Secure connections to the University Hospital Basel's clinical data warehouse (CDWH). 2. API wrappers for our local GPU cluster (Ollama hosting). 3. Upload/Download operations with our local REDCap instance.

For security reasons, `MedOnko` cannot be shared. Consequently, the script `oncology/scripts/run-llm-extraction.R` will not run in your environment without modification.

However, we have provided all prompt templates, logic flows, and extraction pipelines in the `oncology/code-prompts/` directory. These files are self-contained regarding the LLM logic and use the open-source [`ellmer`](https://ellmer.tidyverse.org/) package. You can adapt these functions to run on your own data infrastructure.

## Repository Structure

The repository is organized into the following main directories:

### 1. Oncology (Manuscript Code)

Located in `oncology/`. This directory contains all materials relevant to the cited paper.

-   **`code-prompts/` (Core Methodology):** This is the most important folder for readers of the paper. It contains the exact prompts and logic chains used for the study.
    -   `ResponsePipeline.R`: The logic flow for assessing treatment response (Scope Check → RECIST Check → PET vs Non-PET branching).
    -   `ExtractMetastasis.R`: Prompts for identifying metastases by anatomical region.
    -   `ExtractTumorLocation.R`: Prompts for extracting the primary diagnosis.
    -   `ExtractResponseChained.R`, `ExtractResponseSingle.R`: The specific prompt templates for chained vs single prompting strategies.
    -   `utils.R`: Helper functions for structured output via `ellmer`.
-   **`scripts/`**: The execution scripts used to run the study.
    -   `run-llm-extraction.R`: The driver script that iterates through reports and calls the pipelines (relies on `MedOnko`).
    -   `clean-report-texts.R`: Pre-processing steps for text cleaning.
-   **Analysis & Figures**:
    -   `analysis-main.qmd`: Quarto document generating the figures and tables for the main manuscript.
    -   `analysis-supplementary.qmd`: Quarto document generating all eFigures and eTables for the supplement.
    -   `data-preparation.qmd`: Code used to merge human ground truth with LLM predictions.

### 2. Orthopedics (Related Project)

Located in `ortho/`. \* This directory contains code and prompts for a separate, unrelated project regarding distal radius fractures. It is not part of the oncology manuscript but shares similar extraction methodologies.