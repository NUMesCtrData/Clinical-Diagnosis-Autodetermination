# Clinical Diagnosis Calculator
This R function, clin_dx_calc_function, is designed to process and classify clinical research diagnostic information for cognitive and neurodegenerative syndromes. It supports datasets from PreUDS3, UDS3, and UDS4, and produces derived variables for syndromic diagnoses, clinical subtypes, and cognitive/behavioral domains.

### 💡 Link to Code: https://github.com/NUMesCtrData/Clinical-Diagnosis-Autodetermination/autodetermine_clindx.R

### 🔧 Purpose

The function creates standardized diagnosis and domain variables from raw clinical research data, including:

  - Dementia subtypes (e.g., Alzheimer's variants, FTD, LBD, PSP, MSA)
  - Mild Cognitive Impairment (MCI) and subtypes
  - Subjective Cognitive Decline (SCD) classifications
  - Primary Progressive Aphasia (PPA) and its subtypes
  - Functional and behavioral domain impairments (e.g., executive, visuospatial, affect regulation)

It includes logic for internal vs. external data extraction and can optionally retain all individual diagnosis columns or only combined fields.

### Function Signature
clin_dx_calc_function(data, preuds3 = FALSE, uds3 = FALSE, uds4 = FALSE, 
                      internal = FALSE, checkbox = FALSE)

### Dependencies
This function uses the following R packages:
  - dplyr
  - stringr
  - tidyr

Make sure to install them if not already.

### Inputs



Argument	Type	Description
data	DataFrame	A dataset containing raw clinical variables.
preuds3	Logical	Flag for using PreUDS3 logic.
uds3	Logical	Flag for using UDS3 logic.
uds4	Logical	Flag for using UDS4 logic.
internal	Logical	If TRUE, includes additional diagnostic refinements for internal use.
checkbox	Logical	If TRUE, retains all individual diagnosis columns alongside combined ones.
Outputs
Returns a modified version of the input data with added variables:

calc_dx: Unified clinical diagnosis string

calc_dx_specify: Extended diagnosis details (if internal = TRUE)

domains: Summary of cognitive/behavioral domains affected

Optionally: All intermediate diagnosis and domain columns (if checkbox = TRUE)

Usage
r
Copy
Edit
# Load packages
library(dplyr)
library(stringr)
library(tidyr)

# Run the function
df <- clin_dx_calc_function(data = my_clinical_data, preuds3 = TRUE, uds3 = TRUE, uds4 = TRUE, 
                            internal = TRUE, checkbox = TRUE)
Author & License
Author: [Your Name or Team Name]
Institution: [Your Organization or Affiliation]
License: MIT or other license of your choice

Notes
Only applicable when preuds3, uds3, and uds4 are all TRUE.

The logic is based on variable naming conventions and encoding specific to UDS datasets.

This function is intended for research/clinical analytics; use with caution for clinical decision-making.

