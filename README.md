# Exploring Patient Engagement and Retention in the CCMDD Programme

Final R code repository for a Master’s dissertation on patient engagement and retention in South Africa’s Central Chronic Medicines Dispensing and Distribution (CCMDD) programme. The repository includes scripts for data preparation, exploratory data analysis, proxy outcome construction, and modelling.

No raw patient-level data are included in this repository.

## Repository Structure

The repository is organised according to the main stages of the analysis workflow:

### `01_data_import_and_reshaping/`

Contains scripts used to read in the raw SyNCH and prescription datasets, reshape files where needed, clean column formats, and prepare the datasets for later linkage and analysis.

### `02_mapping_and_lookup_tables/`

Contains scripts and supporting lookup tables used to create and apply location and medicine mappings. This includes matching facility and pickup point information, assigning coordinates, extracting medication names, and grouping medicines into broader medication categories.

### `03_exploratory_data_analysis/`

Contains scripts used for exploratory data analysis, including summaries and figures describing the data structure, geographic distribution, timing of records, medicine group distributions, and parcel-level outcome patterns.

### `04_proxy_outcomes_and_patient_tables/`

Contains scripts used to define the proxy outcomes for patient engagement and retention, classify parcel collection behaviour, and align prescription-level and parcel-level information. This step creates the final patient-level analysis tables used for modelling.

### `05_modelling/`

Contains scripts used for the final modelling analyses, including model preparation, training, evaluation, and summary outputs.

## Notes

The scripts are intended to be run in order, following the numbered folder structure. Some scripts rely on intermediate outputs created in earlier stages of the workflow.
