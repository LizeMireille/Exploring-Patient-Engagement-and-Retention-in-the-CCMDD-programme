# Exploring Patient Engagement and Retention in the CCMDD Programme

Final R code repository for a Master’s dissertation on patient engagement and retention in South Africa’s Central Chronic Medicines Dispensing and Distribution (CCMDD) programme. The repository includes scripts for data preparation, exploratory data analysis, proxy outcome construction, and modelling.

No raw patient-level data are included in this repository.

## Repository Structure

The repository is organised according to the main stages of the analysis workflow:

### `00_shared/`

Contains shared setup and helper scripts used across the analysis, including data paths, the files used to pass data between stages, modelling and evaluation functions, and figure styling. These scripts are mainly used by stages 03 and 04 rather than run directly.

### `01_data_import_and_reshaping/`

Contains scripts used to read in the raw SyNCH and prescription datasets, reshape files where needed, clean column formats, and prepare the datasets for analysis.

### `02_exploratory_data_analysis/`

Contains scripts used for exploratory data analysis, including summaries and figures describing the data structure, geographic distribution, timing of records, medicine group distributions, and parcel-level outcome patterns.

### `03_proxy_outcomes_and_patient_tables/`

Contains scripts used to define the proxy outcomes for patient engagement and retention, classify parcel collection behaviour, and align prescription-level and parcel-level information. This step creates the final patient-level analysis tables used for modelling.

### `04_modelling/`

Contains scripts used for the final modelling analyses, including model preparation, training, evaluation, and summary outputs.

