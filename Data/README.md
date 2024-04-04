# Data README
Data for the different models and summary data

Data files are referenced by the [R Scripts](/RScripts).

#### File Descriptions
- Any csv with 'NULL' means that prey are not able to shift in time, habitat, or spatial domain.
- The second part of any csv name is the length a simulation was run: 1year, 5year, 10year, 20year.
- Last part of any csv name is the date that the simulation was run.

1 Year Models
- "Prepped_data/NCvsC_1year_TSH_Nov29.csv" _1 year data with consumptive effects and non-consumptive effects_
- "Prepped_data/NCvsC_Nullyear_TSH_Nov19.csv" _1 year data with only consumptive effects_

5 Year Models
- "Prepped_data/NCvsC_5year_TSH_Nov29.csv" _5 years data with consumptive effects and non-consumptive effects_
- "Prepped_data/NCvsC_NULL_5year_TSH_Nov29.csv" _5 years data with only consumptive effects_

10 Year Models
- "Prepped_data/NCvsC_10year_TSH_Sept252023.csv" _10 years data with consumptive effects and non-consumptive effects_
- "Prepped_data/NCvsC_NULL_10year_TSH_Sept252023.csv" _10 years data with only consumptive effects_

20 Year Models
- "Prepped_data/NCvsC_20year_TSH_Sept252023.csv" _20 years data with consumptive effects and non-consumptive effects_
- "Prepped_data/NCvsC_NULL_20year_TSH_Sept252023.csv" _20 years data with only consumptive effects_

Raw Outputs from NetLogo
- 1YEAR_NULL_Nov19-table.csv
- 1YEAR_MODELS_Nov29-table.csv
- 10YEAR_NULL_Sept23_2023-table.csv
- 10YEAR_MODELS_Sept23_2023-table.csv
- 20YEAR_NULL_Sept23_2023-table.csv
- 20YEAR_MODELS_Sept23_2023-table.csv
- YEAR5_MODELS-tableNov29.csv
- YEAR5_NULL_Nov29-table.csv

#### File List
- HazardRatioSummary_Feb2022.csv : summary file for the survival hazard ratio models for each model condition
- NCEvNullSummary.csv : summary file with median survival, 95% CI on median survival, p-value for difference between CE and CE + NCE models
- 1YEAR_NULL_Nov19-table.csv