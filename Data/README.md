# Data README
Data for the different models and summary data

Data files are referenced by the [R Scripts](/RScripts).

#### File Descriptions
- Any csv with 'NCvsC' means that the model includes prey dying.
- Any csv with 'NC' means that the model does not include prey dying (predators are always unsuccessful)
- The second part of any csv name is the length a simulation was run: 1year, 5years or unlimited (until all prey die)
- Last part of any csv name is the date that the simulation was run.

1 Year Models
- "NCvsC_1year_TSH_Nov29.csv" _1 year data with consumptive effects and non-consumptive effects_
- "NCvsC_Nullyear_TSH_Nov19.csv" _1 year data with only consumptive effects_
- "NCvsC_1year_TSH_EqualDetection_Nov29.csv" _1 year data with consumptive effects and non-consumptive effects and equal habitat detection set at 0.55_

5 Year Models
- "NCvsC_5year_TSH_Nov29.csv" _5 years data with consumptive effects and non-consumptive effects_
- "NCvsC_NULL_5year_TSH_Nov29.csv" _5 years data with only consumptive effects_


#### File List
- NCvsC_unlimited_Nov11.csv : prey die, the simulation runs until the prey is dead, simulation run on the cluster Nov 11
- NCvsC_1year_Nov11.csv :prey die, the simulation runs for 1 year, simulation run on the cluster Nov 11
- NCvsC_5years_Nov11.csv :prey die, the simulation runs for 5 years, simulation run on the cluster Nov 11
- NCvsC_everymonth_Nov1o.csv :prey die, the simulation runs for 10 years but gives an output every month, simulation run on the cluster on Nov 10
- HazardRatioSummary_Feb2022.csv : summary file for the survival hazard ratio models for each model condition
- NCEvNullSummary.csv : summary file with median survival, 95% CI on median survival, p-value for difference between CE and CE + NCE models
