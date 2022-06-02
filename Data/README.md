## README
In this folder are the data for the different versions run on Netlogo and summarized data for models.

The most up-to-date files are: (From Nov 29 and 30 2021)
1 Year
- "NCvsC_1year_TSH_Nov29.csv" - 1 year data with consumptive effects and non-consumptive effects.
- "NCvsC_Nullyear_TSH_Nov19.csv" - 1 year data with only consumptive effects.
- "NCvsC_1year_TSH_EqualDetection_Nov29.csv" - 1 year data with consumptive effects and non-consumptive effects and equal habitat detection set at 0.55

5 Years
- "NCvsC_5year_TSH_Nov29.csv" - 5 years data with consumptive effects and non-consumptive effects.
- "NCvsC_NULL_5year_TSH_Nov29.csv" - 5 years data with only consumptive effects.



Any file with 'NCvsC' means that the model has included prey dying
Any file with 'NC' means that the model does not include prey dying (predators are always unsuccessful)

The second part of any name is the length a simulation was run: 1year, 5years or unlimited (until all prey die)

Last part of any name is the date that the simulation was run.

#### File List
- NCvsC_unlimited_Nov11.csv : prey die, the simulation runs until the prey is dead, simulation run on the cluster Nov 11
- NCvsC_1year_Nov11.csv :prey die, the simulation runs for 1 year, simulation run on the cluster Nov 11
- NCvsC_5years_Nov11.csv :prey die, the simulation runs for 5 years, simulation run on the cluster Nov 11
- NCvsC_everymonth_Nov1o.csv :prey die, the simulation runs for 10 years but gives an output every month, simulation run on the cluster on Nov 10
- HazardRatioSummary_Feb2022.csv : summary file for the survival hazard ratio models for each model condition
- NCEvNullSummary.csv : summary file with median survival, 95% CI on median survival, p-value for difference between CE and CE + NCE models
