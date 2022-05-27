# PredPrey-HabitatDomains

#### Collaborators:
- Kaggie Orrick
- Freya Rowland
- Nathalie Sommer
- Kristy Ferraro

## Current figures

For each of these, the p-value is survival difference between the consumptive (null) and non-consumptive (NCE) models. The vertical lines show the median survival of each curve. Suggestions on colors or tweaks are welcome. All code is under [RScripts/StatsModels_survival_v2.R](RScripts/StatsModels_survival_v2.R)

# Active survival
![Figure 1](Output_Figures/ActivePredSurv.png)

# Sit-and-pursue survival
![Figure 3](Output_Figures/SPPredSurv.png)

# Sit-and-wait survival
![Figure 2](Output_Figures/SWPredSurv.png)


## Five year NCE data hazard tables

[Explanation of hazard ratio table](<https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#Estimating_survival_curves_with_the_Kaplan-Meier_method>)

The quantity of interest from a Cox regression model is a hazard ratio (HR). The HR represents the ratio of hazards between two groups at any particular point in time.
The HR is interpreted as the instantaneous rate of occurrence of the event of interest in those who are still at risk for the event. It is not a risk, though it is commonly interpreted as such.

If you have a regression parameter β (from column estimate in our coxph) then HR = exp(β).

A **HR < 1 indicates reduced hazard of death** whereas a **HR > 1 indicates an increased hazard of death**.
So a HR = 0.59 implies that around 0.6 times as many females are dying as males, at any given time.

**For reference:
- propHabitat = proportion of time spent on white vs. black spaces = **habitat shift**
- propPredFree = proportion of time spent in hrs where predators are not active = **time shift**
- propSafeSpace = proportional habitat shift to areas where predators are excluded (so only in Prey.Start.Con = Large and Pred.Start.Con = Small) = **space shift**




### The hazard ratios of each predator strategy/predator domain/prey domain combination

All hazard ratio data for each combination are summarized in [HazardRatioSummary_Feb2022.csv](Data/HazardRatioSummary_Feb2022.png)

# Hazard ratio summary figure
![HRFigure](Output_Figures/HazardRatiosPlot.png)

Hazard ratio summary figure. HR = 1 (no benefit or detriment) is dotted. I think we should do something like this.

# Behavioral shifts at one year
![oneyrshifts](Output_Figures/OneYrShifts.png)

To make tweaks: https://mjskay.github.io/ggdist/reference/stat_halfeye.html

# Behavioral shifts at five years
![fiveyrshifts](Output_Figures/FiveYrShifts.png)

