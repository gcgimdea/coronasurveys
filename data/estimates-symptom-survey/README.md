The data in these folders is derived from the non-aggregated COVID-19 World Symptoms Survey Microdata https://covidmap.umd.edu/fbsurvey/

- aggregated-data contains aggegated data obtained directly from the microdata.
- The rest of folders contain processed data derived from the former

These are the folder description:
- The *PlotData* folder has estimates at the country level.
- The *region* folder has estimates at the regional level.
- The *age* folder has estimates at the country level per age group.: 
  * 1: 18-24 years
  * 2: 25-34 years 
  * 3: 35-44 years 
  * 4: 45-54 years 
  * 5: 55-64 years 
  * 6: 65-74 years 
  * 7: 75 years or older
  * -99/-77: not provided or invalid

The estimates are obtained from the UMD Survey Data (https://covidmap.umd.edu/), in particular from the COVID-19 World Symptoms Survey Microdata (https://covidmap.umd.edu/fbsurvey/).

Each row of the tables contains (some may not be available):
- date
- ISO2
- country
- region
- ISO3
- population
- cli: Number of responses "that have reported COVID-like illness (CLI). COVID-like illness: fever, along with cough, shortness of breath, or difficulty breathing."
- cliWHO: Number of responses that report fever, cough and fatigue (from https://www.who.int/news-room/q-a-detail/coronavirus-disease-covid-19)
- anosmia: Number of YES resposes to the question "In the last 24 hours, have you had any of the following? Loss of smell or taste"
- count: Total number of responses considered for the cli and anosmia values.
- cli_local: Sum of the number of reported cases with CLI symptoms in local community. These are the cases reported by answering YES to the question (1) "Do you personally know anyone in your local community who is sick with a fever and either a cough or difficulty breathing?" and answering the question (2) "How many people do you know with these symptoms?"
- count_local: Total number of responses considered for the cli_local value. These are those answering NO to (1) and those answering YES to (1) and answering (2).
- cli_Xdays: Sum of the cli value of previous X days.
- cliWHO_Xdays: Sum of the cliWHO value of previous X days.
- anosmia_Xdays: Sum of the anosmia value of previous X days.
- count_Xdays: Sum of the count value of previous X days.
- cli_local_Xdays: Sum of the cli_local value of previous X days.
- count_local_Xdays: Sum of the count_local value of previous X days.
- p_cli_X, p_cli_X_low, p_cli_X_high: Ratio and 95% confidence interval of cli_X/count_X.
- p_cliWHO_X, p_cliWHO_X_low, p_cliWHO_X_high: Ratio and 95% confidence interval of cliWHO_X/count_X.
- p_anosmia_X, p_anosmia_X_low, p_anosmia_X_high: Ratio and 95% confidence interval of anosmia_X/count_X.
- p_cli_local_X, p_cli_local_X_low, p_cli_local_X_high: Ratio and 95% confidence interval of cli_local_X/ ( reach * count_local_X ). The value of reach used in 53, 
the average of the reach declared (https://coronasurveys.org/participation/)



