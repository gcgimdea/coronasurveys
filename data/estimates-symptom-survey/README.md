NOTE: November 10th, 2021. Data will no longer be available for Afghanistan after 11/9/2021. Since the data is not of sufficient quality to recommend for use, historical data will also no longer be available. If any of your partners are using the data in Afghanistan, please do inform them. UMD Global CTIS Team

**Description:**

The data in these folders is derived from the non-aggregated COVID-19 World Symptoms Survey Microdata https://covidmap.umd.edu/fbsurvey/. 

**All values aggregate at least 100 responses**

**WARNING: If on a day there are less than 100, additional responses will be used from previous days. Column first_date indicates the date of the oldest response used.**

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
- ISO2
- ISO_3
- date
- first_date: date of the oldest response aggregated.
- country_agg
- region_agg
- population
- count: Total number of responses aggregated.
- p_cli: Ratio of responses "that have reported COVID-like illness (CLI). COVID-like illness: fever, along with cough, shortness of breath, or difficulty breathing."
- p_cli_weight: p_cli weighted by the representativeness of each response.
- p_cliWHO: Ratio of responses that report fever, cough and fatigue (from https://www.who.int/news-room/q-a-detail/coronavirus-disease-covid-19)
- p_cliWHO_weight: p_cli weighted by the representativeness of each response.
- p_cli_local: Ratio P_cli but obtained from of the number of reported cases with CLI symptoms in local community. These are the cases reported by answering YES to the question (1) "Do you personally know anyone in your local community who is sick with a fever and either a cough or difficulty breathing?" and answering the question (2) "How many people do you know with these symptoms?"

The signals ending in _CI are the width of the 95% confidence interval.
The signals ending in _smooth are the smoothed values.
The signals ending with _smooth_slope are the slope of the linear regression of the latest 7 days of the smoothed curve.



