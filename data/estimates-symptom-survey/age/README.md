Estimates for each age group at the country level obtained from the UMD Survey Data (https://covidmap.umd.edu/), in particular from the COVID-19 World Symptoms Survey Microdata (https://covidmap.umd.edu/fbsurvey/).

Each row of the tables contains:
- date
- age. This column can take the following values:
-- 1: 18-24 years
-- 2: 25-34 years 
-- 3: 35-44 years 
-- 4: 45-54 years 
-- 5: 55-64 years 
-- 6: 65-74 years 
-- 7: 75 years or older
-- -99/-77: not provided or invalid
- ISO2
- country
- ISO3
- population
- cli: Number of responses "that have reported COVID-like illness (CLI). COVID-like illness: fever, along with cough, shortness of breath, or difficulty breathing."
- count: Total number of responses considered for the cli value.
- cli_local: Sum of the number of reported cases with CLI symptoms in local communities.
- count_local: Total number of responses considered for the cli_local value.
- cli_7days, cli_14days: Average of the cli value of previous 7 and 14 days.
- count_7days, count_14days: Average of the count value of previous 7 and 14 days.
- cli_local_7days, cli_local_14days: Average of the cli_local value of previous 7 and 14 days.
- count_local_7days, count_local_14days: Average of the count_local value of previous 7 and 14 days.
- p_cli_X, p_cli_X_low, p_cli_X_high: Ratio and 95% confidence interval of cli_X/count_X.
- p_cli_local_X, p_cli_local_X_low, p_cli_local_X_high: Ratio and 95% confidence interval of cli_local_X/ ( reach * count_local_X ). The value of reach used in 53, 
the average of the reach declared (https://coronasurveys.org/participation/)
