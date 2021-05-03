Estimates at the regional level obtained from the UMD Survey Data (https://covidmap.umd.edu/), in particular from the COVID-19 World Symptoms Survey Microdata (https://covidmap.umd.edu/fbsurvey/).

Each row of the tables contains:
- date
- ISO2
- country
- region
- ISO3
- population
- cli: Number of responses "that have reported COVID-like illness (CLI). COVID-like illness: fever, along with cough, shortness of breath, or difficulty breathing."
- anosmia: Number of YES resposes to the question "In the last 24 hours, have you had any of the following? Loss of smell or taste"
- count: Total number of responses considered for the cli and anosmia values.
- cli_local: Sum of the number of reported cases with CLI symptoms in local community. These are the cases reported by answering YES to the question (1) "Do you personally know anyone in your local community who is sick with a fever and either a cough or difficulty breathing?" and answering the question (2) "How many people do you know with these symptoms?"
- count_local: Total number of responses considered for the cli_local value. These are those answering NO to (1) and those answering YES to (1) and answering (2).
- cli_14days: Average of the cli value of previous 14 days.
- anosmia_14days: Average of the anosmia value of previous 14 days.
- count_14days: Average of the count value of previous 14 days.
- cli_local_14days: Average of the cli_local value of previous 14 days.
- count_local_14days: Average of the count_local value of previous 14 days.
- p_cli_X, p_cli_X_low, p_cli_X_high: Ratio and 95% confidence interval of cli_X/count_X.
- p_anosmia_X, p_anosmia_X_low, p_anosmia_X_high: Ratio and 95% confidence interval of anosmia_X/count_X.
- p_cli_local_X, p_cli_local_X_low, p_cli_local_X_high: Ratio and 95% confidence interval of cli_local_X/ ( reach * count_local_X ). The value of reach used in 53, 
the average of the reach declared (https://coronasurveys.org/participation/)
