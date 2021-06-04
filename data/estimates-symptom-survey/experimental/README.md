Aggregated data from the responses to the COVID-19 World Symptoms Survey https://covidmap.umd.edu/index.html.

There are 3 folders:
- *country*: Aggregation is done daily at the country level.
- *age*: Aggregation is done daily at the country level per age groups.
  - 1: 18-24 years
  - 2: 25-34 years
  - 3: 35-44 years
  - 4: 45-54 years
  - 5: 55-64 years
  - 6: 65-74 years
  - 7: 75 years or older
- *region*: Aggregation is done daily at the country level per region of the country.

Each line is the aggregation of at least 100 responses (the number of resposses aggregated can be found in column *count*). 
This may require including responses from previous dates. *first_date* reports the date of the oldest such response.

The estimates of the ratio of the population infected is provided in columns:
- *p_cli* (*p_cli_CI* is the width of the confidence interval): Ratio of responses that have reported COVID-like illness (CLI). 
COVID-like illness: fever, along with cough, shortness of breath, or difficulty breathing. Obtained as *cli/count*.
- *p_cli_weight* (*p_cli_weight_CI*): Ratio *p_cli* weighted (see https://arxiv.org/abs/2009.14675). Obtained as *cli_weight/weight*.
- *p_cliWHO* (*p_cliWHO_CI*): Ratio of responses that have reported fever, cough, and fatigue (https://www.who.int/news-room/q-a-detail/coronavirus-disease-covid-19). Obtained as *cliWHO/count*.
- *p_cliWHO_weight* (*p_cliWHO_weight_CI*): Ratio *p_cliWHO* weighted. Obtained as *cliWHO_weight/weight*.
- *p_cli_local* (*p_cli_local_CI*): Ratio of reported cases with CLI symptoms via indirect reporting. 
These are the cases reported by answering YES to the question (1) “Do you personally know anyone in your local community who is sick 
with a fever and either a cough or difficulty breathing?” and answering the question (2) “How many people do you know with these symptoms?” 
The ratio is obtained by dividing the number of cases by the number of responses multiplied by the estimated average “reach”: 71 is being used.
Obtained as *cli_local/(count_local * 71)*.

Columns B1_1.1 to E2.NA correspond to the responses to the corresponding survey questions. For instance, the responses to question B1_1 ("In the last 24 hours, have you had any of the following? Fever") are aggregated in columns:
- B1_1.1: Number of responses that answered 1 (=yes).
- B1_1.2: Number of responses that answered 2 (=no).
- B1_1.NA: Number of responses that did not answered.

Columns B2.mean, B2.sd, B4.mean, B4.sd, E5.mean, E5.sd are the mean and standard deviation of the responses to the corresponding question.

Before aggregation the individual responses have been filtered to remove outliers (e.g., responses that report knowing millions of people with CLI symptoms). 
Responses have been removed if they satisfy any of the following:
- Report having all symptoms in questions B1_1 to B1_12.
- Report at least one symptom and report a value larger than 100 in question B2: "For how many days have you had at least one of these symptoms?"
- Answer "yes" to question B3 "Do you personally know anyone in your local community who is sick with a fever and either a cough or difficulty breathing?" and report a
value larger than 100 in question B4: "How many people do you know with these symptoms?"
- Report a value larger than 100 in question E5: "How many people slept in the place where you stayed last night?"





