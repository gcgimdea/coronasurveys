#/bin/bash

datapath="../data/"
predictionspath="./predictions/"
prescriptpath="./prescriptions/"
outputpath="../../../data/xprize/xp-blindgreedy-tasks/"

predictorpath="../standard_predictor/"
rscriptspath="../R-scripts/"

starthistory="2020-12-01" # Date from which the IPS files start
startdate="2021-01-01" # Date from which predictions start
today=`date +%Y-%m-%d` # Date from which prescriptions start
enddate=`date -v+89d +%Y-%m-%d`  # Last date considered (today plus 89 days)


# Run prescriptor from today to enddate

echo --- Running prescriptor $today $enddate

echo --- With weights fixed_equal_costs.csv

python prescribe.py -s $today -e "$enddate" -ip ${datapath}IPS-latest-full.csv \
  -c ${datapath}fixed_equal_costs.csv -o ${prescriptpath}fixed_equal_costs.csv

echo --- With weights uniform_random_costs.csv

python prescribe.py -s $today -e "$enddate" -ip ${datapath}IPS-latest-full.csv \
  -c ${datapath}uniform_random_costs.csv -o ${prescriptpath}uniform_random_costs.csv


# Run the predictions for each prescriptor

for i in 0 1 2 3 4 5 6 7 8 9
do
  echo --- Preparing IPS for predictor $i for $startdate $today $enddate

  echo --- With weights fixed_equal_costs.csv

  Rscript ${rscriptspath}prepare-prediction.R $startdate $today $enddate ${datapath}IPS-latest-full.csv \
    ${prescriptpath}fixed_equal_costs.csv $i ${prescriptpath}fixed_equal_costs-${i}.csv


  echo --- With weights uniform_random_costs.csv

  Rscript ${rscriptspath}prepare-prediction.R $startdate $today $enddate ${datapath}IPS-latest-full.csv \
    ${prescriptpath}uniform_random_costs.csv $i ${prescriptpath}uniform_random_costs-${i}.csv

   
  echo --- Running predictor $i from $today to $enddate

  echo --- With weights fixed_equal_costs.csv

  python ${predictorpath}predict.py -s "$today" -e "$enddate" \
    -ip ${prescriptpath}fixed_equal_costs-${i}.csv -o ${predictionspath}fixed_equal_costs-${i}.csv

  echo --- With weights uniform_random_costs.csv

  python ${predictorpath}predict.py -s "$today" -e "$enddate" \
    -ip ${prescriptpath}uniform_random_costs-${i}.csv -o ${predictionspath}uniform_random_costs-${i}.csv


  echo -- Adding fatalities, hospital, ICU, Cost

  echo --- With weights fixed_equal_costs.csv 

  Rscript ${rscriptspath}add-deaths-hospital-cost.R ${predictionspath}fixed_equal_costs-${i}.csv ${outputpath}fixed_equal_costs-${i}.csv \
    $i ${prescriptpath}fixed_equal_costs.csv ${datapath}fixed_equal_costs.csv

  echo --- With weights uniform_random_costs.csv 

  Rscript ${rscriptspath}add-deaths-hospital-cost.R ${predictionspath}uniform_random_costs-${i}.csv ${outputpath}uniform_random_costs-${i}.csv \
    $i ${prescriptpath}uniform_random_costs.csv ${datapath}uniform_random_costs.csv 

  echo -- Summarizing
  echo --- With weights fixed_equal_costs.csv

  Rscript ${rscriptspath}performance-summary.R ${outputpath}fixed_equal_costs-${i}.csv \
    ${outputpath}fixed_equal_costs-summary-${i}.csv $i

  echo --- With weights uniform_random_costs.csv 

  Rscript ${rscriptspath}performance-summary.R ${outputpath}uniform_random_costs-${i}.csv \
    ${outputpath}uniform_random_costs-summary-${i}.csv $i
done

