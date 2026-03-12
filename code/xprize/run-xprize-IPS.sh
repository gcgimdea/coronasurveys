#/bin/bash

outfile="out.$$"
datapath="./data/"

predictionspath="./predictions/"
predictorpath="./standard_predictor/"
outputpath="../../data/xprize/cs-tasks/"

rscriptspath="./R-scripts/"

starthistory="2020-12-01" # Date from which the IPS files start
startdate="2021-01-01" # Date from which predictions start
today=`date +%Y-%m-%d` # Date from which prescriptions start
enddate=`date -v+89d +%Y-%m-%d`  # Last date considered (today plus 89 days)

# Generate the IPS file from the Oxford repository, propagating the last IP until enddate

echo --- Downloading and completing IPS file from $starthistory to $enddate

Rscript --vanilla ${rscriptspath}complete-IPS.R "$starthistory" "$enddate" 
# > $outfile 2>&1


# Run predictor with real IPS
echo --- Running predictor with real IPS from $startdate to $enddate

python ${predictorpath}predict.py -s "$startdate" -e "$enddate" \
  -ip ${datapath}IPS-latest-full.csv -o ${predictionspath}real-IPS-predictions.csv 
# >> $outfile 2>&1

echo -- Adding fatalities, hospital, ICU, cost, and interventions to real IPS predictions from $startdate to $enddate

echo --- With weights fixed_equal_costs.csv

Rscript --vanilla ${rscriptspath}add-deaths-hospital-cost-IPS.R "$startdate" "$enddate" \
  ${predictionspath}real-IPS-predictions.csv ${outputpath}real-IPS-predictions-fixed_equal_costs.csv \
  ${datapath}IPS-latest-full.csv ${datapath}fixed_equal_costs.csv 
# >> $outfile 2>&1


echo --- With weights uniform_random_costs.csv

Rscript --vanilla ${rscriptspath}add-deaths-hospital-cost-IPS.R "$startdate" "$enddate" \
  ${predictionspath}real-IPS-predictions.csv ${outputpath}real-IPS-predictions-uniform_random_costs.csv \
  ${datapath}IPS-latest-full.csv ${datapath}uniform_random_costs.csv 
# >> $outfile 2>&1
