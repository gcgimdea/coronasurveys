#

Run () {
  echo
  date
  echo "Running $1"
  Rscript $1
  date
}

# Run script-get-oxford-data.R
# Run script-get-jhu-data-v2.R
# Does not need the JHU nor Oxford, taking the data from OWID

Run script-confirmed5.R 

# UMD estimates from the API
# Run script-umd_batch_symptom_country.R
# Run script-umd_batch_symptom_region.R
# UMD estimates from the microdata

Run script-umd-country-v5.R
Run script-umd-region-v4.R
Run script-umd-age-v3.R

# Run script-umd-regions-plot.R

# Run script-30responses.R

Run script-300responses-v2.R

# Run script-W-alpha.R
# Run script-W.R

# date

Run script-nsum-provinces.R
Run script-nsum-regions-no-province.R
Run script-nsum-combine-region-province.R
Run script-nsum-regions.R
Run script-nsum-country-region-1.R
Run script-nsum-country-region-2.R

#Run script-nsum-provinces-map.R
Run script-nsum-provinces-plot.R

# The estimates based on fatalities use the Oxford official data. Has to be changes if using JHU
Run script-ccfr-fatalities-country.R

Run script-ccfr-based-v4.R
# Run script-ES-ccfr-based.R

Run script-rivas-arganda-daily.R

# Run script-liverpool-daily.R

Run participation-ranking-v2.R

# Run corona_surveys_estimate3.R
