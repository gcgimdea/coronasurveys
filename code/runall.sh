#

date

# Rscript script-get-oxford-data.R
# Rscript script-get-jhu-data-v2.R
# Does not need the JHU nor Oxford, taking the data from OWID
Rscript script-confirmed4.R 

date

# UMD estimates from the API
# Rscript script-umd_batch_symptom_country.R
# Rscript script-umd_batch_symptom_region.R
# UMD estimates from the microdata
Rscript script-umd-country-v5.R
Rscript script-umd-region-v4.R
Rscript script-umd-age-v3.R
# Rscript script-umd-regions-plot.R

date

# Rscript script-30responses.R
Rscript script-300responses-v2.R

date

# Rscript script-W-alpha.R
# Rscript script-W.R

# date

Rscript script-nsum-provinces.R
Rscript script-nsum-regions-no-province.R
Rscript script-nsum-combine-region-province.R
Rscript script-nsum-regions.R
Rscript script-nsum-country-region-1.R
Rscript script-nsum-country-region-2.R

Rscript script-nsum-provinces-map.R
Rscript script-nsum-provinces-plot.R

date

# The estimates based on fatalities use the Oxford official data. Has to be changes if using JHU
Rscript script-ccfr-fatalities-country.R

Rscript script-ccfr-based-v4.R
# Rscript script-ES-ccfr-based.R

Rscript script-rivas-arganda-daily.R

# Rscript script-liverpool-daily.R

Rscript participation-ranking-v2.R

# Rscript corona_surveys_estimate3.R

date

