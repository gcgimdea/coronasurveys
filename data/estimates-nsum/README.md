Estimates obtained from the CoronaSurveys responses in data/aggregate.

./estimates-nsum/provinces has estimates for the provinces of ES, FR and IT, computed from data/aggregate by script-nsum-provinces.R.
./estimates-nsum/regions-no-province has estimates for the regions of ES, FR and IT, computed from data/aggregate by script-nsum-regions-no-province.R.
./estimates-nsum/regions has estimates for all the regions. 
- For ES, FR and IT they are obtained by combination of the two above by script-nsum-combine-region-province.R. 
- For the rest they are computed from data/aggregate by script-nsum-regions.R.
./estimates-nsum/PlotData has estimates for all the countries. They are obtained from the regional estimates.
- For ES, FR and IT are computed by script-nsum-country-region-1.R.
- For the rest they are computed by script-nsum-country-region-2.R.
