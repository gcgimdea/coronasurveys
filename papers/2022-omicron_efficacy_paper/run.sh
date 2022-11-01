
echo "*** Omicron presence computation since December 15th, 2021" script-variants-monthly.R

Rscript script-variants-monthly.R

echo "*** Data for Table 2" script-TPR.R

Rscript script-TPR.R

echo "*** Plots for South Africa" script-plots.R ZA 2021-06-18 2021-12-31

Rscript script-country-plots-data-create.R ZA 2021-06-18  2021-12-31
Rscript script-country-plots.R ZA

echo "*** Plot of vaccination in South Africa" script-vaccination-plot-ZA.R

Rscript script-vaccination-plot-ZA.R

echo "*** Efficacy tables for ZA" script-efficacy-ZA.R

Rscript script-efficacy-ZA.R

echo "*** Efficacy tables for Gauteng" script-efficacy-ZA-Gauteng.R

Rscript script-efficacy-ZA-Gauteng.R


echo "*** Generate efficacy data for all countries" script-efficacy-data-create.R

Rscript script-efficacy-data-create.R

echo "*** Generate efficacy plots for all countries" script-efficacy-plots.R

Rscript script-efficacy-plots.R

echo "Generate tables for all countries" script-efficacy-tables.R

Rscript script-efficacy-tables.R
