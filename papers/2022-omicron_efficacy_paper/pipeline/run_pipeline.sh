#

process_quarter () {

  quarter=$1

  date
  echo "**** Rscript dates2microdata.R ${quarter}"
  Rscript dates2microdata.R ${quarter}

  date
  echo "**** Rscript microdata2total.R ${quarter}"
  Rscript microdata2total.R ${quarter}

  date
  echo "**** Rscript total2dummies.R ${quarter}"
  Rscript total2dummies.R ${quarter}

  date
  echo "**** Rscript model_rf_generation.R ${quarter}"
  Rscript model_rf_generation.R ${quarter}

  date
  echo "**** Rscript model_rf_symp_generation.R ${quarter}"
  Rscript model_rf_symp_generation.R ${quarter}

  date
  echo "**** Rscript model_Xgboost_generation.R ${quarter}"
  Rscript model_Xgboost_generation.R ${quarter}

  date
  echo "**** Rscript model_Xgboost_symp_generation.R ${quarter}"
  Rscript model_Xgboost_symp_generation.R ${quarter}

  date
  echo "**** Rscript dummies2aggregates.R ${quarter}"
  Rscript dummies2aggregates.R ${quarter}
}

# ----- main ----

export MC_CORES=10

process_quarter "2020-Q2"

process_quarter "2020-Q3"

process_quarter "2020-Q4"

process_quarter "2021-Q1"

process_quarter "2021-Q2"

process_quarter "2021-Q3"

process_quarter "2021-Q4"

process_quarter "2022-Q1"

process_quarter "2022-Q2"

date
