En esta carpeta están las estimaciones obtenidas de la encuesta en España a nivel de provincia. Hay un archivo por provincia y un archivo por día con todas las provincias.

Las estimaciones se calculan periódicamente (y automáticamente) a partir de las respuestas recogidas con la encuesta accesible en https://survey.coronasurveys.org/ES. Estas respuestas están accesibles en https://github.com/GCGImdea/coronasurveys/blob/master/data/aggregate/ES-aggregate.csv. El programa usado para calcular la estimación está disponible en https://github.com/GCGImdea/coronasurveys/blob/master/code/script-provinces-daily.R.

Las estimaciones de una provincia se calculan para cada día, usando las 100 respuestas más recientes (o todas las del día). Para las estimaciones de casos recientes (de los últimos 14 días) se usan solamente las respuestas de los últimos 7 días. Para el resto se consideran todas las respuestas, independientemente de la fecha. Si hay varias respuestas con la misma cookie, se usa la respuesta mas reciente. 

La estimaciones están en ficheros CSV con las siguientes columnas (entre corchetes se dan las columnas que dan el intervalo de confianza al 95%):

- date: Fecha de la estimatión.
- region: Código ISO de la provincia.
- regionname: Nombre de la provincia.
- sample_size: Número de respuestas usadas en las estimaciones totales. No incluye encuestas con valores atípicos (outliers).
- reach: Número total de personas conocidas en esas respuestas.
- sample_size_recent: Número de respuestas usadas en las estimaciones recientes. No incluye encuestas con valores atípicos (outliers).
- reach_recent: Número total de personas conocidas en esas respuestas.
- cases_est [cases_low, cases_high]: Número acumulado estimado de casos de COVID-19.
- recentcases_est [recentcases_low, recentcases_high]: Número estimado de casos que empezaron a mostrar síntomas en los últimos 7 días.
- fatalities_est [fatalities_low, fatalities_high]: Número acumulado estimado de muertes de COVID-19.
- p_cases [p_cases_low, p_cases_high]: Prevalencia estimada. Se calcula dividiendo los casos reportados por las personas conocidas (reach).
- p_fatalities [p_fatalities_low, p_fatalities_high]: Estimación de tasa de fallecimientos sobre casos reportados. Se calcula dividiendo los fallecimientos por los casos reportados.
- p_recentcases [p_recentcases_low, p_recentcases_high]: Estimación de tasa de casos que empezaron a mostrar síntomas en los últimos 7 días.
- p_stillsick [p_stillsick_low, p_stillsick_high]: Estimación de tasa de casos que están todavía enfermos.
- population: Población del área.
