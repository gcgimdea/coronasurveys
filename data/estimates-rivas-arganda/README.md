
En esta carpeta están las estimaciones obtenidas de la encuesta lanzada en Rivas y Arganda del Rey el 30 de julio de 2020.

Hay un archivo por área geográfica:
- ESMRC		Rivas Barrio Centro
- ESMRE		Rivas Barrio Este
- ESMRO		Rivas Barrio Oeste
- ESMAR		Arganda del Rey

Las estimaciones se calculan periódicamente (y automáticamente) a partir de las respuestas recogidas con la encuesta accesible en https://survey.coronasurveys.org/RA. Estas respuestas están accesibles en https://github.com/GCGImdea/coronasurveys/tree/master/data/aggregate/rivas-arganda. El programa usado para calcular la estimación está disponible en https://github.com/GCGImdea/coronasurveys/blob/master/code/script-rivas-arganda-daily.R.

Las estimaciones de un área se calculan para cada día. Si hay al menos **1000 respuestas,** se usan todas las respuestas del día. Si no hay 1000 respuestas ese día, se usan
respuestas de los **6 días anteriores** (7 días en total) hasta completar 1000 (si hay). En los valores acumulados (por ejemplo, el número total de casos o de fallecidos), si hay varias respuestas con la misma cookie sólo se usa la más reciente.

La estimaciones están en ficheros CSV con las siguientes columnas (entre corchetes se dan las columnas que dan el intervalo de confianza al 95%):
- date: Fecha de la estimatión
- sample_size: Número de respuestas usadas. No incluye encuestas con valores atípicos (outliers).
- reach: Número total de personas conocidas en esas respuestas.
- cases: Número de casos reportados en las respuestas usadas.
- recentcases: Número de casos de los últimos 7 días reportados en las respuestas usadas.
- fatalities: Número de fallecimientos reportados en las respuestas usadas.
- cases_est [cases_low, cases_high]: Número acumulado estimado de casos de COVID-19.
- fatalities_est [fatalities_low, fatalities_high]: Número acumulado estimado de muertes de COVID-19.
- recentcases_est [recentcases_low, recentcases_high]: Número estimado de casos que empezaron a mostrar síntomas en los últimos 7 días.
- hospital_est [hospital_low, hospital_high]: Número acumulado estimado de casos que han sido trasladados al hospital por COVID-19.
- icu_est [icu_low, icu_high]: Número acumulado estimado de casos que han estado en la Unidad de Cuidados Intensivos (UCI).
- p_cases [p_cases_low, p_cases_high]: Prevalencia estimada. Se calcula dividiendo los casos reportados por las personas conocidas (reach).
- p_recovered [p_recovered_low, p_recovered_high]: Estimación de tasa de casos recuperados. Se calcula dividiendo los casos recuperados reportados por las personas conocidas (reach).
- p_fatalities [p_fatalities_low, p_fatalities_high]: Estimación de tasa de fallecimientos. Se calcula dividiendo los fallecimientos reportados por las personas conocidas (reach).
- p_recentcases [p_recentcases_low, p_recentcases_high]: Estimación de tasa de casos que empezaron a mostrar síntomas en los últimos 7 días.
- p_recenthospital [p_recenthospital_low, p_recenthospital_high]: Estimación de tasa de hospitalizados en los últimos 7 días.
- p_recentcasesnursing [p_recentcasesnursing_low, p_recentcasesnursing_high]: Estimación de tasa de casos empezaron a mostrar síntomas en los últimos 7 días y son residentes o trabajadores de una residencia de la tercera edad.
- p_stillsick [p_stillsick_low, p_stillsick_high]: Estimación de tasa de casos que están todavía enfermos.
- p_hospital [p_hospital_low, p_hospital_high]: Estimación de tasa de casos que han sido trasladados al hospital por COVID-19.
- p_severe [p_severe_low, p_severe_high]: Estimación de tasa de casos que han sido trasladados al hospital por COVID-19 y han sido casos graves.
- p_icu [p_icu_low, p_icu_high]: Estimación de tasa de casos que han estado en la Unidad de Cuidados Intensivos (UCI).
- p_tested [p_tested_low, p_tested_high]: Estimación de la tasa de población a la que se le han hecho pruebas de COVID-19.
- p_positive [p_positive_low, p_positive_high]: Estimación de la tasa de pruebas que fueron positivas.
- population: Población del área.
