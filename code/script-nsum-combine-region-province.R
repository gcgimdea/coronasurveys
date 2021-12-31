# <!-- Copyright {{ 2021 }} {{ IMDEA Networks Institute }} -->
# <!-- Author {{ Ananth Venkatesh, Antonio Fernández Anta }} {{https://coronasurveys.org/}} -->
# <!-- Licensed under the Apache License, Version 2.0 (the "License"); -->
# <!-- you may not use this file except in compliance with the License. -->
# <!-- You may obtain a copy of the License at -->
#
# <!-- http://www.apache.org/licenses/LICENSE-2.0 -->
#
# <!-- Unless required by applicable law or agreed to in writing, software -->
# <!-- distributed under the License is distributed on an "AS IS" BASIS, -->
# <!-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express -->
# <!-- or implied. See the License for the specific language governing -->
# <!-- permissions and limitations under the License. -->

#!/usr/bin/env Rscript
# USAGE: `Rscript script-nsum-combine-region-province.R`
#        (1) set `countries` to a character vector of country codes to fetch
#            e.g. `countries <- c('ES', 'FR', 'IT')`
#        (2) set `region_types` to the type of region to fetch;
#            regions will then be detected automatically for each country
#            e.g. `region_types <- c('Autonomous community', 'Metropolitan region', 'Region')`
# INPUT: ../data/estimates-nsum/provinces/{countrycode}/{provincecode}-estimate.csv
# INPUT: ../data/estimates-nsum/regions-no-province/{countrycode}/{regioncode}-estimate.csv
# OUTPUT: ../data/estimates-nsum/regions/{countrycode}/{regioncode}-estimate.csv


# This script fetches the necessary data from 
# (1) ../data/common-data/regions-tree-population.csv
# (2) ../data/common-data/provinces-tree-population.csv
# (3) ../data/estimates-nsum/provinces/{countrycode}/{provincecode}-estimate.csv *1
# (4) ../data/estimates-nsum/regions-no-province/{countrycode}/{regioncode}-estimate.csv *2
#
# [*1] This step is repeated for all of the provinces in the country.
# [*2] This step is repeated for all of the regions in the country.
#
# and merges the province estimates with the regional estimates for all 
# the parameters in the files provided. Only statistics that are 
# available at the regional and provincial levels are merged. These include:
#
# date                  region                  population 
# p_cases               p_cases_error           p_fatalities 
# p_fatalities_error    p_recentcases           p_recentcases_error 
# p_cases_daily         p_cases_daily_error     p_stillsick 
# p_stillsick_error
#
# If any of these values cannot be obtained for a specific date, its 
# corresponding column will have an 'NA' marker.
#
# This includes:
# (a) a list of all the region codes in the country from (1)
#     => regioncode     population
#
# (b) a list of all the province codes for each region in the country from (2)
#     => regioncode     provincecode        population
#
# (c) a list of required data for each province from (3)
#     => region     p_cases     ...
#
# (d) a list of required data for each region from (4)
#     => region     p_cases     ...

# Debugging Notes:
# Since this script takes a long time to run, changes can be previewed by 
# running this script for only one region. Modify the main routine to use only 
# the `process_region()` function and choose one region (e.g. 'ESPV') to run. 
# That way, the script will run much faster since there is much less data to 
# fetch and process.

# load libraries
library(data.table)

estimates_path <- "../data/estimates-nsum/regions/"
regions_path <- "../data/estimates-nsum/regions-no-province/"
provinces_path <- "../data/estimates-nsum/provinces/"
regions_tree_file <- "../data/common-data/regions-tree-population.csv"
provinces_tree_file <- "../data/common-data/provinces-tree-population.csv"

countries <- c('ES', 'FR', 'IT')

# data loaders & helper functions
load_regioncodes <- function(args.countrycode, args.regiontype) {  # get (1)
    
    data <- fread(regions_tree_file)
    filtered_data <- data[countrycode == args.countrycode & 
                          regiontype %in% args.regiontype][, c(2, 5)]
    filtered_data
    
}

load_provincecodes <- function(args.countrycode) {  # get (2)

    data <- fread(provinces_tree_file)
    filtered_data <- data[countrycode == args.countrycode][, c(2, 3, 5)]
    filtered_data

}

load_province_data <- function(args.countrycode, 
                               args.provincecodes) {  # get (3)

    data <- list()
    for (i in 1:nrow(args.provincecodes)) {

        provincecode <- args.provincecodes[i]$provincecode
        province_data <- fread(paste(provinces_path, 
                            args.countrycode, '/', 
                            provincecode, '-estimate.csv', 
                            sep=''))
        filtered_data <- province_data[, c(1, 9:10, 12:19)]
        data[[provincecode]] <- filtered_data

    }

    data

}

load_region_data <- function(args.countrycode, args.regioncode) {  # get (4)
    
    data <- fread(paste(regions_path, 
                        args.countrycode, '/', 
                        args.regioncode, '-estimate.csv', 
                        sep=''))
    filtered_data <- data[, c(1, 8:17)]
    filtered_data

}

make_csv <- function(countrycode, regioncode, data) {
    
    # directory setup
    # setwd(estimates_path)
    country_path <- paste0(estimates_path, countrycode)
    if (!dir.exists(country_path)) dir.create(country_path)

    # dump to csv
    write.csv(data, 
              paste(country_path, '/', regioncode, '-estimate.csv', sep=''), 
              quote=FALSE)

}


# data processors
process_region <- function(countrycode, regioncode, population, provincecodes) {

    # fetch data
    estimates_region <- load_region_data(countrycode, regioncode)
    estimates_provinces <- load_province_data(countrycode, provincecodes)

    # combine data
    combined_data <- data.table()
    for ( region in 1:nrow(estimates_region) ) {
        
        # get region-level info (prefixed with `r.`)
        r.data <- estimates_region[region, 2:11]
        r.date <- estimates_region[region, 1]$date

        # initialize data aggregators (prefixed with `a.`)
        # as new data is collected, it is weighted according to its 
        # population and combined with the aggregated total
        # data with a value of `NA` is excluded

        a.data <- data.table(
            p_cases = 0,
            p_cases_error = 0,
            p_fatalities = 0,
            p_fatalities_error = 0,
            p_recentcases = 0,
            p_recentcases_error = 0,
            p_cases_daily = 0,
            p_cases_daily_error = 0,
            p_stillsick = 0,
            p_stillsick_error = 0
        )
        # corresponding aggregate populations for each piece of data
        a.populations <- data.table(
            p_cases = 0,
            p_cases_error = 0,
            p_fatalities = 0,
            p_fatalities_error = 0,
            p_recentcases = 0,
            p_recentcases_error = 0,
            p_cases_daily = 0,
            p_cases_daily_error = 0,
            p_stillsick = 0,
            p_stillsick_error = 0
        )

        for (l.i in 1:10) {
            l.item <- r.data[[l.i]]
            if ( !is.na(l.item) ) {
                a.data[[l.i]] <- population * r.data[[l.i]]
                a.populations[[l.i]] <- population
            }
        }
        
        # get province-level info (prefixed with `p.`)
        for ( province in 1:nrow(provincecodes) ) {

            p.name <- provincecodes[province]$provincecode
            p.population <- provincecodes[province]$population
            # only fetch data with the same data as the region
            p.data <- estimates_provinces[[p.name]][date == r.date, 2:11]
            
            if ( nrow(p.data) != 1 ) break  # skip if no data is available

            for (l.i in 1:10) {
                l.item <- p.data[[l.i]]
                if ( !is.na(l.item) ) {
                    a.data[[l.i]] <- a.data[[l.i]] + p.population * p.data[[l.i]]
                    a.populations[[l.i]] <- a.populations[[l.i]] + p.population
                }
            }
            
        }
        
        # calculate weighted average
        a.data <- a.data / a.populations
        a.data$date <- r.date
        
        for (l.i in 1:11) {  # replace `NaN` with `NA`
            l.item <- a.data[[l.i]]
            if ( is.nan(l.item) ) a.data[[l.i]] <- NA
        }

        combined_data <- data.table(
            date = c(combined_data$date, a.data$date),
            p_cases = c(combined_data$p_cases, a.data$p_cases),
            p_cases_error = c(combined_data$p_cases_error, a.data$p_cases_error),
            p_fatalities = c(combined_data$p_fatalities, a.data$p_fatalities),
            p_fatalities_error = c(combined_data$p_fatalities_error, a.data$p_fatalities_error),
            p_recentcases = c(combined_data$p_recentcases, a.data$p_recentcases),
            p_recentcases_error = c(combined_data$p_recentcases_error, a.data$p_recentcases_error),
            p_cases_daily = c(combined_data$p_cases_daily, a.data$p_cases_daily),
            p_cases_daily_error = c(combined_data$p_cases_daily_error, a.data$p_cases_daily_error),
            p_stillsick = c(combined_data$p_stillsick, a.data$p_stillsick),
            p_stillsick_error = c(combined_data$p_stillsick_error, a.data$p_stillsick_error)
        )
        
    }

    # dump to csv
    make_csv(countrycode, regioncode, combined_data)
    print(paste('Created ', 
                countrycode, '/', 
                regioncode, '-estimate.csv', 
                sep=''))

    # cleanup for next region*
    rm(combined_data, 
       estimates_region, 
       estimates_provinces)

    # *Saves memory on resource-constrained systems.

}

process_country <- function(countrycode, regiontype) {
    
    # fetch data
    regioncodes <- load_regioncodes(countrycode, regiontype)
    provincecodes <- load_provincecodes(countrycode)
    
    # call routine
    for (i in 1:nrow(regioncodes)) {
        c.regioncode <- regioncodes[i, ]
        process_region(countrycode, c.regioncode$regioncode, 
                       as.numeric(c.regioncode$population), 
                       provincecodes[regioncode == c.regioncode$regioncode]
                       [, c(2, 3)])
    }
    
}


# main routine
# countries <- c('ES', 'FR', 'IT')
region_types <- list(
    ES=c('Autonomous community', 'Autonomous city'), 
    FR=c('Metropolitan region'), 
    IT=c('Region')
)

for (i in 1:length(countries)) {
    process_country(countries[i], region_types[[i]])
}
