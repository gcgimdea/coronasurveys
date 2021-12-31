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
# USAGE: `Rscript script-nsum-country-region-1.R`
# OUTPUT: ../data/estimates-nsum/PlotData//{countrycode}-estimate.csv


# This script fetches the necessary data from 
# (1) ../data/common-data/regions-tree-population.csv
# (2) ../data/estimates-nsum/regions/{countrycode}/{regioncode}-estimate.csv
#
# and adds* all the regional estimates together to generate estimates at the 
# national level. Only statistics that are available at the regional levels are 
# merged. These include:
#
# date                  region                  population 
# p_cases               p_cases_error           p_fatalities 
# p_fatalities_error    p_recentcases           p_recentcases_error 
# p_cases_daily         p_cases_daily_error     p_stillsick 
# p_stillsick_error
#
# *In the cases of values prefixed with a 'p_' (i.e. those representing 
# percentages), the values are combined using a weighted average with respect 
# to population.
#
# If any of these values cannot be obtained for a specific date, its 
# corresponding column will have an 'NA' marker. Conversely, if any of these 
# values cannot be obtained for a specific region at a specific date, the value
# will be ignored in the sum.
#
# This includes:
# (a) a list of all the region codes in the country from (1)
#     => regioncode     population
#
# (d) a list of required data for each region from (2)
#     => region     p_cases     ...


# imports
library(data.table); 'Imports successful'

regions_tree_file <- "../data/common-data/regions-tree-population.csv"
region_path <- "../data/estimates-nsum/regions/"
estimates_path <- "../data/estimates-nsum/PlotData/"

countries <- c("ES", "FR", "IT")

# resources
# rtp = regions-tree-population.csv
rtp <- fread(
    regions_tree_file
    )
region_types <- list(
    ES=c('Autonomous community', 'Autonomous city'), 
    FR=c('Metropolitan region'), 
    IT=c('Region')
)


# data getters
find_regions <- function(country) {

    # Produces an iterable containing all region codes 
    # for the country provided--uses (1)

    types <- region_types[[country]]
    rtp[countrycode == country & regiontype %in% types][, 2][[1]]

}

get_region_data <- function(country, region) {

    # Produces a data.table object containing all the data 
    # for the region provided--uses (2)

    fread(paste(
        region_path, country, '/', region, '-estimate.csv',
    sep=''))

}

get_regions <- function(countrycode) {

    # STRUCTURE:
    # find_regions (list country regions)
    # get_region_data (get regional data)

    regions <- find_regions(countrycode)
    data <- list()

    for (region in regions) {
        region.data <- get_region_data(countrycode, region)
        data[[region]] <- region.data
    }

    data

}

get_data <- function(countrycode) {

    # Produces a data.table object containing all the regional populations
    # for the country provided--uses (1)

    regions <- find_regions(countrycode)
    rtp[regioncode %in% regions][, c(2, 5)]

}


# data processing
combine_data <- function(data, regions) {

    # For each region:
    # (a) get the population
    # (b) strip the population and date columns
    # (c) add the population columns
    # (d) keep the date column
    # (e) multiply the remaining data by the region population and add
    
    # (a)
    region_names <- regions[, 1][[1]]
    region_populations <- as.numeric(regions[, 2][[1]])

    country_population <- sum(region_populations)  # (c)
    dates <- data[[1]]$date  # (d)

    all_data <- list()
    all_weights <- list()

    for (region in region_names) {

        region.data <- data[[region]][, c(3:12)]  # (b)

        weight <- as.numeric(regions[regioncode == region][, 2][[1]])
        weight_table <- copy(region.data)
        weight_table[!is.na(weight_table)] <- weight
        weight_table[is.na(weight_table)] <- 0
        all_weights[[region]] <- weight_table
        
        region.data[is.na(region.data)] <- 0
        all_data[[region]] <- region.data

    }

    # (e)
    data_sum <- copy(all_data[[1]]); data_sum[] <- 0
    weights_sum <- copy(all_weights[[1]]); weights_sum[] <- 0

    for (region in region_names) {
        data_sum <- data_sum + all_data[[region]] * all_weights[[region]]
        weights_sum <- weights_sum + all_weights[[region]]
    }

    out <- data_sum / weights_sum
    out$date <- dates
    out$population <- country_population
    out <- out[, lapply(.SD, function(x) replace(x, is.nan(x), NA))]
    out

}

make_csv <- function(data, countrycode) {

    # Produces a csv file containing the data provided at:
    # ../../data/combined-region/{countrycode}-estimate.csv

    write.table(
        data, 
        file=paste(
            estimates_path, 
            countrycode, 
            '-estimate.csv', 
        sep=''), 
        row.names=FALSE, 
        col.names=TRUE, 
        quote=FALSE, 
        sep=','
    )

}


# main routine
main <- function(countrycode) {

    # STRUCTURE:
    # get_regions (get regional data)
    # get_data (get population data)
    # combine_data (combine data)
    # out (dump data)

    regional <- get_regions(countrycode)
    populations <- get_data(countrycode)
    combined <- combine_data(regional, populations)
    make_csv(combined, countrycode)

}


# argument parsing
go <- function() {

    # countries <- c('ES', 'FR', 'IT')
    for (country in countries) {
        main(country)  # main routine
        print(paste('Process complete for', country))
    }

    print('Exited with 0')

}


# initialization
go()
