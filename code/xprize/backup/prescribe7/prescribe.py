# Copyright 2021 (c) IMDEA Networks Institute.

import argparse

import numpy  as np
import pandas as pd

from datetime import timedelta

import sys
import os
import subprocess
import re
import time

sys.path.append(os.path.realpath(os.path.join(os.path.dirname(__file__), "..", "logger")))
import coronasurveys_utils

IP_COLS = ['C1_School closing',
           'C2_Workplace closing',
           'C3_Cancel public events',
           'C4_Restrictions on gatherings',
           'C5_Close public transport',
           'C6_Stay at home requirements',
           'C7_Restrictions on internal movement',
           'C8_International travel controls',
           'H1_Public information campaigns',
           'H2_Testing policy',
           'H3_Contact tracing',
           'H6_Facial Coverings']

PRESCRIPTION_INDEX = 7  # Prescriptor identifier
ACTION_DURATION    = 15 # Duration of the policies in days

# Required files in local directory
regions_file    = "countries_regions.csv"
threshold_file  = "dance_threshold50.csv"
ratios_file     = "ratios.csv"
num_cases_file  = "numcases.csv"


# Prescriptor
def prescribe(start_date_str: str,
              end_date_str: str,
              path_to_prior_ips_file: str,
              path_to_cost_file: str,
              output_file_path) -> None:

    # Compute prescription time
    start_date  = pd.to_datetime(start_date_str, format='%Y-%m-%d')
    end_date    = pd.to_datetime(end_date_str,   format='%Y-%m-%d')
    num_days    = (end_date - start_date).days
    num_periods = int( np.ceil( (num_days) / ACTION_DURATION ) )

    # Maximum cost per day
    MAX_COST = 12 * 4 / 2

    # Load list of regions
    regions_df = pd.read_csv(path_to_prior_ips_file)
    regions_df['RegionName'] = regions_df['RegionName'].fillna("")
    regions_df['GeoID'] = regions_df['CountryName'] + '__' + regions_df['RegionName'].astype(str)
    geos = regions_df['GeoID'].unique()

    # Load IP costs to condition prescriptions
    cost_df = pd.read_csv(path_to_cost_file)
    cost_df['RegionName'] = cost_df['RegionName'].fillna("")
    cost_df['GeoID'] = cost_df['CountryName'] + '__' + cost_df['RegionName'].astype(str)
    geo_costs = {}
    for geo in geos:
        costs = cost_df[cost_df['GeoID'] == geo]
        cost_arr = np.array(costs[IP_COLS])[0]
        geo_costs[geo] = cost_arr

    # Load ratios data
    ratios_df = pd.read_csv(ratios_file, dtype={'CountryName'                           : str,
                                                'RegionName'                           : str,
                                                'Date'                                 : object,
                                                'C1_School closing'                    : int,
                                                'C2_Workplace closing'                 : int,
                                                'C3_Cancel public events'              : int,
                                                'C4_Restrictions on gatherings'        : int,
                                                'C5_Close public transport'            : int,
                                                'C6_Stay at home requirements'         : int,
                                                'C7_Restrictions on internal movement' : int,
                                                'C8_International travel controls'     : int,
                                                'H1_Public information campaigns'      : int,
                                                'H2_Testing policy'                    : int,
                                                'H3_Contact tracing'                   : int,
                                                'H6_Facial Coverings'                  : int,
                                                'avg_ratio'                            : float,
                                                'sd_ratio'                             : float,
                                                'ratio15days'                          : float} )
    ratios_df['RegionName'] = ratios_df['RegionName'].fillna("")
    ratios_df['GeoID'] = ratios_df['CountryName'] + '__' + ratios_df['RegionName'].astype(str)
    ratios_df = ratios_df[~ratios_df.drop(columns=["avg_ratio", "sd_ratio", "ratio15days"]).duplicated()]

    # Compute simulated policies for all regions
    geo_policies = {}
    for geo in geos:
        # Rank list of simulated policies
        policies = np.array(ratios_df[ratios_df['GeoID'] == geo][IP_COLS])
        costs    = geo_costs[geo]
        order    = np.dot(policies, np.transpose(costs)).flatten()
        order    = np.argsort(order)
        policies = policies[order]
        ratios   = np.array(ratios_df[ratios_df['GeoID'] == geo]["avg_ratio"])
        ratios   = ratios[order]
        geo_policies[geo] = {"policies" : policies,
                             "ratios"   : ratios}

    # Compute MAX_CASES for all regions
    threshold_df  = pd.read_csv(threshold_file)
    geo_max_cases = {}
    for geo in geos:
        country_name = geo.split("__")[0]
        region_name  = geo.split("__")[1]
        # Compute the maximum number of cases for this region
        if region_name == "":
            population = threshold_df[(threshold_df["CountryName"] == country_name)]["population"].values[0]
        else:
            population = threshold_df[(threshold_df["CountryName"] == country_name) & (threshold_df["RegionName"] == region_name)]["population"].values[0]           
        # MAX_CASES = (population / 10000) * 10
        MAX_CASES = (population / 10000) * 50
        geo_max_cases[geo] = MAX_CASES

    # Load current cases
    cases_df = pd.read_csv(num_cases_file, parse_dates=['Date'])
    cases_df['RegionName'] = cases_df['RegionName'].fillna("")
    cases_df['GeoID'] = cases_df['CountryName'] + '__' + cases_df['RegionName'].astype(str)
    geo_cases = {}
    for geo in geos:
        cases = cases_df[(cases_df['GeoID'] == geo) & (cases_df['Date'] == start_date_str)]['PredictedDailyNewCases'].values[0]
        geo_cases[geo] = cases

    # Final data frame
    final_prescriptions = pd.DataFrame(columns=['CountryName', 'RegionName', 'Date', 'C1_School closing', 'C2_Workplace closing',
                                                'C3_Cancel public events', 'C4_Restrictions on gatherings', 'C5_Close public transport',
                                                'C6_Stay at home requirements', 'C7_Restrictions on internal movement',
                                                'C8_International travel controls', 'H1_Public information campaigns',
                                                'H2_Testing policy', 'H3_Contact tracing', 'H6_Facial Coverings', 'PrescriptionIndex'])

    # For every geographical region
    for geo in geos:

        country_name = geo.split("__")[0]
        region_name  = geo.split("__")[1]

         # Ranked list of used policies and their rate
        policies = geo_policies[geo]["policies"]
        ratios   = geo_policies[geo]["ratios"]

        # Cases for this region
        num_cases = geo_cases[geo]
        MAX_CASES = geo_max_cases[geo]

        best_fitness  = np.inf
        best_policies = [0] * num_periods

        # For every period
        for period_id in np.arange(num_periods):


            # For every policy
            for policy_id in np.arange(len(policies)):

                total_cost  = 0
                total_cases = 0

                # Compute candidate policy
                candidate_policies            = best_policies.copy()
                candidate_policies[period_id] = policy_id

                # Compute total costs of measures
                costs = geo_costs[geo]
                for pid in np.arange(num_periods):
                    policy = policies[candidate_policies[pid]]
                    tmp    = np.dot(costs, policy)
                    tmp    = np.sum(tmp)
                    total_cost = total_cost + tmp * ACTION_DURATION
                    
                # Compute total number of cases
                local_cases = num_cases
                for pid in np.arange(num_periods):
                    rate = ratios[candidate_policies[pid]]
                    for j in np.arange(ACTION_DURATION):
                        local_cases = local_cases * rate
                        total_cases = total_cases + local_cases

                rel_cost  = total_cost  / num_days / MAX_COST
                rel_cases = total_cases / num_days / MAX_CASES 

                # Arithmetic mean
                fitness = (rel_cost + rel_cases) / 2
                
                if fitness < best_fitness:
                    best_fitness  = fitness 
                    best_policies = candidate_policies.copy()
                    
        # Save optimal results
        
        current_date = start_date

        for i in np.arange(num_periods):
            
            policy = policies[best_policies[i]]
            
            # For each day
            for j in np.arange(ACTION_DURATION):

                tmp_df = pd.DataFrame({
                    'CountryName'                          : country_name,
                    'RegionName'                           : region_name,
                    'Date'                                 : current_date.strftime("%Y-%m-%d"),
                    'C1_School closing'                    : policy[0],
                    'C2_Workplace closing'                 : policy[1],
                    'C3_Cancel public events'              : policy[2],
                    'C4_Restrictions on gatherings'        : policy[3],
                    'C5_Close public transport'            : policy[4],
                    'C6_Stay at home requirements'         : policy[5],
                    'C7_Restrictions on internal movement' : policy[6],
                    'C8_International travel controls'     : policy[7],
                    'H1_Public information campaigns'      : policy[8],
                    'H2_Testing policy'                    : policy[9],
                    'H3_Contact tracing'                   : policy[10],
                    'H6_Facial Coverings'                  : policy[11],
                    'PrescriptionIndex'                    : PRESCRIPTION_INDEX
                }, index=[0])
                
                final_prescriptions = pd.concat([final_prescriptions, tmp_df], ignore_index=True)
                
                current_date = current_date + timedelta(days=1)

    # Save final predictions
    final_prescriptions.to_csv(output_file_path, header=True, index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--start_date",
                        dest="start_date",
                        type=str,
                        required=True,
                        help="Start date from which to prescribe, included, as YYYY-MM-DD."
                             "For example 2020-08-01")
    parser.add_argument("-e", "--end_date",
                        dest="end_date",
                        type=str,
                        required=True,
                        help="End date for the last prescription, included, as YYYY-MM-DD."
                             "For example 2020-08-31")
    parser.add_argument("-ip", "--interventions_past",
                        dest="prior_ips_file",
                        type=str,
                        required=True,
                        help="The path to a .csv file of previous intervention plans")
    parser.add_argument("-c", "--intervention_costs",
                        dest="cost_file",
                        type=str,
                        required=True,
                        help="Path to a .csv file containing the cost of each IP for each geo")
    parser.add_argument("-o", "--output_file",
                        dest="output_file",
                        type=str,
                        required=True,
                        help="The path to an intervention plan .csv file")
    args = parser.parse_args()

    start = time.time()

    log_name = "default"
    matches = re.findall(r'/(prescribe\d+)', os.path.dirname(os.path.realpath(__file__)))
    if len(matches) > 0:
        log_name = matches[0]

    logger = coronasurveys_utils.named_log(str(log_name), log_name)

    logger.info(f"Generating prescriptions from {args.start_date} to {args.end_date}...")


    try:
        prescribe(args.start_date, args.end_date, args.prior_ips_file, args.cost_file, args.output_file)

    except OSError as error:
        logger.info(error)
    except:
        logger.info("Unexpected error: %s", sys.exc_info()[0])
        raise
    else:
        logger.info("Successfully executed %s", os.path.realpath(__file__))

    print("Done!")
    logger.info("Duration: %s seconds", coronasurveys_utils.secondsToStr(time.time() - start))
