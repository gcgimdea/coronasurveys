"""
Reads the "prescription tasks" file locally (or from a locally visible remote mount). For each date pair (start date
and end date) requested, runs the designated prescription module, using the supplied interventions plan,
to generate the prescriptions.
"""

# Copyright 2021 (c) Cognizant Digital Business, Evolutionary AI. All rights reserved. Issued under the Apache 2.0 License.

import argparse
import logging
import os
import subprocess
from os.path import isfile, expanduser

import pandas as pd
from pandas import DataFrame
from concurrent.futures import ThreadPoolExecutor
import time

logging.basicConfig(
    format='%(asctime)s %(name)-20s %(levelname)-8s %(message)s',
    level=logging.INFO,
    datefmt='%Y-%m-%d %H:%M:%S'
)

LOGGER = logging.getLogger('robojudge')


    
       

def concatenate_prescriptions(change_date, end_date, ips_file, presc_file, base_out_ip_file, concatenate_script: str) -> None:
    # now concatenate prescription file
    procs = [None] * 10
    for pn in range(10):
        out_ip_file_pn =  os.path.splitext(base_out_ip_file)[0]+"-"+str(pn)+".csv"
        if isfile(expanduser(out_ip_file_pn)):
            LOGGER.warning(f'Concatenation already generated at {out_ip_file_pn}. Skipping concatenation.')
            continue
        # command start_date change_date end_date path_to_ips_file path_to_prescriptions prescription_number output_file_path
        r_cmd = [
            "Rscript",
            "--vanilla",
            concatenate_script,
            "2020-01-01",
            change_date, 
            end_date,
            ips_file,
            presc_file,
            str(pn),
            out_ip_file_pn
        ]
        procs[pn] =subprocess.Popen(r_cmd)
#        print (r_cmd)



        
    for pn in range(10):
        if procs[pn] is not None: 
            procs[pn].wait()


def generate_predictions(start_date, end_date, ipbase, outbase, predictor_module: str) -> None:
  # now generate predictions
    procs = [None] * 10
    for pn in range(10):
        ip_file = os.path.splitext(ipbase)[0]+"-"+str(pn)+".csv"
        outpred = os.path.splitext(outbase)[0]+"-"+str(pn)+".csv"
        if isfile(expanduser(outpred)):
            LOGGER.warning(f'Predictions already generated at {outpred}. Skipping prediction.')
            continue
        # command start_date change_date end_date path_to_ips_file path_to_prescriptions prescription_number output_file_path
        p_cmd = [
            'python', predictor_module,
            '--start_date', start_date,
            '--end_date', end_date,
            '--interventions_plan', ip_file,
            '--output_file', outpred
        ]
        LOGGER.info(f"launching prediction {p_cmd}")
        procs[pn] = subprocess.Popen(p_cmd)
        
    for pn in range(10):
        if procs[pn] is not None:
            procs[pn].wait()



        ########################################33

def get_prescriptions_tasks(requested_prescriptions_file):
    """
    Reads the file containing the list of prescriptions to be generated.
    :param requested_prescriptions_file: Path to the CSV file containing the prescriptions to be generated
    :return: A Pandas DataFrame containing the prescriptions to be generated, one row per requested prescription
    """
    # Don't want to parse dates here as we'll be sending them as strings to the spawned process command line
    return pd.read_csv(
        requested_prescriptions_file,
        encoding="ISO-8859-1"
    )



def generate_prescriptions_and_predictions(requested_prescriptions_df: DataFrame, prescription_module: str, concatenate_script: str, 
                                           validation_module: str, predictor_module: str) -> None:
    """
    Generates prescriptions for each of the requested scenarios by invoking `prescription_module`
    :param requested_prescriptions_df: A Pandas DataFrame containing the prescriptions to be made, one per row.
    See sample in `examples/sample_prescriptions_task.csv` for format
    :param prescription_module: Path to the module to be invoked to generate prescriptions. Generally should be
    <path>/prescribe.py
    :param validation_module: Module to be used after generating the prescriptions to validate them
    :return Nothing. Prescriptions are written to the designated output file supplied in
    requested_prescriptions_df
    """
    for row in requested_prescriptions_df.itertuples():
        start_date = row.StartDate
        end_date = row.EndDate
        ip_file = row.IpFile
        cost_file = row.CostFile
        output_ip_file = row.OutputFile

        LOGGER.info(f'Running prescribe concatenate and predict module {prescription_module}')
        LOGGER.info(f'Start date: {start_date}')
        LOGGER.info(f'End date: {end_date}')
        LOGGER.info(f'IP file: {ip_file}')
        LOGGER.info(f'Cost file: {cost_file}')
        LOGGER.info(f'Output file: {output_ip_file}')

        # Skip if file exists already -- don't want to needlessly generate the same prescriptions again
        if isfile(expanduser(output_ip_file)):
            LOGGER.warning(f'Prescriptions already generated at {output_ip_file}. Skipping.')
        else: 

            # Spawn an external process to generate prescriptions
            subprocess.call(
                [
                    'python', prescription_module,
                    '--start_date', start_date,
                    '--end_date', end_date,
                    '--interventions_past', ip_file,
                    '--intervention_costs', cost_file,
                    '--output_file', output_ip_file
                ]
            )

            LOGGER.info(f'Running validation module {validation_module}')
            LOGGER.info(f'Start date: {start_date}')
            LOGGER.info(f'End date: {end_date}')
            LOGGER.info(f'IP file: {ip_file}')
            LOGGER.info(f'Output file: {output_ip_file}')

            # Now run validation
            subprocess.call(
                [
                    'python', validation_module,
                    '--start_date', start_date,
                    '--end_date', end_date,
                    '--interventions_plan', ip_file,
                    '--submission_file', output_ip_file
                ]
            )




        change_date=start_date
        complete_ip_file=os.path.splitext(output_ip_file)[0]+"-concat.csv"
        predOutBase=os.path.splitext(output_ip_file)[0]+"-pred"
        
        concatenate_prescriptions(change_date, end_date, ip_file, output_ip_file, complete_ip_file, concatenate_script)
        generate_predictions(change_date, end_date, complete_ip_file, predOutBase, predictor_module)



def do_main():
    """
    Main line for this module
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--requested-prescriptions-file",
                        dest="requested_prescriptions_file",
                        type=str,
                        required=True,
                        help="Path to the filename containing dates for prescriptions to be generated, IP costs and "
                             "requested output files.")
    parser.add_argument("-p", "--prescription-module",
                        dest="prescription_module",
                        type=str,
                        required=True,
                        help="Path to the python script that should be run to generate prescriptions. According to the "
                             "API conversion this script should be named prescribe.py")
    parser.add_argument("-d", "--prediction-module",
                        dest="prediction_module",
                        type=str,
                        required=True,
                        help="Path to the python script that should be run to generate predictions. According to the "
                             "API conversion this script should be named predict.py")
    parser.add_argument("-v", "--validation-module",
                        dest="validation_module",
                        type=str,
                        required=True,
                        help="Path to the python script that should be run to validate prescriptions. Any errors found "
                             "in the prescriptions will be written to stdout")
    parser.add_argument("-c", "--concatenate-script",
                        dest="concatenate_script",
                        type=str,
                        required=True,
                        help="Path to the R script that concatenates IPS files")
    args = parser.parse_args()

    LOGGER.info(f'Generating prescriptions from file {args.requested_prescriptions_file}')
    requested_prescriptions_df = get_prescriptions_tasks(args.requested_prescriptions_file)
    generate_prescriptions_and_predictions(requested_prescriptions_df, args.prescription_module, args.concatenate_script, args.validation_module, args.prediction_module)


if __name__ == '__main__':
    do_main()
