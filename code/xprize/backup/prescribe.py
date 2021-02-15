import os
import sys
import argparse
import pandas as pd
import glob
import subprocess
import logging
import time
import json
import re

sys.path.append(os.path.expanduser("logger"))
import coronasurveys_utils

##########################################################################################

subprocess_timeout = 19800 # 5.5 hours in seconds

##########################################################################################

#bin_path = os.path.join('usr', 'bin')
#opt_conda_path = os.path.join('opt', 'conda', 'bin')
#os.environ["PATH"] += os.pathsep + opt_conda_path + os.pathsep + bin_path

# workdir = "~/work"
workdir="."

def zeroOutput(start_date_str: str,
               end_date_str: str,
               path_to_hist_file: str,
               path_to_cost_file: str,
               output_file_path,
               prescription_index: int) -> None:

    NPI_COLS = ['C1_School closing',
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

    # Create skeleton df with one row for each geo for each day
    hdf = pd.read_csv(path_to_hist_file,
                      parse_dates=['Date'],
                      encoding="ISO-8859-1",
                      dtype={"RegionName": str},
                      error_bad_lines=True)
    start_date = pd.to_datetime(start_date_str, format='%Y-%m-%d')
    end_date = pd.to_datetime(end_date_str, format='%Y-%m-%d')
    country_names = []
    region_names = []
    dates = []

    for country_name in hdf['CountryName'].unique():
        cdf = hdf[hdf['CountryName'] == country_name]
        for region_name in cdf['RegionName'].unique():
            for date in pd.date_range(start_date, end_date):
                country_names.append(country_name)
                region_names.append(region_name)
                dates.append(date.strftime("%Y-%m-%d"))

    prescription_df = pd.DataFrame({
        'CountryName': country_names,
        'RegionName': region_names,
        'Date': dates})

    # Fill df with all zeros
    for npi_col in NPI_COLS:
        prescription_df[npi_col] = 0

    # Add prescription index column.
    prescription_df['PrescriptionIndex'] = prescription_index

    # Create the output path
    os.makedirs(os.path.dirname(output_file_path), exist_ok=True)

    # Save to a csv file
    prescription_df.to_csv(output_file_path, index=False)

    return


if __name__ == '__main__':

    logger = coronasurveys_utils.named_log("main")

    logger.info('')
    logger.info('')
    logger.info('#######  EXECUTING CORONASURVEYS MULTI-PRESCRIPTOR RUNNER  #####################################')
    logger.info('')

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

    workdir = os.path.expanduser(workdir)

    os.chdir(workdir)


    try:
        output_prescriptions_dir = os.path.join(workdir, 'prescriptions')
        os.makedirs(output_prescriptions_dir, exist_ok=True)
    except OSError:
        logger.info(f'Creation of the directory {output_prescriptions_dir} failed')
    else:
        logger.info(f'Successfully created the directory {output_prescriptions_dir}')

    prescriptions = sorted(glob.glob('prescribe[0-9]/prescribe.py', recursive=False))
    output_files = {}

    logger.info('')
    logger.info('***************************************************************************************')
    logger.info(f'********* CORONASURVEYS PRESCRIPTIONS *************************************************')
    logger.info(json.dumps(prescriptions, indent=32))
    logger.info('***************************************************************************************')
    logger.info('')

    procs = {}

    for p in prescriptions:

        precription_index = -1
        matches = re.findall(r'prescribe(\d+)/prescribe.py', p)
        if len(matches) > 0:
            precription_index = int(matches[0])
        else:
            continue

        #if precription_index == 0:
        #    precription_index = 10

        logger.info(f'Launching prescriptor [{p}] from {args.start_date} to {args.end_date}')

        prescription_script = os.path.basename(p)
        prescription_dir = os.path.dirname(p)

        filename, file_extension = os.path.splitext(args.output_file)
        prescription_output_file = os.path.realpath(
            os.path.expanduser(os.path.join(prescription_dir, prescription_dir + "_output" + file_extension)))

        if os.path.exists(prescription_output_file):
            os.remove(prescription_output_file)


        zeroOutput(args.start_date,
                   args.end_date,
                   os.path.expanduser(args.prior_ips_file),
                   os.path.expanduser(args.cost_file),
                   prescription_output_file,
                   precription_index)

        try:
            execute_cmd = [
                'python', prescription_script,
                '--start_date', args.start_date,
                '--end_date', args.end_date,
                '--interventions_past', os.path.realpath(os.path.expanduser(args.prior_ips_file)),
                '--intervention_costs', os.path.realpath(os.path.expanduser(args.cost_file)),
                '--output_file', os.path.realpath(os.path.expanduser(prescription_output_file))
            ]

            logger.info("Command: " + ' '.join(execute_cmd))

            #logger.info(json.dumps(execute_cmd, indent=32))

            procs[p] = subprocess.Popen(
                execute_cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                cwd=os.path.realpath(os.path.expanduser(prescription_dir))
            )

        except subprocess.CalledProcessError as err:
            logger.info(f'Error occurred: {err.stderr}')
        except:
            logger.info("Main script - Unexpected error:", sys.exc_info()[0])
            raise
        else:
            logger.info("Main script - Successfully launched subprocesses.")

        # let this be here, in case we need to parallelize prescriptors
        output_files[p] = prescription_output_file

    ###################################################################################
    logger.info("")
    logger.info("####### Launched CORONASURVEYS MULTI-PRESCRIPTOR RUNNER")
    logger.info("")

    procs_to_terminate = list(procs.keys())
    start = time.time()

    # do this while there are processes alive
    while procs_to_terminate:

        time.sleep(1)

        # if elapsed time exceeds the timeout threshold
        if (time.time() - start) > subprocess_timeout:

            # ok go ahead and forcibly terminate all processes
            for pkey, p in list(procs.items()):
                if p.poll() is None:
                    p.terminate()
                    #del output_files[pkey] # since we abruptly terminated, ignore any outputs
                    logger.info(f"      Terminated {pkey} - {subprocess_timeout} secs timeout exceeded.")
                else:
                    logger.info(f"Already finished {pkey} - {subprocess_timeout} secs timeout exceeded.")

            break


        for pkey in procs_to_terminate:

            # check if process has terminated (i.e. poll() != None)
            if procs[pkey].poll() is not None:

                procs_to_terminate.remove(str(pkey))

                (stdout, stderr) = procs[pkey].communicate()
                logger.info("")
                logger.info("---------------------------------------------------------------------------------------")
                logger.info(f'Prescriptor: {pkey} - exitcode: {procs[pkey].returncode}')
                #logger.info("")
                #logger.info("=== stdout ============================================================================")
                #logger.info(stdout.decode())
                #logger.info("")
                #logger.info("=== stderr ============================================================================")
                #logger.info(stderr.decode())
                #logger.info("")
                logger.info("---------------------------------------------------------------------------------------")
                #logger.info("")
                #logger.info("")

                #if procs[pkey].returncode:
                #    del output_files[pkey]

    if output_files:
        # filter for outputs that cannot be read
        outputs_to_combine = list(output_files.keys())
        for pkey in outputs_to_combine:
            if not os.path.isfile(output_files[pkey]):
                del output_files[pkey]

        # concatenate csv files
        combined_csv = pd.concat([pd.read_csv(f) for f in output_files.values()])

        # combined_csv.to_csv( args.output_file, index=False, encoding='utf-8-sig')
        combined_csv.to_csv(args.output_file, encoding='utf-8-sig')

    # prescribe(args.start_date, args.end_date, args.prior_ips_file, args.cost_file, args.output_file)
    logger.info('#######   COMPLETED CORONASURVEYS MULTI-PRESCRIPTOR RUNNER  #####################################')
