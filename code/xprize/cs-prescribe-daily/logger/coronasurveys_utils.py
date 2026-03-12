import os
import logging

from time import time, strftime, localtime
from datetime import datetime, timedelta


def log_popen_pipe(p, stdfile, logger):

    while p.poll() is None:
        line = stdfile.readline().rstrip()
        if line:
            logger.info(line)

    rest = stdfile.read().rstrip()
    if rest:
        logger.info(rest)


def named_log(loggerName, logfile="coronasurveys"):

    for handler in logging.root.handlers[:]:
        logging.root.removeHandler(handler)

    logdir = os.path.realpath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", "logs"))

    try:
        os.makedirs(logdir, exist_ok=True)
    except OSError:
        print(f'Creation of log directory {logdir} failed')
    #else:
    #    print(f'CoronaSurveys log directory {logdir}')

    datefile = '{}-{:%Y-%m-%d}.log'.format(logfile, datetime.now())

    logging.basicConfig(
        filename=os.path.join(logdir, datefile),
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        level=logging.DEBUG,
        datefmt='%Y-%m-%d %H:%M:%S')

    return logging.getLogger(loggerName)


def secondsToStr(elapsed=None):

    if elapsed is None:
        return strftime("%Y-%m-%d %H:%M:%S", localtime())
    else:
        return str(timedelta(seconds=elapsed))

