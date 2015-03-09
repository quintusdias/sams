"""
Command line interface for running the SAMS model.
"""
import argparse
import os

from . import SamsWrapper


def run_sams():
    """
    Command line interface for running the SAMS model.
    """
    description = "Run SAMS model."
    parser = argparse.ArgumentParser(description=description)

    help = 'Number of runs, defaults to 1000.'
    parser.add_argument('--num-runs', '-n',
                        metavar='NUMRUNS',
                        default=1000,
                        dest='numruns',
                        help=help)

    help = 'Starting year, defaults to 2014.'
    parser.add_argument('--starting-year',
                        metavar='STARTYEAR',
                        default=2014,
                        dest='startyear',
                        help=help)

    help = 'Number of years the model runs, defaults to 16.'
    parser.add_argument('--num-years',
                        metavar='NUMYEARS',
                        default=16,
                        dest='numyears',
                        help=help)

    help = 'Output directory, defaults to current working directory.'
    parser.add_argument('--output-dir', '-o',
                        metavar='OUTDIR',
                        default=os.getcwd(),
                        dest='outdir',
                        help=help)

    args = parser.parse_args()

    kwargs = {'outdir': args.outdir,
              'numruns': args.numruns,
              'startyear': args.startyear}
    samsw = SamsWrapper(**kwargs)
    samsw.run()
