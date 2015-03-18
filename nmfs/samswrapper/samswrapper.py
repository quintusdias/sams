"""
Manage operation of SAMS fortran model.
"""

import contextlib
import io
import os
import pkg_resources as pkg
import shutil
import subprocess
import sys
import time

import numpy as np
import pandas as pd

from . import core


class SamsWrapper(object):
    """
    Wrapper for managing SAMS model and output.

    Attributes
    ----------
    config : dict
        Items include sub area mortality (dataframes) for each
        region, plus scalar values for natural, discard, and incident
        mortality for each region.
    numruns : int
        Number of model runs.
    numyears : int
        Number of years that the model runs.
    output_directory : str
        Path where output files will be written.
    model_executable_path : str
        Path to SAMS model executable.
    startyear : int
        Year that the model starts.
    f_df, s_df, cf_df : pandas.DataFrame
        tables of SAMS output
    """
    def __init__(self, numruns=1000, startyear=core._start_year,
                 access_area_management=None, outdir=os.getcwd(),
                 open_area_f=0.48, model_executable_path=None):
        """
        Parameters
        ----------
        numruns : int
            number of model runs
        startyear : int
            year that the model should start
        access_area_management : dict
            Items include sub area mortality (dataframes) for each
            region, plus scalar values for natural, discard, and incident
            mortality for each region.
        outdir : str
            Directory where the model is to be executed.  It is in this
            directory that model outputs will be found.
        open_area_f : float
            Number between 0 and 1, describes open area mortality.
        model_executable_path : str
            Path to model executable.  If not specified, we look for it in the
            package directory containing the model source.
        """
        self.numruns = numruns
        self.startyear = startyear

        self.config = access_area_management

        # Number of years is the number of rows in each region's access area
        # configuration.
        self.numyears = self.config['mid_atlantic']['sub_area_mortality'].shape[0]

        self.open_area_f = open_area_f

        self.output_directory = os.path.abspath(outdir)

        if model_executable_path is None:
            relpath = os.path.join('share', 'fortran', 'sams25')
            model_executable_path = pkg.resource_filename(__name__, relpath)
        self.model_executable_path = model_executable_path
        
        self._config_file = pkg.resource_filename(__name__,
                                                  'share/basev3.dat')
        self._boot_config = pkg.resource_filename(__name__,
                                                  'share/bootbasev3.dat')

    def parse_outputs(self):
        """
        Collect the outputs into dataframes.
        """
        output_file = os.path.join(self.output_directory,
                                   'f_samsout.dat')

        # The Reg column contains values like '   1', '   2', '   All'.  We
        # need to remove the whitespace, otherwise it makes indexing difficult
        # later on.
        strip_whitespace = lambda x: x.replace(' ', '')
        df = pd.read_csv(output_file,
                         sep='\s*,\s*',
                         converters={'Reg': strip_whitespace,
                                     'Sreg': strip_whitespace},
                         engine='python')

        # Drop the first and last columns, they are not needed.
        df.drop(df.columns[[0, -1]], axis=1, inplace=True)
        self.shell_height = df

        # Compute the mean shell heights
        self.shell_height = df.groupby(['Year', 'Reg', 'Sreg']).mean()

        output_file = os.path.join(self.output_directory,
                                   's_samsout.dat')

        # Some columns have 5 or 6 asterisks in them, which prevents these
        # columns from being read as floating point.  Convert these values to
        # NaN.
        cvrtfun = lambda x: np.nan if '*****' in x else float(x)
        converters = {'Recrt': cvrtfun,
                      'Catch': cvrtfun,
                      'CatchN': cvrtfun,
                      'Reg': strip_whitespace,
                      'Sreg': strip_whitespace}
        self.s_df = pd.read_csv(output_file,
                                sep=',\s*',
                                engine='python',
                                converters=converters,
                                na_values=[-9999])

        output_file = os.path.join(self.output_directory,
                                   'cf_samsout.dat')
        self.cf_df = pd.read_csv(output_file,
                                 sep=',\s*',
                                 engine='python')


    def run(self):
        """
        Run the model.
        """

        if not os.path.exists(self.output_directory):
            os.mkdir(self.output_directory)
        with chdir(self.output_directory):

            self.write_configuration_boot_files()

            args = [self.model_executable_path,
                    os.path.basename(self._config_file),
                    '_samsout.dat']
            print(args)
            process = subprocess.Popen(args)
            process.wait()

            self.parse_outputs()
        self.post_process()
        print("Done!")

    def post_process(self):
        """
        Primary duty is to compute Days at Sea.
        """
        df = self.s_df
        Fn = df['Fn'].groupby([df.Year, df.Reg, df.Sreg]).mean()
        
        subareas = [str(x) for x in range(1, 12)]
        nsubareas = len(subareas)
        years = list(Fn.index.get_level_values(level=0).unique())
        years = years[:-1]
        nyears = len(years)
        
        tuples = [(year, subarea) for year in years for subarea in subareas]
        idx = pd.MultiIndex.from_tuples(tuples, names=['Year', 'Sreg'])
        
        num_ma_subareas = len(core._mid_atlantic_config['sub_area_names'])
        
        maopareas = np.ones((nyears, num_ma_subareas))
        ma_config = self.config['mid_atlantic']['sub_area_mortality']
        for col in ma_config.columns:
            col_idx = int(col) - 1
            s = ma_config[col]
            maopareas[:, col_idx] = np.where(np.isnan(s), 1, 0)

        num_gb_subareas = len(core._georges_bank_config['sub_area_names'])
        gbopareas = np.ones((nyears, num_gb_subareas))
        gb_config = self.config['georges_bank']['sub_area_mortality']
        for col in gb_config.columns:
            col_idx = int(col) - 1
            s = gb_config[col]
            gbopareas[:, col_idx] = np.where(np.isnan(s), 1, 0)

        gbf = Fn.loc[:, '2', subareas]
        
        FTEq = 327
        
        ebmsmt = df['EBmsMT'].groupby([df.Year, df.Reg, df.Sreg]).mean()
        
        def gbopsum(v):
            years = list(v.index.get_level_values(level=0).unique())
            years = years[:-1]
            nyears = len(years)
            subareas = [str(x) for x in range(1, 12)]
            nsubareas = len(subareas)
        
            # extract the values from the data frame
            vmat = np.zeros((nyears, nsubareas))
            for yidx, year in enumerate(years):
                for sidx, subarea in enumerate(subareas):
                    vmat[yidx, sidx] = v.loc[year, '2', subarea]
        
            temp = np.nan * np.ones(nyears)
            for idx in range(nyears):
                x = np.dot(vmat[idx], gbopareas[idx])
                temp[idx] = 0 if np.isnan(x) else x
        
            return temp
        
        def maopsum(v):
            years = list(v.index.get_level_values(level=0).unique())
            years = years[:-1]
            nyears = len(years)
            subareas = list('12345678')
            nsubareas = 8
        
            # extract the values from the data frame
            vmat = np.zeros((nyears, nsubareas))
            for idx, year in enumerate(years):
                vmat[idx] = v.loc[year, '1', '1':'8']
        
            temp = np.nan * np.ones(nyears)
            for idx in range(nyears):
                x = np.dot(vmat[idx], maopareas[idx])
                temp[idx] = 0 if np.isnan(x) else x
        
            return temp
        
        catch = df['CatchMT'].groupby([df.Year, df.Reg, df.Sreg]).mean()
        DAS = df['DAS'].groupby([df.Year, df.Reg, df.Sreg]).mean()
        
        def shiftyears(stuff):
            return stuff[1:]
        
        GBOpLand = shiftyears(np.round(gbopsum(catch))) 
        MAOpLand = shiftyears(np.round(maopsum(catch)))
        OpLand = MAOpLand + GBOpLand
        GBOpDAS = gbopsum(DAS)
        MAOpDAS = maopsum(DAS)
        
        def tot1(v):
            # Just lops off the first year?
            return np.round(v.loc[years[1:], 'All', 'All'])
        
        # TODO:  what?
        # TLand = tot1(catch)
        # Subtracting off subarea 4 (CA2-Acc?), but why?
        x = np.round(tot1(catch).values - catch.loc[years[1]:years[-1], '2', '4'].values)
        TLand2 = x
        
        TLand = tot1(catch).values
        ABC = TLand * 48 / 34      
        ABC[0] = 20785
        ABC[1:4] = [25352, 31807, 35000]
        GCLand = 0.055*(0.99*ABC-1.25e6/2204.6) 
        
        OpDAS = shiftyears(GBOpDAS+MAOpDAS)
        
        GCOpDAS = OpDAS*GCLand/TLand2
        OpLPUE = np.round(2204.6*OpLand/OpDAS)
        OpObSA = ABC*0.01*OpLand/TLand   
        incidentaldas = 50000/OpLPUE
        OpObSADAS = OpObSA/OpLPUE*2204.6               
        OpRSADAS = OpLand/TLand*1.25e6/OpLPUE
        LAOpDAS = OpDAS - GCOpDAS - OpRSADAS - OpObSADAS - incidentaldas
        
        data = LAOpDAS / FTEq;
        
        self.ftdas = pd.Series(data, index=years[1:])

    def run_r(self):
        """
        Run R scripts on output
        """
        print('Setting up to run R...')
        with chdir(self.output_directory):

            # sfunctions261.R -- copy as-is
            basename = 'sfunctions261.R'
            print('Copying {0}...'.format(basename))
            relname = os.path.join('share', 'r', basename)
            src = pkg.resource_filename(__name__, relname)
            dest = basename
            shutil.copyfile(src, dest)

            # samsf266.R -- must rewrite the setwd command
            basename = 'samsf266.R'
            print('Rewriting {0}...'.format(basename))
            relname = os.path.join('share', 'r', basename)
            rscript = pkg.resource_filename(__name__, relname)
            with open(rscript, 'rt') as fptr:
                text = fptr.read()
                text = text.replace('SETWD_REPLACEMENT', self.output_directory)
                with open(basename, 'wt') as outfptr:
                    outfptr.write(text)

            # samssf-1.R -- must rewrite the setwd command
            basename = 'samssf-1.R'
            print('Rewriting {0}...'.format(basename))
            relname = os.path.join('share', 'r', basename)
            rscript = pkg.resource_filename(__name__, relname)
            with open(rscript, 'rt') as fptr:
                text = fptr.read()
                text = text.replace('SETWD_REPLACEMENT', self.output_directory)
                with open(basename, 'wt') as outfptr:
                    outfptr.write(text)

            # samsgraphs26.R -- copy as-is
            basename = 'samsgraphs26.R'
            print('Copying {0}...'.format(basename))
            relname = os.path.join('share', 'r', basename)
            src = pkg.resource_filename(__name__, relname)
            dest = basename
            shutil.copyfile(src, dest)

            # one script to bind them
            basename = 'run_all.R'
            print('Copying {0}...'.format(basename))
            relname = os.path.join('share', 'r', basename)
            src = pkg.resource_filename(__name__, relname)
            dest = basename
            shutil.copyfile(src, dest)

            print('Shelling out to run R...')
            args = ['R', 'CMD', 'BATCH', 'run_all.R']
            print(args)
            process = subprocess.Popen(args)
            process.wait()
            print("Done!")

    def run_pdf_viewer(self):
        """
        Run PDF viewer on output
        """
        print('Setting up to run pdf viewer...')
        with chdir(self.output_directory):
            if sys.platform.startswith('linux'):
                args = ['xdg-open', 'test.pdf']
                print(args)
                subprocess.Popen(args)

                # speedy machines
                time.sleep(2)
                args = ['xdg-open', 'test2.pdf']
                print(args)
                subprocess.Popen(args)
            else:
                os.startfile('test.pdf')
                os.startfile('test2.pdf')
            print("Done!")

    def write_configuration_boot_files(self):
        """
        """
        # Construct the access area management section of the configuration
        # file.
        s = io.StringIO()
        for region in self.config.keys():
            for year_idx, sub_areas in self.config[region]['sub_area_mortality'].iterrows():
                # Is the subregion open this year?
                if all(list(np.isnan(area) for area in sub_areas)):
                    # Yes, the subregion is open.
                    s.write('0\n')
                    continue

                count = 0
                # Must count the number of values actually specified.
                for j, x in sub_areas.iteritems():
                    if not np.isnan(x):
                        count += 1
                s.write('{0} '.format(count))

                column_ids = ''
                column_values = '' 
                for j, x in sub_areas.iteritems():
                    if not np.isnan(x):
                        column_ids += ' {}'.format(j)
                        column_values += ' {}'.format(x)
                s.write('{0} {1}\n'.format(column_ids, column_values))

        access_area_management_string = s.getvalue().strip()

        # Create the string defining the natural, discard, and incidental
        # mortality.
        s = io.StringIO()
        values = (self.config['mid_atlantic']['natural_mortality'],
                  self.config['mid_atlantic']['discard_mortality'],
                  self.config['mid_atlantic']['incidental_mortality'])
        for _ in range(11):
            # TODO: why?
            s.write('{:0.2f} {:0.2f} {:0.2f}\n'.format(*values))

        values = (self.config['georges_bank']['natural_mortality'],
                  self.config['georges_bank']['discard_mortality'],
                  self.config['georges_bank']['incidental_mortality'])
        for _ in range(11):
            # TODO: why?
            s.write('{:0.2f} {:0.2f} {:0.2f}\n'.format(*values))

        region_mortalities = s.getvalue().strip()

        # Copy the configuration and boot file  to the current directory.
        # It would appear that SAMS needs them in the current working
        # directory, rather than somewhere else.
        with open(self._config_file) as fptr:
            config = fptr.read()
            outputfile = os.path.join(self.output_directory,
                                      os.path.basename(self._config_file))
            kwargs = {'numruns': self.numruns,
                      'access_area_management': access_area_management_string,
                      'startyear': self.startyear,
                      'numyears': self.numyears,
                      'natural_discard_incidental_mortality': region_mortalities,
                      'open_area_f': self.open_area_f}
            self.model_configuration = config.format(**kwargs)
            with open(outputfile, 'wt') as ofptr:
                ofptr.write(self.model_configuration)

        # Copy the boot file into place.
        shutil.copyfile(self._boot_config,
                        os.path.join(self.output_directory,
                                     os.path.basename(self._boot_config)))


@contextlib.contextmanager
def chdir(dirname=None):
    """Cleanly change working directory.

    This context manager restores the value of the current working
    directory (cwd) after the enclosed code block completes or raises
    an exception.  If a directory name is supplied to the context manager
    then the cwd is changed prior to running the code block.

    Taken from http://www.astropython.org/snippet/2009/10/chdir-context-manager
    """
    curdir = os.getcwd()
    try:
        if dirname is not None:
            os.chdir(dirname)
        yield
    finally:
        os.chdir(curdir)
