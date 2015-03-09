"""
Defaults values for access area management.

Each column is a mortality rate in a sub area.
"""
import numpy as np
import pandas as pd


# Number of years that the model will run.
_num_years = 15

_start_year = 2014

# Number of runs to be averaged.
_num_runs = 100

_open_area_f = 0.48

# Mid Atlantic region configuration.
# sub regions 1, 3, 4, and 5 are active by default.
_sub_areas = list('1345')
_sub_area_names = ['HCAA', 'VB', 'ET-Op', 'ET-Cl', 'Dmv', 'NYB', 'LI',
                   'Inshore']
_sub_area_longnames = ['Hudson Canyon',
                       'Virginia Beach',
                       'Elephant Trunk Open',
                       'Elephant Trunk Closed',
                       'Delmarva',
                       'New York Bight',
                       'Long Island',
                       'Inshore NYB']

_data = np.zeros((_num_years, len(_sub_areas)), dtype=np.float64) * np.nan
_data[0:4] = [[0.02, 0.00, 0.00, 0.4],
              [0.35, 0.35, 0.35, 0.35],
              [0.45, 0.45, 0.45, 0.45],
              [0.55, 0.55, 0.55, 0.55]]
_df_mid_atlantic = pd.DataFrame(_data, columns=_sub_areas)
_mid_atlantic_config = {'mortality': _df_mid_atlantic,
                        'sub_area_names': _sub_area_names,
                        'name': 'Mid Atlantic',
                        'sub_area_longnames': _sub_area_longnames}

# Georges Bank region configuration.
# sub regions 2, 4, and 6 are active by default.
_sub_areas = list('246')
_data = [[0.02, 0.60, 0.80],
         [0.00, 0.00, 0.00],
         [0.00, 0.30, 0.00],
         [0.00, 0.40, 0.30],
         [0.00, 0.50, 0.40],
         [0.25, 0.25, 0.25],
         [0.25, 0.25, 0.25],
         [0.25, 0.25, 0.25],
         [0.25, 0.25, 0.25],
         [0.25, 0.25, 0.25],
         [0.25, 0.25, 0.25],
         [0.25, 0.25, 0.25],
         [0.25, 0.25, 0.25],
         [0.25, 0.25, 0.25],
         [0.25, 0.25, 0.25]]
_df_georges_bank = pd.DataFrame(np.array(_data, dtype=np.float64),
                                columns=_sub_areas)
_sub_area_names = ['CA1-NA', 'CA1-Acc', 'CA2-NA', 'CA2-Acc', 'NLS-NA',
                   'NLS-Acc', 'CA2-Ext', 'NLS-Ext', 'Sch', 'NEP', 'SEP']
_sub_area_longnames = ['CA1-NA', 'CA1-Acc', 'CA2-NA', 'CA2-Acc', 'NLS-NA',
                       'NLS-Acc', 'CA2-Ext', 'NLS-Ext', 'Sch', 'NEP', 'SEP']

# Region 2
_georges_bank_config = {'mortality': _df_georges_bank,
                        'sub_area_names': _sub_area_names,
                        'name': 'Georges Bank',
                        'sub_area_longnames': _sub_area_longnames}

_region_configuration = [_mid_atlantic_config, _georges_bank_config]
