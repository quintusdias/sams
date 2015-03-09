from django.core.urlresolvers import resolve
from django.test import TestCase
from django.http import HttpRequest

from samswrapper import SamsWrapper
from samsweb.views import home_page

class SamsTests(TestCase):

    def generate_default_post_params(self):
        data = {'ma_row_0_col_0': '0.02',
                'ma_row_1_col_0': '0.35',
                'ma_row_2_col_0': '0.45',
                'ma_row_3_col_0': '0.55',
                'ma_row_0_col_2': '0.00',
                'ma_row_1_col_2': '0.35',
                'ma_row_2_col_2': '0.45',
                'ma_row_3_col_2': '0.55',
                'ma_row_0_col_3': '0.00',
                'ma_row_1_col_3': '0.35',
                'ma_row_2_col_3': '0.45',
                'ma_row_3_col_3': '0.55',
                'ma_row_0_col_4': '0.40',
                'ma_row_1_col_4': '0.35',
                'ma_row_2_col_4': '0.45',
                'ma_row_3_col_4': '0.55'}

        data['ma_sub_area_active_0'] = 'on'
        data['ma_sub_area_active_2'] = 'on'
        data['ma_sub_area_active_3'] = 'on'
        data['ma_sub_area_active_4'] = 'on'

        sub_region_names = ['Hudson Canyon',
                            'Virginia Beach',
                            'Elephant Trunk Open',
                            'Elephant Trunk Closed',
                            'DelMarVa',
                            'New York Bight',
                            'Long Island',
                            'Inshore NYB']
        post_format = ','.join([sname for sname in sub_region_names])
        data['ma_sub_region_names'] = [post_format]

        sub_region_names = ['CA1-NA', 'CA1-Acc', 'CA2-NA',  'CA2-Acc',
                            'NLS-NA', 'NLS-Acc', 'CA2-Ext', 'NLS-Ext',
                            'Sch',    'NEP',     'SEP']
        post_format = ','.join([sname for sname in sub_region_names])
        data['gb_sub_region_names'] = [post_format]

        # CA1_Acc
        data['gb_sub_area_active_1'] = 'on'
        mortality = [0.02, 0.00, 0.00, 0.00, 0.00, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_1'.format(row)
            data[key] = value

        # CA2_Acc
        data['gb_sub_area_active_3'] = 'on'
        mortality = [0.60, 0.00, 0.30, 0.40, 0.50, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_3'.format(row)
            data[key] = value

        # NLS-Acc
        data['gb_sub_area_active_5'] = 'on'
        mortality = [0.80, 0.00, 0.00, 0.30, 0.40, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_5'.format(row)
            data[key] = value

        data['start_year'] = 2014
        data['num_years'] = 15
        data['open_area_f_mortality'] = 0.48
        return(data)


    def generate_elephand_trunk_closed_extended(self):
        data = {'ma_row_0_col_0': '0.02',
                'ma_row_1_col_0': '0.35',
                'ma_row_2_col_0': '0.45',
                'ma_row_3_col_0': '0.55',
                'ma_row_0_col_2': '0.00',
                'ma_row_1_col_2': '0.35',
                'ma_row_2_col_2': '0.45',
                'ma_row_3_col_2': '0.55',
                'ma_row_0_col_3': '0.00',
                'ma_row_1_col_3': '0.05',
                'ma_row_2_col_3': '0.00',
                'ma_row_3_col_3': '0.35',
                'ma_row_4_col_3': '0.45',
                'ma_row_5_col_3': '0.55',
                'ma_row_0_col_4': '0.40',
                'ma_row_1_col_4': '0.35',
                'ma_row_2_col_4': '0.45',
                'ma_row_3_col_4': '0.55'}

        data['ma_sub_area_active_0'] = 'on'
        data['ma_sub_area_active_2'] = 'on'
        data['ma_sub_area_active_3'] = 'on'
        data['ma_sub_area_active_4'] = 'on'

        sub_region_names = ['Hudson Canyon',
                            'Virginia Beach',
                            'Elephant Trunk Open',
                            'Elephant Trunk Closed',
                            'DelMarVa',
                            'New York Bight',
                            'Long Island',
                            'Inshore NYB']
        post_format = ','.join([sname for sname in sub_region_names])
        data['ma_sub_region_names'] = [post_format]

        sub_region_names = ['CA1-NA', 'CA1-Acc', 'CA2-NA',  'CA2-Acc',
                            'NLS-NA', 'NLS-Acc', 'CA2-Ext', 'NLS-Ext',
                            'Sch',    'NEP',     'SEP']
        post_format = ','.join([sname for sname in sub_region_names])
        data['gb_sub_region_names'] = [post_format]

        # CA1_Acc
        data['gb_sub_area_active_1'] = 'on'
        mortality = [0.02, 0.00, 0.00, 0.00, 0.00, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_1'.format(row)
            data[key] = value

        # CA2_Acc
        data['gb_sub_area_active_3'] = 'on'
        mortality = [0.60, 0.00, 0.30, 0.40, 0.50, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_3'.format(row)
            data[key] = value

        # NLS-Acc
        data['gb_sub_area_active_5'] = 'on'
        mortality = [0.80, 0.00, 0.00, 0.30, 0.40, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_5'.format(row)
            data[key] = value

        data['start_year'] = 2014
        data['num_years'] = 15
        data['open_area_f_mortality'] = 0.48
        return(data)


    def generate_gb6_post_params(self):
        """
        First six sub areas in Georges Bank are active.
        """
        data = {'ma_row_0_col_0': '0.02',
                'ma_row_1_col_0': '0.35',
                'ma_row_2_col_0': '0.45',
                'ma_row_3_col_0': '0.55',
                'ma_row_0_col_2': '0.00',
                'ma_row_1_col_2': '0.35',
                'ma_row_2_col_2': '0.45',
                'ma_row_3_col_2': '0.55',
                'ma_row_0_col_3': '0.00',
                'ma_row_1_col_3': '0.35',
                'ma_row_2_col_3': '0.45',
                'ma_row_3_col_3': '0.55',
                'ma_row_0_col_4': '0.40',
                'ma_row_1_col_4': '0.35',
                'ma_row_2_col_4': '0.45',
                'ma_row_3_col_4': '0.55'}

        data['ma_sub_area_active_0'] = 'on'
        data['ma_sub_area_active_2'] = 'on'
        data['ma_sub_area_active_3'] = 'on'
        data['ma_sub_area_active_4'] = 'on'

        sub_region_names = ['Hudson Canyon',
                            'Virginia Beach',
                            'Elephant Trunk Open',
                            'Elephant Trunk Closed',
                            'DelMarVa',
                            'New York Bight',
                            'Long Island',
                            'Inshore NYB']
        post_format = ','.join([sname for sname in sub_region_names])
        data['ma_sub_region_names'] = [post_format]

        sub_region_names = ['CA1-NA', 'CA1-Acc', 'CA2-NA',  'CA2-Acc',
                            'NLS-NA', 'NLS-Acc', 'CA2-Ext', 'NLS-Ext',
                            'Sch',    'NEP',     'SEP']
        post_format = ','.join([sname for sname in sub_region_names])
        data['gb_sub_region_names'] = [post_format]

        # CA1_NA
        data['gb_sub_area_active_0'] = 'on'
        mortality = [0.02, 0.00, 0.00, 0.00, 0.00, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_0'.format(row)
            data[key] = value

        # CA1_Acc
        data['gb_sub_area_active_1'] = 'on'
        mortality = [0.02, 0.00, 0.00, 0.00, 0.00, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_1'.format(row)
            data[key] = value

        # CA2_NA
        data['gb_sub_area_active_2'] = 'on'
        mortality = [0.60, 0.00, 0.30, 0.40, 0.50, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_2'.format(row)
            data[key] = value

        # CA2_Acc
        data['gb_sub_area_active_3'] = 'on'
        mortality = [0.60, 0.00, 0.30, 0.40, 0.50, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_3'.format(row)
            data[key] = value

        # NLS-NA
        data['gb_sub_area_active_4'] = 'on'
        mortality = [0.80, 0.00, 0.00, 0.30, 0.40, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_4'.format(row)
            data[key] = value

        # NLS-Acc
        data['gb_sub_area_active_5'] = 'on'
        mortality = [0.80, 0.00, 0.00, 0.30, 0.40, 0.25, 0.25, 0.25, 0.25,
                     0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
        for row, value in enumerate(mortality):
            key = 'gb_row_{0}_col_5'.format(row)
            data[key] = value

        data['start_year'] = 2014
        data['num_years'] = 15
        data['open_area_f_mortality'] = 0.48
        return(data)


    def test_root_url_resolves_to_home_page_view(self):
        found = resolve('/sams/')
        self.assertEqual(found.func, home_page)

    def test_home_page_returns_correct_html(self):
        response = self.client.get('/sams/')
        self.assertContains(response, 'Sams Model for scallops')

    def test_home_page_uses_correct_template(self):
        response = self.client.get('/sams/')
        self.assertTemplateUsed(response, 'home.html')

    def test_model_was_run(self):
        data = self.generate_default_post_params()
        response = self.client.post('/sams/run', data)
        self.assertContains(response, 'Model Results')

    def test_et_closed_extended_model(self):
        """
        First six georges bank areas are open and have mortalities specified.
        """
        data = self.generate_elephand_trunk_closed_extended()
        response = self.client.post('/sams/run', data)
        self.assertContains(response, 'Model Results')

    def test_gb6_model(self):
        """
        First six georges bank areas are open and have mortalities specified.
        """
        data = self.generate_gb6_post_params()
        response = self.client.post('/sams/run', data)
        self.assertContains(response, 'Model Results')

