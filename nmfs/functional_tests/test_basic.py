from .base import BaseTest

class NewVisitorTest(BaseTest):

    def test_can_run_the_model(self):
        # Sam has heard about a cool new online model named "Sams".  He goes to
        # check out its homepage.
        self.browser.get(self.starting_url)

        # He notices the page title and header mention the sams model and scallops.
        self.assertIn('Sams', self.browser.title)
        self.assertIn('scallops', self.browser.title)

        header_text = self.browser.find_element_by_tag_name('h1').text
        self.assertIn('Sams', header_text)

        # There are checkboxes for the sub areas.  Sam clicks on Virginia
        # Beach (turning it on), then turns it off again.
        self.browser.find_element_by_css_selector('#virginia_beach_checkbox_id').click()
        self.browser.find_element_by_css_selector('#virginia_beach_checkbox_id').click()

        # There are tabs for model configuration of the georges bank
        # region and the mid atlantic.  Sam clicks on the Georges Bank tab
        self.browser.find_element_by_css_selector('#gb_link_id').click()

        # There should be a label for open area f
        self.browser.find_element_by_css_selector('h3#open_area_f_label_id')

        # The f-mortality display has a value.
        id = '#open_area_f_mortality_display_id'
        slider_display = self.browser.find_element_by_css_selector(id)
        slider_text = slider_display.get_attribute("value")
        self.assertEqual(slider_text, '0.48')

        # Sam gets bored and just wants to see if the model will run.
        self.browser.find_element_by_css_selector('#submit_id').click()

        # A results page is displayed.
        # There is a set of tabs for Mid Atlantic shell height.  Sam clicks
        # thru all of them.
        ids = ['#ma_hudson_canyon_shell_height_link_id',
               '#ma_virginia_beach_shell_height_link_id',
               '#ma_elephant_trunk_open_shell_height_link_id',
               '#ma_elephant_trunk_closed_shell_height_link_id',
               '#ma_delmarva_shell_height_link_id',
               '#ma_new_york_bight_shell_height_link_id',
               '#ma_long_island_shell_height_link_id',
               '#ma_inshore_nyb_shell_height_link_id',
               '#gb_ca1_na_shell_height_link_id',
               '#gb_ca1_acc_shell_height_link_id',
               '#gb_ca2_na_shell_height_link_id',
               '#gb_ca2_acc_shell_height_link_id',
               '#gb_nls_na_shell_height_link_id',
               '#gb_nls_acc_shell_height_link_id',
               '#gb_ca2_ext_shell_height_link_id',
               '#gb_nls_ext_shell_height_link_id',
               '#gb_sch_shell_height_link_id',
               '#gb_nep_shell_height_link_id',
               '#gb_sep_shell_height_link_id',
               '#biomass_by_region_link_id',
               '#biomass_by_quantiles_link_id',
               '#mid_atlantic_biomass_link_id',
               '#georges_bank_biomass_link_id',
               '#fishing_mortality_link_id',
               '#landings_link_id',
               '#landings_quantiles_link_id',
               '#days_at_sea_link_id',
               ]
        for id in ids:
            self.browser.find_element_by_css_selector(id).click()

        # Go back to the model setup page.
        self.browser.find_element_by_css_selector('#new_model_run_link_id').click()
        self.assertEqual(self.browser.current_url, self.starting_url)
