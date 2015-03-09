import functools
import sys

import numpy as np
import pandas as pd

from matplotlib.backends import qt_compat

# This should evaluate to false.
use_pyside = qt_compat.QT_API == qt_compat.QT_API_PYSIDE

if use_pyside:
    from PySide import QtGui, QtCore
else:
    from PyQt4 import QtGui, QtCore

from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure

from . import core
from . import SamsWrapper


class QSams(QtGui.QMainWindow):
    """
    Attributes
    ----------
    numyears : int
        number of years that the model will run
    shell_height : pandas dataframe
        mean shell height multi-indexed on year, region, sub region
    """
    def __init__(self, parent=None):
        super(QSams, self).__init__(parent)

        self.numyears = core._num_years
        self.initUI()

    def _setup_menubar(self):
        """
        Just a single item for quitting.
        """

        exit_action = QtGui.QAction('&Quit', self)
        exit_action.setShortcut('Ctrl+Q')
        exit_action.triggered.connect(self.close)

        menubar = self.menuBar()
        filemenu = menubar.addMenu('File')
        filemenu.addAction(exit_action)

    def initUI(self):
        """
        setup the graphical user interface

        There is only one widget at the top-level, a set of tabs.

            tab 1 : model configuration
            tab 2 : model output, i.e. plots

        """

        self.resize(450, 750)
        self.setObjectName("Main Window")

        # The central widget really doesn't seem to be all that important.
        # But it has to be set if the top-level widget is a QMainWindow,
        # apparently.
        central_widget = QtGui.QWidget(self)
        self.setCentralWidget(central_widget)
        central_widget.setObjectName("central widget")

        # The main window has three sections
        #   1)  menu bar
        #   2)  model configuration tab
        #   3)  model output tab
        self._setup_menubar()

        tabs = QtGui.QTabWidget()

        model_configuration_frame = self._set_model_configuration_frame()
        tabs.addTab(model_configuration_frame, "Model Configuration")

        model_output_frame = self._setup_model_output_frame()
        tabs.addTab(model_output_frame, "Model Output")

        layout = QtGui.QVBoxLayout()
        layout.addWidget(tabs)
        central_widget.setLayout(layout)

    def _plot_shell_height(self, label, state):
        """
        callback for plotting shell height

        The individual descriptions of the parameters is more complicated than
        the sum total description.  If a button is pushed, then this function
        is called with the label that identifies the button (and therefore the
        subarea).  The state becomes True in that case.

        Parameters
        ----------
        label : str
            Identifies the sub area in question.
        state : bool
            If True, then a shell height plot is desired for the sub area
            specified by the label.  If false, then a shell height plot is not
            desired.
        """
        fmt = "radio button {label} clicked, state is {state}"
        print(fmt.format(label=label, state=state))

        if not state:
            # a button was turned off, but at the same time another button
            # was turned on.  Do nothing, wait for the "turn on" event to
            # trigger.
            pass

        if label in core._mid_atlantic_config['sub_area_names']:
            config = core._mid_atlantic_config
            region = '1'
            canvas = self._shell_height_canvas[config["name"]]
            subareas = config['sub_area_names']
        else:
            config = core._georges_bank_config
            region = '2'
            canvas = self._shell_height_canvas[config["name"]]
            subareas = config['sub_area_names']

        idx = subareas.index(label)
        subregion = str(idx + 1)

        canvas.update_shell_height(self.shell_height, region, subregion)

    def _plot_biomass_timeseries(self, state):
        """
        callback for plotting the biomass timeseries

        Parameters
        ----------
        state : bool
            True if the biomass timeseries is desired.
        """
        fmt = "radio button for biomass clicked, state is {state}"
        print(fmt.format(state=state))

        if not state:
            # a button was turned off, but at the same time another button
            # was turned on.  Do nothing, wait for the "turn on" event to
            # trigger.
            pass

        self._timeseries_canvas.update_biomass_timeseries(self.s_df)

    def _plot_mid_atlantic_biomass_timeseries(self, state):
        """
        callback for plotting the mid atlantic biomass timeseries

        Parameters
        ----------
        state : bool
            True if the biomass timeseries is desired.
        """
        fmt = "radio button for mid atlantic biomass clicked, state is {state}"
        print(fmt.format(state=state))

        if not state:
            # a button was turned off, but at the same time another button
            # was turned on.  Do nothing, wait for the "turn on" event to
            # trigger.
            pass

        self._timeseries_canvas.update_mid_atlantic_biomass_timeseries(self.s_df)

    def _plot_georges_bank_biomass_timeseries(self, state):
        """
        callback for plotting the georges bank biomass timeseries

        Parameters
        ----------
        state : bool
            True if the biomass timeseries is desired.
        """
        fmt = "radio button for georges bank biomass clicked, state is {state}"
        print(fmt.format(state=state))

        if not state:
            # a button was turned off, but at the same time another button
            # was turned on.  Do nothing, wait for the "turn on" event to
            # trigger.
            pass

        self._timeseries_canvas.update_georges_bank_biomass_timeseries(self.s_df)

    def _plot_biomass_quantiles_timeseries(self, state):
        """
        callback for plotting the biomass quantiles timeseries

        Parameters
        ----------
        state : bool
            True if the biomass timeseries is desired.
        """
        fmt = "radio button for biomass quantiles clicked, state is {state}"
        print(fmt.format(state=state))

        if not state:
            # a button was turned off, but at the same time another button
            # was turned on.  Do nothing, wait for the "turn on" event to
            # trigger.
            pass

        self._timeseries_canvas.update_biomass_quantiles_timeseries(self.s_df)

    def _plot_fishing_mortality_timeseries(self, state):
        """
        callback for plotting the fishing mortality timeseries

        Parameters
        ----------
        state : bool
            True if the fishing mortality timeseries is desired.
        """
        fmt = "radio button for fishing mortality clicked, state is {state}"
        print(fmt.format(state=state))

        if not state:
            # a button was turned off, but at the same time another button
            # was turned on.  Do nothing, wait for the "turn on" event to
            # trigger.
            pass

        self._timeseries_canvas.update_fishing_mortality_timeseries(self.s_df)

    def _plot_landings_timeseries(self, state):
        """
        callback for plotting the landings timeseries

        Parameters
        ----------
        state : bool
            True if the fishing mortality timeseries is desired.
        """
        fmt = "radio button for landings clicked, state is {state}"
        print(fmt.format(state=state))

        if not state:
            # a button was turned off, but at the same time another button
            # was turned on.  Do nothing, wait for the "turn on" event to
            # trigger.
            pass

        self._timeseries_canvas.update_landings_timeseries(self.s_df)

    def _plot_ftdas_timeseries(self, state):
        """
        callback for plotting the ftdas timeseries

        Parameters
        ----------
        state : bool
            True if the ftdas timeseries is desired.
        """
        fmt = "radio button for FTDAS clicked, state is {state}"
        print(fmt.format(state=state))

        if not state:
            # a button was turned off, but at the same time another button
            # was turned on.  Do nothing, wait for the "turn on" event to
            # trigger.
            pass

        self._timeseries_canvas.update_ftdas_timeseries(self.ftdas)

    def _plot_landings_quantiles_timeseries(self, state):
        """
        callback for plotting the landings quantiles timeseries

        Parameters
        ----------
        state : bool
            True if the fishing mortality timeseries is desired.
        """
        fmt = "radio button for landings quantiles clicked, state is {state}"
        print(fmt.format(state=state))

        if not state:
            # a button was turned off, but at the same time another button
            # was turned on.  Do nothing, wait for the "turn on" event to
            # trigger.
            pass

        self._timeseries_canvas.update_landings_quantiles_timeseries(self.s_df)

    def _setup_model_output_shell_height_subarea_vstrip(self, config):
        """
        Setup a vertical strip of buttons, one for each subarea.
        """
        frame = QtGui.QFrame()
        layout = QtGui.QVBoxLayout()

        for label in config['sub_area_names']:
            rbox = QtGui.QRadioButton(label, parent=frame)

            # functools.partial is a "trick" that allows us to pass more
            # parameters to the callback than would the usual setup.
            rbox.toggled.connect(functools.partial(self._plot_shell_height,
                                                   label))

            layout.addWidget(rbox)
        frame.setLayout(layout)

        return frame

    def _setup_model_output_shell_height_plots(self, config):
        """
        Embed a matplotlib window for plotting the shell height.
        """
        frame = QtGui.QFrame()
        layout = QtGui.QVBoxLayout()
        canvas = MyMplCanvas(frame)
        self._shell_height_canvas[config["name"]] = canvas
        layout.addWidget(canvas)
        return frame

    def _setup_model_output_shell_height_area_frame(self, config):
        """
        Setup frame for displaying shell height plots.  Specific for one area.

        There will be a vertical strip of buttons on the left, one for each
        subarea.  When the user hits one of the buttons, they get a plot of the
        shell height on the right.

        Parameters
        ----------
        config : dict
           set of configuration parameters for a particular area, either
           mid atlantic or georges bank
        """
        sh_frame = QtGui.QFrame()

        # Either "Mid Atlantic shell height" or "Georges Bank shell height"
        name = config["name"] + " shell height"
        sh_frame.setObjectName(name)

        sh_frame_layout = QtGui.QHBoxLayout()

        vstrip = self._setup_model_output_shell_height_subarea_vstrip(config)
        sh_frame_layout.addWidget(vstrip, 1)

        mpl_frame = self._setup_model_output_shell_height_plots(config)
        sh_frame_layout.addWidget(mpl_frame, 3)

        sh_frame.setLayout(sh_frame_layout)
        return sh_frame

    def _setup_model_output_shell_height_frame(self):
        """
        Setup frame containing all output for shell height.

        There will be a single widget, a set of tabs.  One tab is for the Mid
        Atlantic and one tab for Georges Bank.
        """

        shell_height_frame = QtGui.QFrame()
        shell_height_frame.setObjectName("Shell Height")

        layout = QtGui.QVBoxLayout()

        # Two tabs, one for Mid Atlantic, one for Georges Bank
        tabs = QtGui.QTabWidget()

        # Will hold plotting widgets for each area.
        self._shell_height_canvas = {}

        config = core._mid_atlantic_config
        frame = self._setup_model_output_shell_height_area_frame(config)
        tabs.addTab(frame, "Mid Atlantic")

        config = core._georges_bank_config
        frame = self._setup_model_output_shell_height_area_frame(config)
        tabs.addTab(frame, "Georges Bank")

        layout.addWidget(tabs)
        shell_height_frame.setLayout(layout)

        return shell_height_frame

    def _setup_model_output_shell_timeseries_vstrip(self):
        """
        """
        frame = QtGui.QFrame()
        layout = QtGui.QVBoxLayout()

        # Biomass
        rbox = QtGui.QRadioButton("Biomass by region", parent=frame)
        rbox.toggled.connect(self._plot_biomass_timeseries)
        layout.addWidget(rbox)

        # Biomass quantiles
        rbox = QtGui.QRadioButton("Biomass Quantiles", parent=frame)
        rbox.toggled.connect(self._plot_biomass_quantiles_timeseries)
        layout.addWidget(rbox)

        # Biomass Mid Atlantic
        rbox = QtGui.QRadioButton("Mid Atlantic Biomass by sub region", parent=frame)
        rbox.toggled.connect(self._plot_mid_atlantic_biomass_timeseries)
        layout.addWidget(rbox)

        # Biomass Georges Bank
        rbox = QtGui.QRadioButton("Georges Bank Biomass by sub region", parent=frame)
        rbox.toggled.connect(self._plot_georges_bank_biomass_timeseries)
        layout.addWidget(rbox)

        # Fishing mortality
        rbox = QtGui.QRadioButton("Fishing mortality", parent=frame)
        rbox.toggled.connect(self._plot_fishing_mortality_timeseries)
        layout.addWidget(rbox)

        # Landings
        rbox = QtGui.QRadioButton("Landings", parent=frame)
        rbox.toggled.connect(self._plot_landings_timeseries)
        layout.addWidget(rbox)

        # Landings quantiles
        rbox = QtGui.QRadioButton("Landings Quantiles", parent=frame)
        rbox.toggled.connect(self._plot_landings_quantiles_timeseries)
        layout.addWidget(rbox)

        # Days at Sea
        rbox = QtGui.QRadioButton("Days at Sea", parent=frame)
        rbox.toggled.connect(self._plot_ftdas_timeseries)
        layout.addWidget(rbox)

        frame.setLayout(layout)
        return frame


    def _setup_model_output_timeseries_plots(self):
        """
        Embed a matplotlib window for plotting the time series
        """
        frame = QtGui.QFrame()
        layout = QtGui.QVBoxLayout()
        canvas = MyMplCanvas(frame)
        self._timeseries_canvas = canvas
        layout.addWidget(canvas)
        return frame

    def _setup_model_output_other_frame(self):
        """
        Setup frame containing other model outputs

        There will be a single frame showing a list of timeseries plots.
        Each plot is triggered by a radio button on the left.
        """

        frame = QtGui.QFrame()
        frame.setObjectName("Other Model Output Frame")

        layout = QtGui.QHBoxLayout()

        vstrip = self._setup_model_output_shell_timeseries_vstrip()
        layout.addWidget(vstrip, 1)

        mpl_frame = self._setup_model_output_timeseries_plots()
        layout.addWidget(mpl_frame, 3)

        frame.setLayout(layout)

        return frame

    def _setup_model_output_frame(self):
        """
        Just a single widget here, a tab widget for "Shell Height" plots and
        potentially others.

        --------------------------------------------------------------------
        || Shell Height || Others |                                        |
        --------------------------                                         |
        || Mid Atlantic || Gb |                                            |
        ----------------------                                             |
        | HCAA  o |      -----------------------------------------------   |
        |  ----         |                                               |  |
        | VB    o |     |                                               |  |
        |  ----         |                                               |  |
        | ET-Op o |     |                                               |  |
        |  ----         |                                               |  |
        |               |                                               |  |
        |               |                                               |  |
        |                -----------------------------------------------   |
        --------------------------------------------------------------------
        """

        model_output_frame = QtGui.QFrame()
        layout = QtGui.QVBoxLayout()

        tabs = QtGui.QTabWidget()

        frame = self._setup_model_output_shell_height_frame()
        tabs.addTab(frame, "Shell Height")

        frame = self._setup_model_output_other_frame()
        tabs.addTab(frame, "Other Model Output")

        layout.addWidget(tabs)
        model_output_frame.setLayout(layout)
        return model_output_frame

    def _set_model_configuration_frame(self):
        """
        Setup user interface for configuring the model.

        Three elements in a vertical layout.

            1) a frame for the open area F slider
            2) a tabbed widget for specifying the region mortality
            3) a push button at the bottom that runs the model
        """

        model_configuration_frame = QtGui.QFrame()
        layout = QtGui.QVBoxLayout()

        open_area_f_frame = self.setup_open_area_f_frame()
        layout.addWidget(open_area_f_frame)

        tabs = QtGui.QTabWidget()
        tabs.setObjectName('Model Area Configurations')
        self.setup_region(tabs, **core._mid_atlantic_config)
        self.setup_region(tabs, **core._georges_bank_config)
        layout.addWidget(tabs)

        # The bottom of the main window is a strip of buttons for running the
        # model and/or quitting.
        button_strip = QtGui.QFrame()
        bottom_button_layout = QtGui.QHBoxLayout()
        run_button = QtGui.QPushButton("Run")
        run_button.clicked.connect(self.run_model)

        bottom_button_layout.addWidget(run_button)
        button_strip.setLayout(bottom_button_layout)
        layout.addWidget(button_strip)

        model_configuration_frame.setLayout(layout)
        return model_configuration_frame

    def setup_open_area_f_frame(self):
        """
        Setup slider frame for open area F control.

        This is a slider that controls the F value.  TODO:  more explanation
        There is also a text box for displaying the numerical value (a floating
        point value between 0 and 1).
        """
        frame = QtGui.QFrame()
        frame.setObjectName('Open Area F Slider Frame')
        layout = QtGui.QVBoxLayout()

        # Sliders look like they can only work with integer ranges.
        # The value of the slider will be divided by 100.
        self.open_area_f_slider = QtGui.QSlider(QtCore.Qt.Horizontal)
        self.open_area_f_slider.setRange(0, 100)
        self.open_area_f_slider.setValue(core._open_area_f * 100)
        self.open_area_f_slider.setFocusPolicy(QtCore.Qt.NoFocus)
        self.open_area_f_slider.valueChanged.connect(self.slider_changed)
        layout.addWidget(self.open_area_f_slider)

        self.open_f_edit = QtGui.QLineEdit()
        self.open_f_edit.textChanged.connect(self.open_f_changed)
        self.open_f_edit.setText(str(core._open_area_f))
        layout.addWidget(self.open_f_edit)

        frame.setLayout(layout)
        return frame

    def slider_changed(self, value):
        """
        Callback invoked when the F slider changes value.
        """
        print("slider changed to {0}".format(value))
        text = "{0:.02f}".format(float(value)/100)
        print("Changing text to {0}".format(text))
        self.open_f_edit.setText(text)

    def open_f_changed(self, value):
        """
        Callback invoked when the F text box changes value.
        """
        print("Open area f changed to {0}".format(value))
        value = float(value) * 100
        self.open_area_f_slider.setValue(value)

    def setup_region(self, tabs, name=None, mortality=None,
                     sub_area_names=None, **kwargs):
        """
        Setup user interface for mortalify configuration for this region.

        Parameters
        ----------
        tabs : tab widget
            tab widget for model configuration
        name : str
            name of the region, i.e. "Mid Atlantic" or "Georges Bank"
        mortality : pandas.DataFrame
            mortality matrix for the region
        sub_area_names : list
            list of names for the sub areas

        Returns
        -------
        frame : QtGui.QFrame
            frame with GUI individualized to the configuration specified by
            the index
        """
        frame = QtGui.QFrame()
        frame.setObjectName(name)

        layout = QtGui.QGridLayout()
        frame.setLayout(layout)
        self.setup_region_mortality_subarea_name_strip(frame, sub_area_names)
        self.setup_region_mortality_years_strip(frame)
        self.setup_region_mortality_subgrid(frame, mortality, sub_area_names)

        tabs.addTab(frame, name)

    def set_region_active_areas(self, frame, config_df):
        """
        Enable any areas specified by the configuration.

        The columns of the configuration dataframe correspond to open regions.
        TODO: more explanation
        Each of these columns should be checked in the GUI to mark them as
        open.

        Parameters
        ----------
        frame : PySide.QtGui.QFrame
            contains grid of mortality boxes
        config_df : pandas.core.frame.DataFrame
            contains configuration information for this region
        """
        for active_subregion in config_df.columns:
            name = self.get_subarea_label(int(active_subregion))
            widget = frame.findChild(QtGui.QCheckBox, name)
            widget.setChecked(True)

    def set_region_active_years(self, frame, config_df):
        """
        Enable any years specified by the configuration.

        Parameters
        ----------
        frame : PySide.QtGui.QFrame
            contains grid of mortality boxes
        config_df : pandas.core.frame.DataFrame
            contains configuration information for this region
        """
        for j, row in config_df.iterrows():

            # j is a python index, so convert this to a year number.
            year = j + 1

            # If all items in a row are NaN, then the "year" is open.
            if all([np.isnan(item) for item in row]):
                continue

            pattern = self.get_year_label(year)
            lst = frame.findChildren(QtGui.QCheckBox, pattern)
            if len(lst) != 1:
                msg = "Find John Evans and put a gun to his head.  Unable to "
                msg += "find the specified Year checkbox widget "
                msg += "(year='{0}')".format(pattern)
                raise RuntimeError(msg)

            widget = lst[0]
            widget.setChecked(True)

    def set_region_mortality(self, frame, config_df):
        """
        Set the region configuration.

        Parameters
        ----------
        frame : PySide.QtGui.QFrame
            contains grid of mortality boxes
        config_df : pandas.core.frame.DataFrame
            contains configuration information for this region
        """
        self.set_region_active_areas(frame, config_df)
        self.set_region_active_years(frame, config_df)

        # Turn on the active columns and rows as specified by the data frame
        for j, row in config_df.iterrows():

            # j is a python index, so convert this to a year number.
            year = j + 1

            for k, mortality in row.iteritems():
                # k is a pandas index (should be a single char).  Convert this
                # to an area number.
                subarea = int(k)

                # Find the specified mortality box
                name = self.get_mortality_box_name(subarea, year)
                widget = frame.findChild(QtGui.QLineEdit, name)
                if self.mortality_box_active(year, subarea, config_df):
                    self.enable_mortality_box(widget, mortality)
                else:
                    self.disable_mortality_box(widget)

    def setup_region_mortality_subarea_name_strip(self, frame, sub_area_names):
        """
        Setup the horizontal strip of checkboxes controlling which subareas are
        active.

        Parameters
        ----------
        frame : PySide.QtGui.QFrame
            widget containing the mortality matrix for the current region
        sub_area_names : list
            list of names of each sub area
        """
        layout = frame.layout()
        row = 0
        for idx in range(len(sub_area_names)):
            subarea = self.index_to_year(idx)
            label = sub_area_names[idx]

            # If the button is pressed, then column becomes active.  If a
            # certain row and column is active, then the line edit defined by
            # that row and column can accept input.
            checkbox = QtGui.QCheckBox(label, parent=frame)

            name = self.get_subarea_label(subarea)
            checkbox.setObjectName(name)

            subarea_cb = functools.partial(self.toggle_subarea_column,
                                           frame, subarea)
            checkbox.stateChanged.connect(subarea_cb)
            layout.addWidget(checkbox, row, subarea)

    def setup_region_mortality_years_strip(self, frame):
        """
        Setup the vertical strip of checkboxes controlling which years are
        active.

        Parameters
        ----------
        frame : PySide.QtGui.QFrame
            widget containing the mortality matrix for the current region
        """
        layout = frame.layout()
        for year in range(self.numyears):
            row = year + 1
            label = str(core._start_year + year)
            checkbox = QtGui.QCheckBox(label, parent=frame)
            name = self.get_year_label(year + 1)
            checkbox.setObjectName(name)

            # Normally the callback accepts a single argument for the state
            # of the checkbox.  Modify that so that we can also send the
            # parent frame and the year that identifies the checkbox that
            # was toggled.
            checkbox_cb = functools.partial(self.toggle_year_mortality_row,
                                            frame, year)
            checkbox.stateChanged.connect(checkbox_cb)
            layout.addWidget(checkbox, row, 0)

    def setup_region_mortality_subgrid(self, frame, mortality, subareas):
        """
        Setup the grid of mortality boxes.

        Parameters
        ----------
        frame : PySide.QtGui.QFrame
            widget containing the mortality matrix for the current region
        subareas : list
            list of sub areas to configure
        """
        layout = frame.layout()
        max_subareas = len(subareas)
        for year in range(self.numyears):
            row = year + 1
            for subarea in range(max_subareas):
                column = subarea + 1
                mortality_box = QtGui.QLineEdit(parent=frame)
                name = self.get_mortality_box_name(subarea + 1, year + 1)
                mortality_box.setObjectName(name)

                # By default, the mortality box is turned off.
                self.disable_mortality_box(mortality_box)

                layout.addWidget(mortality_box, row, column)

        self.set_region_mortality(frame, mortality)

    def toggle_year_mortality_row(self, frame, row, state):
        """
        The row checkbox was toggled.  This means that the row should be
        turned either on or off.

        A row is associated with a model year.

        Parameters
        ----------
        frame : PySide.QtGui.QFrame
            parent frame containing the mortality grid
        row : int
            identifies the row to be toggled
        state : int
            if zero, toggle off
        """
        year = row + 1

        # How many subareas do we have?
        rx = QtCore.QRegExp('^Subarea')
        lst = frame.findChildren(QtGui.QCheckBox, rx)
        num_sub_areas = len(lst)

        # Go through each subarea.
        for idx in range(num_sub_areas):
            subarea = idx + 1
            subarea_checkbox_name = self.get_subarea_label(subarea)
            subarea_checkbox = frame.findChild(QtGui.QCheckBox,
                                               subarea_checkbox_name)

            mbox_name = self.get_mortality_box_name(subarea, year)
            mbox = frame.findChild(QtGui.QLineEdit, mbox_name)

            if state == 0:
                # Mortality has been disassociated with this year.
                self.disable_mortality_box(mbox)
            else:
                # Mortality has been associated with this year.
                if subarea_checkbox.checkState() == QtCore.Qt.Checked:
                    # yes, the given year has associated mortality.  Enable the
                    # mortality box for this year.
                    self.enable_mortality_box(mbox, 0.0)
                else:
                    # no, the given year does not have associated mortality.
                    self.disable_mortality_box(mbox)

    def toggle_subarea_column(self, frame, column, state):
        """
        The column checkbox was toggled.  This means that the column should be
        turned either on or off.

        Parameters
        ----------
        frame : PySide.QtGui.QFrame
            parent frame containing the mortality grid
        column : int
            identifies the column to be toggled
        state : int
            if zero, toggle off
        """
        print("toggling column {0} to state {1}".format(column, state))
        subarea = column

        # Go through each year for this subarea.
        for idx in range(self.numyears):

            year = idx + 1
            year_name = self.get_year_label(year)
            year_check_box = frame.findChild(QtGui.QCheckBox, year_name)

            mbox_name = self.get_mortality_box_name(subarea, year)
            mbox = frame.findChild(QtGui.QLineEdit, mbox_name)

            if state == 0:
                # The subarea  was turned off.   Don't care about the current
                # state of the mortality box, just turn it off.
                self.disable_mortality_box(mbox)
            else:
                # The subarea  was turned on.
                if year_check_box.checkState() == QtCore.Qt.Checked:
                    # yes, the given year has associated mortality.  Enable the
                    # mortality box for this year.
                    self.enable_mortality_box(mbox, 0.0)
                else:
                    # no, the given year does not have associated mortality.
                    self.disable_mortality_box(mbox)

    def enable_mortality_box(self, mortality_box, rate):
        """
        Enable a mortality box.

        Parameters
        ----------
        mortality_box : QLineEdit
            Will hold mortality rate.
        rate : float
            A mortality rate.
        """
        mortality_box.setReadOnly(False)
        mortality_box.setStyleSheet("background-color: #ffffff")
        mortality_box.setText(str(rate))

    def disable_mortality_box(self, mortality_box):
        """
        Disable a mortality box.

        Parameters
        ----------
        mortality_box : QLineEdit
            Holds mortality rate.
        """
        mortality_box.setReadOnly(True)
        mortality_box.setText("")
        mortality_box.setStyleSheet("background-color: #bbbbbb")

    def get_mortality_box_name(self, subarea, year):
        """
        Construct the name attached to a particular mortality box.

        Parameters
        ----------
        subarea : int
            identifies a subarea
        year : int
            identifies a particular year

        Returns
        -------
        name of subarea for a specific model run year
        """
        pattern = "Subarea {0:02} Year {1:02}".format(subarea, year)
        return pattern

    def get_year_label(self, year):
        """
        Construct the name attached to a particular year checkbox

        Parameters
        ----------
        year : int
            identifies a particular year

        Returns
        -------
        label for a specific model run year
        """
        pattern = "Year {0:02}".format(year)
        return pattern

    def get_subarea_label(self, subarea):
        """
        Construct the name attached to a particular area checkbox

        Parameters
        ----------
        subarea : int
            identifies a particular subarea

        Returns
        -------
        label for a specific sub area
        """
        pattern = "Subarea {0:02}".format(subarea)
        return pattern

    def mortality_box_active(self, year, subarea, config):
        """
        Determine whether or not a mortality box should be active or not.

        Parameters
        ----------
        year : int
            identifies a particular year
        subarea : int
            identifies a particular subarea
        config : dataframe
            configuration for an region
        """
        if str(subarea) not in config.columns:
            # If the specified sub area is not among the active columns
            # as specified by the configuration dataframe, then the mortality
            # box is inactive.
            return False

        idx = year - 1
        if all(list(np.isnan(item) for item in config.ix[idx])):
            # If all items in a row (corresponding to a year for all areas)
            # are NaN, then the area is "open" (closed?), therefore not subject
            # to mortality.
            return False

        return True

    def run_model(self):
        """
        Run the model for the current model configuration.
        """
        print("Run the model!")
        access_area_configuration, f = self.collect_run_configuration()

        s = SamsWrapper(numruns=core._num_runs,
                        access_area_management=access_area_configuration,
                        open_area_f=f)
        s.run()
        self.shell_height = s.shell_height
        self.cf_df = s.cf_df
        self.s_df = s.s_df
        self.ftdas = s.ftdas
        #s.run_r()
        #s.run_pdf_viewer()

    def collect_run_configuration(self):
        """
        Collect the region configurations from the GUI.

        Parameters
        ----------
        None

        Returns
        -------
        config : list
            list of region configurations, one dataframe per region
        open_area_f : float
            open area F value
        """
        config = []

        tabs = self.findChild(QtGui.QTabWidget, "Model Area Configurations")

        # Collect the inputs and run the model.
        for i in range(2):
            tab = tabs.widget(i)
            arr = []

            # How many subareas do we have?
            rx = QtCore.QRegExp('^Subarea')
            lst = tab.findChildren(QtGui.QCheckBox, rx)
            num_sub_areas = len(lst)

            active_subareas = []
            for k in range(num_sub_areas):
                subarea = self.index_to_subarea(k)
                widget = tab.findChild(QtGui.QCheckBox,
                                       self.get_subarea_label(subarea))
                if widget.checkState() == QtCore.Qt.Checked:
                    active_subareas.append(str(subarea))

            for j in range(self.numyears):

                year = self.index_to_year(j)
                year_mortality = []

                for k in range(num_sub_areas):
                    subarea = self.index_to_subarea(k)
                    name = self.get_mortality_box_name(subarea, year)
                    widget = tab.findChild(QtGui.QLineEdit, name)
                    if widget.isReadOnly():
                        continue

                    try:
                        mortality = float(widget.text())
                    except ValueError:
                        mortality = 0.0
                    year_mortality.append(mortality)

                # if not all mortalities are NaN, then set the NaNs to zero.
                if not all(list(np.isnan(x) for x in year_mortality)):
                    ym = [0.0 if np.isnan(x) else x for x in year_mortality]
                    year_mortality = ym
                arr.append(year_mortality)

            region_config = pd.DataFrame(arr, columns=active_subareas)
            config.append(region_config)

        open_area_f = self.open_area_f_slider.value() / 100
        print(open_area_f)
        return config, open_area_f

    def index_to_year(self, idx):
        return idx + 1

    def index_to_subarea(self, idx):
        return idx + 1


class MyMplCanvas(FigureCanvas):
    """Ultimately, this is a QWidget (as well as a FigureCanvasAgg, etc.)."""
    def __init__(self, parent=None, width=5, height=4, dpi=100):

        width = parent.width() / 100 * 0.95
        height = parent.height() / 100 * 0.95

        fig = Figure(figsize=(width, height), dpi=dpi)

        self.axes = fig.add_subplot(111)

        # We want the axes cleared every time plot() is called
        self.axes.hold(False)

        # Set a default position for the time series plots.  Sometime we
        # override this, such as in Mid atlantic biomass by subregion
        # timeseries.
        self.default_timeseries_position = [0.125, 0.1, 0.775, 0.8]
        self.legend_outside_timeseries_position = [0.125, 0.1, 0.775 * 0.7, 0.8]

        self.timeseries_smaller_fontsize = 10

        self.compute_initial_figure()

        FigureCanvas.__init__(self, fig)
        self.setParent(parent)

        FigureCanvas.setSizePolicy(self,
                                   QtGui.QSizePolicy.Expanding,
                                   QtGui.QSizePolicy.Expanding)
        FigureCanvas.updateGeometry(self)

    def compute_initial_figure(self):
        pass

    def update_shell_height(self, shell_height_df, region, subregion):

        years = list(range(core._start_year,
                           core._start_year + 4))

        self.axes.cla()
        self.axes.hold(True)

        for year in years:
            series = shell_height_df.loc[year, region, subregion]
            label = str(year)
            series.plot(ax=self.axes, label=label)

        self.axes.legend()

        self.draw()

    def standardize_timeseries_ticks(self, xlabel='Years', ylabel=''):
        """
        Set a common size, orientation for xticks, yticks
        """
        # update the xlabels, make them smaller than the default
        for xlabel_handle in self.axes.get_xticklabels():
            xlabel_handle.set_fontsize(10)
        self.axes.set_xlabel(xlabel)

        # update the ylabels, make them vertical, smaller than the default
        for ylabel_handle in self.axes.get_yticklabels():
            ylabel_handle.set_rotation(90)
            ylabel_handle.set_fontsize(10)
        self.axes.set_ylabel(ylabel, rotation=90)

    def update_biomass_timeseries(self, df):

        self.axes.cla()
        self.axes.hold(True)

        bmsmt = df['BmsMT'].groupby([df.Year, df.Reg, df.Sreg]).mean()

        bmsmt.loc[:, 'All', 'All'].plot(ax=self.axes, label='All')

        subregs = [str(x) for x in range(1,12)]
        label = 'Mid Atlantic'
        bmsmt.loc[:, '1', subregs].unstack(level=0).sum().plot(ax=self.axes,
                                                               label=label)

        subregs = [str(x) for x in range(1,12)]
        label = 'Georges Bank'
        bmsmt.loc[:, '2', subregs].unstack(level=0).sum().plot(ax=self.axes,
                                                               label=label)

        self.axes.legend(loc='best')
        self.axes.set_ylim(bottom=-10000)

        self.axes.set_position(self.default_timeseries_position)

        self.standardize_timeseries_ticks(ylabel='Biomass (mt meats)')

        self.draw()

    def update_mid_atlantic_biomass_timeseries(self, df):

        self.axes.cla()
        self.axes.hold(True)

        bmsmt = df['BmsMT'].groupby([df.Year, df.Reg, df.Sreg]).mean()

        for j, label in enumerate(core._mid_atlantic_config['sub_area_longnames']):
            idx = str(j + 1)
            bmsmt.loc[:, '1', idx].plot(ax=self.axes, label=label)

        self.axes.set_ylim(bottom=-100)

        self.standardize_timeseries_ticks(ylabel='Biomass (mt meats)')

        # There are too many lines to make an effective in-plot legend, so
        # we will put it to the right the axis.  Must use a smaller axis width
        # in order to make this work.
        self.axes.set_position(self.legend_outside_timeseries_position)
        self.axes.legend(bbox_to_anchor=(1.05, 1),
                         fontsize = self.timeseries_smaller_fontsize,
                         loc=2)

        self.draw()

    def update_georges_bank_biomass_timeseries(self, df):

        # 11 sub areas, which is too many for the default color cycle.  Must
        # define our own.
        colors = ('k', 'r', 'g', 'b', 'c')
        linestyles = ('-', '--', '-.')
        styles = [(color, linestyle) for linestyle in linestyles for color in colors]

        self.axes.cla()
        self.axes.hold(True)

        bmsmt = df['BmsMT'].groupby([df.Year, df.Reg, df.Sreg]).mean()

        for j, label in enumerate(core._georges_bank_config['sub_area_longnames']):
            idx = str(j + 1)
            bmsmt.loc[:, '2', idx].plot(ax=self.axes,
                                        label=label,
                                        linestyle=styles[j][1],
                                        color=styles[j][0])

        self.axes.set_ylim(bottom=-100)

        self.standardize_timeseries_ticks(ylabel='Biomass (mt meats)')

        # There are too many lines to make an effective in-plot legend, so
        # we will put it to the right the axis.  Must use a smaller axis width
        # in order to make this work.
        self.axes.set_position(self.legend_outside_timeseries_position)
        self.axes.legend(bbox_to_anchor=(1.05, 1),
                         fontsize = self.timeseries_smaller_fontsize,
                         loc=2)

        self.draw()

    def update_landings_quantiles_timeseries(self, df):

        self.axes.cla()
        self.axes.hold(True)

        self.axes.set_position(self.default_timeseries_position)

        grp = df['CatchMT'].groupby([df.Year, df.Reg, df.Sreg])

        qmean = grp.mean().loc[:, 'All', 'All']
        q90 = grp.quantile(0.90).loc[:, 'All', 'All']
        q75 = grp.quantile(0.75).loc[:, 'All', 'All']
        q50 = grp.quantile(0.50).loc[:, 'All', 'All']
        q25 = grp.quantile(0.25).loc[:, 'All', 'All']
        q10 = grp.quantile(0.10).loc[:, 'All', 'All']

        q90.plot(ax=self.axes, color='red', linestyle='--', label='90%')
        q75.plot(ax=self.axes, color='blue', linestyle='--', label='75%')
        qmean.plot(ax=self.axes, color='black', label='Mean')
        q50.plot(ax=self.axes, color='green', linestyle='--', label='50%')
        q25.plot(ax=self.axes, color='blue', linestyle='--', label='25%')
        q10.plot(ax=self.axes, color='red', linestyle='--', label='10%')

        self.axes.legend(loc='best')
        self.axes.set_ylim(bottom=-100)

        self.standardize_timeseries_ticks(ylabel='Landings (mt meats)')

        self.draw()

    def update_biomass_quantiles_timeseries(self, df):

        self.axes.cla()
        self.axes.hold(True)

        self.axes.set_position(self.default_timeseries_position)

        grp = df['BmsMT'].groupby([df.Year, df.Reg, df.Sreg])

        qmean = grp.mean().loc[:, 'All', 'All']
        q90 = grp.quantile(0.90).loc[:, 'All', 'All']
        q75 = grp.quantile(0.75).loc[:, 'All', 'All']
        q50 = grp.quantile(0.50).loc[:, 'All', 'All']
        q25 = grp.quantile(0.25).loc[:, 'All', 'All']
        q10 = grp.quantile(0.10).loc[:, 'All', 'All']

        q90.plot(ax=self.axes, color='red', linestyle='--', label='90%')
        q75.plot(ax=self.axes, color='blue', linestyle='--', label='75%')
        qmean.plot(ax=self.axes, color='black', label='Mean')
        q50.plot(ax=self.axes, color='green', linestyle='--', label='50%')
        q25.plot(ax=self.axes, color='blue', linestyle='--', label='25%')
        q10.plot(ax=self.axes, color='red', linestyle='--', label='10%')

        self.axes.legend(loc='best')
        self.axes.set_ylim(bottom=-500)

        self.standardize_timeseries_ticks(ylabel='Biomass (mt meats)')

        self.draw()

    def update_fishing_mortality_timeseries(self, df):

        self.axes.cla()
        self.axes.hold(True)

        self.axes.set_position(self.default_timeseries_position)

        Fn = df['Fn'].groupby([df.Year, df.Reg, df.Sreg]).mean()

        Fn.loc[:, 'All', 'All'].plot(ax=self.axes, label='All')
        Fn.loc[:, '1', 'All'].plot(ax=self.axes, label='Mid Atlantic')
        Fn.loc[:, '2', 'All'].plot(ax=self.axes, label='Georges Bank')

        self.axes.legend(loc='best')
        self.axes.set_ylim(bottom=-0.05)

        self.standardize_timeseries_ticks(ylabel='Fishing mortality')

        self.draw()

    def update_landings_timeseries(self, df):

        self.axes.cla()
        self.axes.hold(True)

        self.axes.set_position(self.default_timeseries_position)

        Fn = df['CatchMT'].groupby([df.Year, df.Reg, df.Sreg]).mean()

        Fn.loc[:, 'All', 'All'].plot(ax=self.axes, label='All')
        Fn.loc[:, '1', 'All'].plot(ax=self.axes, label='Mid Atlantic')
        Fn.loc[:, '2', 'All'].plot(ax=self.axes, label='Georges Bank')

        self.axes.legend(loc='best')
        self.axes.set_ylim(bottom=-100)

        self.standardize_timeseries_ticks(ylabel='Landings (mt meats)')

        self.draw()

    def update_ftdas_timeseries(self, ftdas):

        self.axes.cla()
        self.axes.hold(True)
        self.axes.set_position(self.default_timeseries_position)
        ftdas.plot(ax=self.axes)
        self.standardize_timeseries_ticks(ylabel='Full Time Days At Sea')
        self.draw()

    def update_figure(self, **kwargs):
        pass


def run_sams_gui():
    """
    
    """

    app = QtGui.QApplication(sys.argv)
    myapp = QSams()
    myapp.show()
    sys.exit(app.exec_())
