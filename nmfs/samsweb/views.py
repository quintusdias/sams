import io
import re
import tempfile

import matplotlib.pyplot as plt
import seaborn

from django.http import HttpResponse
from django.shortcuts import render
import numpy as np
import pandas as pd

from samswrapper import SamsWrapper

image_cache = {'shell_height': {},
               'biomass': {},
               'landings': {},
               'fishing_mortality': {},
               'days_at_sea': {},
               'area_swept': {}}
text_cache = {}

default_timeseries_position = [0.125, 0.1, 0.775, 0.8]
legend_outside_timeseries_position = [0.125, 0.1, 0.775 * 0.7, 0.8]
timeseries_smaller_fontsize = 10


def home_page(request):
    """
    View for introducing the model configuration page, "/sams/"
    """
    return render(request, 'home.html')


def get_model_configuration(request):
    """
    Returns SAMS model configuration.  This is a debug view.
    """
    text = text_cache['model_configuration']
    return HttpResponse(text, content_type='text/plain')


def get_image(request, image_class, image_name):
    """
    View for a particular image produced by the post processing.

    Parameters
    ----------
    image_class : str
        Identifies the kind of image, i.e. 'biomass' or 'shell_height'.
    image_name : str
        Identifies an individual image within the image class, e.g. the Hudson
        Canyon image in the shell height class.
    """
    image = image_cache[image_class][image_name]
    return HttpResponse(image, content_type='image/png')


def run_model(request):
    """
    View for running the SAMS model.

    Parameters
    ----------
    request : HttpRequest object
        The POST field contains all the necessary information for configuring
        a particular model run.
    """

    params = unpack_post_parameters(request.POST)

    config = {}
    df = pd.DataFrame(params['mid_atlantic_mortality'],
                      columns=params['mid_atlantic_active_areas'])
    config['mid_atlantic_sub_area_mortality'] = df

    df = pd.DataFrame(params['georges_bank_mortality'],
                      columns=params['georges_bank_active_areas'])
    config['georges_bank_sub_area_mortality'] = df

    # Add the natural, discard, and incident mortality for each region.
    for region in ['mid_atlantic', 'georges_bank']:
        for label in ['natural', 'discard', 'incidental']:
            key = region + '_' + label + '_mortality'
            config[key] = params[key]

    # Invoke the SAMS model wrapper, which in turn runs the model.
    with tempfile.TemporaryDirectory() as tdir:
        s = SamsWrapper(outdir=tdir, numruns=100,
                        startyear=params['start_year'],
                        access_area_management=config,
                        open_area_f=params['open_area_f_mortality'])
        s.run()

    create_web_outputs(params, s)

    return render(request, 'results.html')


def create_web_outputs(params, samsw):
    """
    Parameters
    ----------
    params : dictionary
        somewhat reprocessed POST parameters that the user configured via
        the web page
    samsw : samswrapper object
        object that encapsulates the SAMS model
    """

    # clear the image cache of any existing images.
    for key, cache_class in image_cache.items():
        cache_class.clear()
    text_cache.clear()

    plot_shell_height(params, samsw.shell_height)
    plot_biomass_by_region(samsw.s_df)
    plot_biomass_quantiles(samsw.s_df)
    plot_mid_atlantic_biomass_by_sub_areas(params['ma_sub_region_names'],
                                           samsw.s_df)
    plot_georges_bank_biomass_by_sub_areas(params['gb_sub_region_names'],
                                           samsw.s_df)
    plot_fishing_mortality(samsw.s_df)
    plot_landings(samsw.s_df)
    plot_landings_quantiles(samsw.s_df)
    plot_days_at_sea(samsw.ftdas)
    plot_area_swept(samsw.s_df)

    text_cache['model_configuration'] = samsw.model_configuration


def plot_days_at_sea(days_at_sea_series):
    """
    Produce time series plot of Days at Sea.

    The image is not written out to file, but rather kept in memory in order
    to facilitate easier web serving.

    Parameters
    ----------
    days_at_sea_series : pandas.Series
        Time series of Days at Sea.
    """

    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.set_position(default_timeseries_position)

    days_at_sea_series.plot(ax=ax)
    ax.set_ylabel('Days at Sea')

    content = io.BytesIO()
    plt.savefig(content, format='png')
    content.seek(0)
    image_cache['days_at_sea']['days_at_sea'] = content

    plt.close()


def plot_landings_quantiles(df):
    """
    Plot landings quantiles.

    Plot the time series of landings quantiles for the combined areas.  As well
    as the 90, 75, 50, 25, and 10% quantiles, also plot the mean.

    The image is not written out to file, but rather kept in memory in order
    to facilitate easier web serving.

    Parameters
    ----------
    df : pandas.DataFrame
        One of the columns is CatchMT (landings)
    """
    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.set_position(default_timeseries_position)

    grp = df['CatchMT'].groupby([df.Year, df.Reg, df.Sreg])

    qmean = grp.mean().loc[:, 'All', 'All']
    q90 = grp.quantile(0.90).loc[:, 'All', 'All']
    q75 = grp.quantile(0.75).loc[:, 'All', 'All']
    q50 = grp.quantile(0.50).loc[:, 'All', 'All']
    q25 = grp.quantile(0.25).loc[:, 'All', 'All']
    q10 = grp.quantile(0.10).loc[:, 'All', 'All']

    # Don't plot the first year.  Also, the data is shifted by one year.
    # For some reason, restricting the year range above results in a series
    # that still have a multi-index.  This seems like the cleanest way to do
    # that.
    qmean = qmean.iloc[2:]
    q90 = q90.iloc[2:]
    q75 = q75.iloc[2:]
    q50 = q50.iloc[2:]
    q25 = q25.iloc[2:]
    q10 = q10.iloc[2:]
    qmean.index = qmean.index - 1
    q90.index = q90.index - 1
    q75.index = q75.index - 1
    q50.index = q50.index - 1
    q25.index = q25.index - 1
    q10.index = q10.index - 1

    colors = seaborn.color_palette(n_colors=3)

    q90.plot(ax=ax, color=colors[0], linestyle='--', label='90%')
    q75.plot(ax=ax, color=colors[1], linestyle='--', label='75%')
    qmean.plot(ax=ax, color='black', label='Mean')
    q50.plot(ax=ax, color=colors[2], linestyle='--', label='50%')
    q25.plot(ax=ax, color=colors[1], linestyle='--', label='25%')
    q10.plot(ax=ax, color=colors[0], linestyle='--', label='10%')

    ax.legend(loc='best')

    content = io.BytesIO()
    plt.savefig(content, format='png')
    content.seek(0)
    image_cache['landings']['quantiles'] = content

    plt.close()


def plot_area_swept(df):
    """
    Plot time series of area swept.

    Plot the time series of Mid Atlantic, Georges Bank, and combined area
    swept.

    The image is not written out to file, but rather kept in memory in order
    to facilitate easier web serving.

    Parameters
    ----------
    df : pandas.DataFrame
        One of the columns is BotArea
    """
    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.set_position(default_timeseries_position)

    Fn = df['BotArea'].groupby([df.Year, df.Reg, df.Sreg]).mean()

    all_area_swept = Fn.loc[:, 'All', 'All']
    ma_area_swept = Fn.loc[:, '1', 'All']
    gb_area_swept = Fn.loc[:, '2', 'All']

    # Don't plot the first year.  Also, the data is shifted by one year.
    # For some reason, restricting the year range above results in a series
    # that still have a multi-index.  This seems like the cleanest way to do
    # that.
    all_area_swept = all_area_swept.iloc[2:]
    ma_area_swept = ma_area_swept.iloc[2:]
    gb_area_swept = gb_area_swept.iloc[2:]
    all_area_swept.index = all_area_swept.index - 1
    ma_area_swept.index = ma_area_swept.index - 1
    gb_area_swept.index = gb_area_swept.index - 1

    all_area_swept.plot(ax=ax, label='All')
    ma_area_swept.plot(ax=ax, label='Mid Atlantic')
    gb_area_swept.plot(ax=ax, label='Georges Bank')

    ax.legend(loc='best')

    content = io.BytesIO()
    plt.savefig(content, format='png')
    content.seek(0)
    image_cache['area_swept']['area_swept'] = content

    plt.close()


def plot_landings(df):
    """
    Plot time series of landings.

    Plot the time series of Mid Atlantic, Georges Bank, and combined landings.

    The image is not written out to file, but rather kept in memory in order
    to facilitate easier web serving.

    Parameters
    ----------
    df : pandas.DataFrame
        One of the columns is CatchMT (landings)
    """
    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.set_position(default_timeseries_position)

    Fn = df['CatchMT'].groupby([df.Year, df.Reg, df.Sreg]).mean()

    all_data = Fn.loc[:, 'All', 'All']
    ma_data = Fn.loc[:, '1', 'All']
    gb_data = Fn.loc[:, '2', 'All']

    # Don't plot the first year.  Also, the data is shifted by one year.
    # For some reason, restricting the year range above results in a series
    # that still have a multi-index.  This seems like the cleanest way to do
    # that.
    all_data = all_data.iloc[2:]
    ma_data = ma_data.iloc[2:]
    gb_data = gb_data.iloc[2:]
    all_data.index = all_data.index - 1
    ma_data.index = ma_data.index - 1
    gb_data.index = gb_data.index - 1

    all_data.plot(ax=ax, label='All')
    ma_data.plot(ax=ax, label='Mid Atlantic')
    gb_data.plot(ax=ax, label='Georges Bank')

    ax.legend(loc='best')

    content = io.BytesIO()
    plt.savefig(content, format='png')
    content.seek(0)
    image_cache['landings']['landings'] = content

    plt.close()


def plot_fishing_mortality(df):
    """
    Plot time series of fishing mortality.

    Plot the time series of Mid Atlantic, Georges Bank, and combined fishing
    mortality.

    The image is not written out to file, but rather kept in memory in order
    to facilitate easier web serving.

    Parameters
    ----------
    df : pandas.DataFrame
        One of the columns is Fn (mortality)
    """
    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.set_position(default_timeseries_position)

    Fn = df['Fn'].groupby([df.Year, df.Reg, df.Sreg]).mean()

    all_fishing_mortality = Fn.loc[:, 'All', 'All']
    ma_fishing_mortality = Fn.loc[:, '1', 'All']
    gb_fishing_mortality = Fn.loc[:, '2', 'All']

    # Don't plot the first year.  Also, the data is shifted by one year.
    # For some reason, restricting the year range above results in a series
    # that still have a multi-index.  This seems like the cleanest way to do
    # that.
    all_fishing_mortality = all_fishing_mortality[2:]
    ma_fishing_mortality = ma_fishing_mortality[2:]
    gb_fishing_mortality = gb_fishing_mortality[2:]

    all_fishing_mortality.index = all_fishing_mortality.index - 1
    ma_fishing_mortality.index = ma_fishing_mortality.index - 1
    gb_fishing_mortality.index = gb_fishing_mortality.index - 1

    all_fishing_mortality.plot(ax=ax, label='All')
    ma_fishing_mortality.plot(ax=ax, label='Mid Atlantic')
    gb_fishing_mortality.plot(ax=ax, label='Georges Bank')

    ax.legend(loc='best')

    content = io.BytesIO()
    plt.savefig(content, format='png')
    content.seek(0)
    image_cache['fishing_mortality']['fishing_mortality'] = content

    plt.close()


def plot_region_biomass_by_sub_areas(df, region_name, region_idx, styles,
                                     sub_area_names):
    """
    Plot biomass time series for each sub area.

    Parameters
    ----------
    df : pandas.DataFrame
        biomass data is in BmsMT column
    region_name : str
        Name of region.  Only used to construct image name
    region_idx : str
        Either '1' for mid atlantic or '2' for georges bank
    styles : list
        tuples of colors and linestyles.
    sub_area_names : list
        List of sub areas, used to label the line plots.

    The image is not written out to file, but rather kept in memory in order
    to facilitate easier web serving.

    """
    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.set_position(default_timeseries_position)

    bmsmt = df['BmsMT'].groupby([df.Year, df.Reg, df.Sreg]).mean()

    for j, label in enumerate(sub_area_names):
        sub_area_idx = str(j + 1)
        bmsmt.loc[:, region_idx, sub_area_idx].plot(ax=ax,
                                                    label=label,
                                                    color=styles[j][0],
                                                    linestyle=styles[j][1])

    ax.set_ylim(bottom=-100)

    ax.set_position(default_timeseries_position)

    # There are too many lines to make an effective in-plot legend, so
    # we will put it to the right the axis.  Must use a smaller axis width
    # in order to make this work.
    ax.set_position(legend_outside_timeseries_position)
    ax.legend(bbox_to_anchor=(1.05, 1),
              fontsize = timeseries_smaller_fontsize,
              loc=2)

    key = "{}_by_sub_area".format(region_name)
    content = io.BytesIO()
    plt.savefig(content, format='png')
    content.seek(0)
    image_cache['biomass'][key] = content

    plt.close()


def plot_georges_bank_biomass_by_sub_areas(gb_sub_area_names, df):
    """
    Plot Georges Bank biomass by sub area.

    Parameters
    ----------
    gb_sub_area_names : list
       List of strings naming each sub area for Georges Bank.
    df : pandas DataFrame
        Dataframe with biomass
    """

    # 11 regions, so split up six colors amongst two color styles
    colors = seaborn.color_palette(n_colors=6)
    linestyles = ('-', '--')
    styles = [(color, style) for style in linestyles for color in colors]

    plot_region_biomass_by_sub_areas(df, 'georges_bank', '2', styles,
                                     gb_sub_area_names)


def plot_mid_atlantic_biomass_by_sub_areas(ma_sub_area_names, df):
    """
    Plot Mid Atlantic biomass by sub area.

    Parameters
    ----------
    ma_sub_area_names : list
       List of strings naming each sub area for the Mid Atlantic.
    df : pandas DataFrame
        Dataframe with biomass
    """

    # Eight regions, so split up four colors amongst two color styles
    colors = seaborn.color_palette(n_colors=4)
    linestyles = ('-', '--')
    styles = [(color, style) for style in linestyles for color in colors]

    plot_region_biomass_by_sub_areas(df, 'mid_atlantic', '1', styles,
                                     ma_sub_area_names)


def plot_biomass_quantiles(df):
    """
    Plot time series of biomass quantiles.

    Plot the time series of biomass quantiles for the combined regions.

    The image is not written out to file, but rather kept in memory in order
    to facilitate easier web serving.

    Parameters
    ----------
    df : pandas.DataFrame
        One of the columns is BmsMT (biomass)
    """
    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.set_position(default_timeseries_position)

    grp = df['BmsMT'].groupby([df.Year, df.Reg, df.Sreg])

    qmean = grp.mean().loc[:, 'All', 'All']
    q90 = grp.quantile(0.90).loc[:, 'All', 'All']
    q75 = grp.quantile(0.75).loc[:, 'All', 'All']
    q50 = grp.quantile(0.50).loc[:, 'All', 'All']
    q25 = grp.quantile(0.25).loc[:, 'All', 'All']
    q10 = grp.quantile(0.10).loc[:, 'All', 'All']

    colors = seaborn.color_palette(n_colors=3)

    q90.plot(ax=ax, color=colors[0], linestyle='--', label='90%')
    q75.plot(ax=ax, color=colors[1], linestyle='--', label='75%')
    qmean.plot(ax=ax, color='black', label='Mean')
    q50.plot(ax=ax, color=colors[2], linestyle='--', label='50%')
    q25.plot(ax=ax, color=colors[1], linestyle='--', label='25%')
    q10.plot(ax=ax, color=colors[0], linestyle='--', label='10%')

    ax.legend(loc='best')
    ax.set_ylim(bottom=-500)

    ax.set_ylabel('Biomass (mt meats)')

    content = io.BytesIO()
    plt.savefig(content, format='png')
    content.seek(0)
    image_cache['biomass']['quantiles'] = content

    plt.close()


def plot_biomass_by_region(df):
    """
    Plot time series of biomass by region and combined.

    The image is not written out to file, but rather kept in memory in order
    to facilitate easier web serving.

    Parameters
    ----------
    df : pandas.DataFrame
        One of the columns is BmsMT (biomass)
    """
    fig = plt.figure()
    ax = fig.add_subplot(111)

    bmsmt = df['BmsMT'].groupby([df.Year, df.Reg, df.Sreg]).mean()

    bmsmt.loc[:, 'All', 'All'].plot(ax=ax, label='All')

    subregs = [str(x) for x in range(1, 12)]
    label = 'Mid Atlantic'
    bmsmt.loc[:, '1', subregs].unstack(level=0).sum().plot(ax=ax, label=label)

    subregs = [str(x) for x in range(1, 12)]
    label = 'Georges Bank'
    bmsmt.loc[:, '2', subregs].unstack(level=0).sum().plot(ax=ax, label=label)

    ax.legend(loc='best')
    ax.set_ylim(bottom=-10000)
    ax.set_position(default_timeseries_position)
    ax.set_ylabel('Biomass (mt meats)')

    content = io.BytesIO()
    plt.savefig(content, format='png')
    content.seek(0)
    image_cache['biomass']['by_region'] = content

    plt.close()


def plot_shell_height(params, df):
    """
    Plot time series of shell height for each sub area.

    The image is not written out to file, but rather kept in memory in order
    to facilitate easier web serving.

    Parameters
    ----------
    df : pandas.DataFrame
        Time series of shell height, indexed by year, region, and sub area.
    """
    fig = plt.figure()
    ax = fig.add_subplot(111)
    years = list(range(params['start_year'], params['start_year'] + 4))

    regions = [('1', 'ma'), ('2', 'gb')]
    for region_idx, region_abbrev in regions:

        subareas = params[region_abbrev + '_sub_region_names']
        for j, subarea_proper_name in enumerate(subareas):
            subarea_idx = str(j + 1)

            # Turn the subarea proper name (like 'Hudson Canyon') into
            # a viable python identifier (like 'hudson_canyon').
            key = subarea_proper_name.lower().replace(' ', '_')

            if (years[0], region_idx, subarea_idx) not in df.index:
                continue

            for year in years:
                series = df.loc[year, region_idx, subarea_idx]
                series.plot(ax=ax, label=str(year))

            ax.set_xlabel('Shell Height (mm)')
            ax.set_ylabel('#/tow')
            ax.legend()

            content = io.BytesIO()
            plt.savefig(content, format='png')
            content.seek(0)
            image_cache['shell_height'][key] = content

            plt.cla()

    plt.close()


def unpack_post_parameters(rpost):
    """
    Unpack post parameters, mold into input for Sams

    Parameters
    ----------
    rpost : django.http.request.QueryDict
        Dictionary of POST parameters

    Returns
    -------
    data : dict
        Set of inputs ready to feed to SamsWrapper to run the model.
    """
    data = {}
    data['gb_sub_region_names'] = rpost['gb_sub_region_names'].split(',')
    data['ma_sub_region_names'] = rpost['ma_sub_region_names'].split(',')
    data['start_year'] = int(rpost['start_year'])
    data['num_years'] = int(rpost['num_years'])

    data['open_area_f_mortality'] = float(rpost['open_area_f_mortality'])

    # Retrieve the mortality matrix, restrict it to the sub areas that the
    # user marked as active.
    #
    # The active area indices that come in are zero-based, while the model
    # expectes one-based numbers, so be sure to convert them.
    active_areas = unpack_active_areas('gb', rpost)
    mortality = unpack_mortality('gb', rpost)
    data['georges_bank_mortality'] = mortality.T[active_areas].T
    data['georges_bank_active_areas'] = [str(x+1) for x in active_areas]

    active_areas = unpack_active_areas('ma', rpost)
    mortality = unpack_mortality('ma', rpost)
    data['mid_atlantic_mortality'] = mortality.T[active_areas].T
    data['mid_atlantic_active_areas'] = [str(x+1) for x in active_areas]

    # Retrieve the natural, discard, and incident mortality for each region.
    regions = ['mid_atlantic', 'georges_bank']
    region_abbrevs = ['ma', 'gb']
    for region, region_abbrev in zip(regions, region_abbrevs):
        for label in ['natural', 'discard', 'incidental']:
            key1 = region + '_' + label + '_mortality'
            key2 = region_abbrev + '_' + label + '_mortality'
            data[key1] = float(rpost[key2])

    return data


def unpack_active_areas(region, post_params):
    """
    Retrieve the sub areas that the user marked as active.

    If the user checks a sub area checkbox, it is "active".   We have to use
    this rather than the mortality matrix because the numbers in the matrix
    are not a reliable indicator of this.

    Parameters
    ----------
    region : str
        Either 'ma' or 'gb'
    post_params : django.http.request.QueryDict
        Dictionary of POST parameters

    Returns
    -------
    active_lst : list
        List of indices of the active regions.
    """
    pattern = '(?P<region>' + region + ')_sub_area_active_(?P<col>\d+)'
    regex = re.compile(pattern)
    lst = []
    for key, value in post_params.items():

        m = regex.match(key)
        if not m:
            continue

        lst.append(int(m.group('col')))

    lst.sort()
    return lst


def unpack_mortality(region, post_params):
    """
    Parameters
    ----------
    region : str
        Either 'ma' for Mid Atlantic or 'gb' for Georges Bank.
    post_params : dictionary
        Parameters received via the POST operation.
    """
    num_years = int(post_params['num_years'])

    sub_area_param = region + '_sub_region_names'
    sub_areas = post_params[sub_area_param].split(',')
    num_sub_areas = len(sub_areas)

    mortality = np.nan * np.ones((num_years, num_sub_areas))

    # Look for POST items that look like 'ma_row_9_col_12'.  We need to parse
    # it for the row and column number, as that tells us which item to set in
    # the corresponding mortality matrix.
    pattern = '(?P<region>' + region + ')_row_(?P<row>\d+)_col_(?P<col>\d+)'
    regex = re.compile(pattern)
    for key, value in post_params.items():

        m = regex.match(key)
        if not m:
            continue
        if len(value) == 0:
            # Most grid values will be empty.  Don't try to parse them into
            # floating point values.
            continue

        # We found a POST item to process.
        row_idx = int(m.group('row'))
        col_idx = int(m.group('col'))

        mortality[row_idx, col_idx] = float(post_params[key])

    return mortality
