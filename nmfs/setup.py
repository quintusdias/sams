"""
Manage installation of samswrapper
"""

from setuptools import setup, find_packages

setup(
    name='samswrapper',
    version='0.2.0rc1',
    packages=['samswrapper'],
    description='Python wrapper for running SAMS model',
    author='John Evans',
    author_email='john.g.evans@noaa.gov',
    classifiers=['Development Status :: 3 - Alpha',
                 'Intended Audience :: Developers',
                 'Topic :: Scientific/Engineering :: Bio-Informatics',
                 'Programming Language :: Python :: 3',
                 'Programming Language :: Python :: 3.4'],
    install_requires=['pandas >= 0.13.1'],
    package_data={'sams': ['share/basev3.dat', 'share/bootbasev3.dat',
                           'share/r/*.R', 'share/fortran/*.f90',
                           'share/fortran/makefile']},
    entry_points={'console_scripts': ['run_sams=samswrapper.command_line:run_sams'],
                  'gui_scripts': ['gsams=sams.gui:run_sams_gui']},
)
