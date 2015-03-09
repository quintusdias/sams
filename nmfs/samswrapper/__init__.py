import os

from . import core
from .samswrapper import SamsWrapper
from . import command_line

if 'DJANGO_SETTINGS_MODULE' in os.environ:                                         
    import matplotlib                                                              
    matplotlib.use('Agg')                                                          
    __all__ = [core, SamsWrapper, command_line]                                    
else:                                                                              
    from . import gui                                                              
    __all__ = [core, SamsWrapper, command_line, gui] 
