
"""
Routine to setup the rcfile of a model run that is actually read by the
executable.
"""

def WriteRcfile( rcf ):

    """
    Reads the settings from the input rcfile, and writes the rcfile that will
    be read by the exectuble.
    The name of new rcfile is returned.
    This routine might be used to change some values and to write the
    new settings to a new file.
    """
    
    # re-write to ensure that options passed to the submiscript are included:
    rcf.WriteFile( rcf.filename )
    
    # just return filename:
    return rcf.filename

#enddef

