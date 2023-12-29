
"""
Environment Module tools.

Background
    Environment modules are used at computer systems to setup the environment.
    Typical used to setup search paths and locations of libraries.
    
Usage:
    Module( command, args )

Examples:
    # Enable NetCDF module:
    Module( 'load', 'netcdf' )    
    
Bugs trapped:
    The python interface of Environment Modules v3.2.8 is buggy,
    there is at least a problem with the implementation of 'unuse' .
    For this version a fix is implemented.
    If a call to 'Module' crashes and reports strange names in the module 
    search path, then it might be this for your version too;
    in that case, extand the bug trapping in this routine.
    
See also:

    Enviroment Module software:
      http://modules.sourceforge.net/

    Code is inspired by the initialization script for python
    that comes with the Enviroment Module software:
        ${MODULESHOME}/init/python.py
    
"""

def Module( *args ) :

    """
    Call Environment Module command with given arguments.
    """
    
    # modules:
    import logging
    import os
    import subprocess
        
    # check installation of Environment Modules:
    vname = 'MODULESHOME'
    if os.getenv(vname) == None :
        logging.error( 'Environment variable "%s" not defined ...' )
        logging.error( 'Environment Module software not installed (or not correctly) ?' )
        raise EnvironmentError
    #endif
    
    # different versions:
    if os.getenv('MODULE_VERSION') in ['1.114'] :

        # full path to tcl script:
        modulecmd_tcl = os.path.join( os.getenv('MODULESHOME'), os.getenv('MODULE_VERSION'), 'bin', 'modulecmd.tcl' )

        # full commands; first argument tells to produce python code, rest is the passed arguments:
        command = [ 'tclsh', modulecmd_tcl, 'python' ]+list(args)

        # setup subprocess:
        p = subprocess.Popen( command, stdout=subprocess.PIPE, stderr=subprocess.PIPE )
        # run:
        (output,error) = p.communicate()
        # error checking through p.returncode does not work, 
        # even in case of error the return code is zero (ok) ...
        # instead, check the error output:
        if len(error) > 0 :
            logging.error( error.strip() )
            logging.error( 'From call: %s' % command )
            # show module search path:
            if os.environ.has_key('MODULEPATH') :
                logging.error( 'Current module search path:' )
                for mdir in os.getenv('MODULEPATH').split(':') :
                    logging.error( '  - %s' % mdir )
                #endfor
            #endif
            raise OSError
        #endif
        # output looks like:
        #   exec '/tmp/modulescript_64815508_0'
        # get the name of the scrpt:
        script = output.split()[1].replace("'","")
        # run:
        execfile(script)

    else :

        # trap known bug ...
        if (os.getenv('MODULEVERSION') in ['3.2.8']) and (args[0] == 'unuse') :

            # implement manually, the official version is buggy ...

            # which dir to unuse?
            udir = args[1]
            # get current search path as list:
            mdirs = os.getenv('MODULEPATH').split(':')
            # present ?
            if udir in mdirs :
                # remove requested dir:
                mdirs.remove(udir)
                # write again to ':' seperated string:
                if len(mdirs) == 0 :
                    line = ''
                else :
                    line = mdirs[0]
                    if len(mdirs) > 1 :
                        for mdir in mdirs[1:] : line = line+':'+mdir
                    #endif
                # store:
                os.putenv( mdirs )
            #endif

        else :

            # full path to module command:
            modulecmd = os.path.join( os.getenv('MODULESHOME'), 'bin', 'modulecmd' )
            # check ...
            if not os.path.isfile(modulecmd) :
                logging.error( 'Could not find module command : %s' % modulecmd )
                logging.error( 'Might be older version of Enviroment Module, change the EnvMod.py to support this.' )
                raise Exception
            #endif

            # full commands; first argument tells to produce python code, rest is the passed arguments:
            command = [ modulecmd, 'python' ]+list(args)

            # setup subprocess:
            p = subprocess.Popen( command, stdout=subprocess.PIPE, stderr=subprocess.PIPE )
            # run:
            (output,error) = p.communicate()
            # error checking through p.returncode does not work, 
            # even in case of error the return code is zero (ok) ...
            # instead, check the error output:
            if len(error) > 0 :
                logging.error( error.strip() )
                logging.error( 'From call: %s' % command )
                # show module search path:
                if os.environ.has_key('MODULEPATH') :
                    logging.error( 'Current module search path:' )
                    for mdir in os.getenv('MODULEPATH').split(':') :
                        logging.error( '  - %s' % mdir )
                    #endfor
                #endif
                raise OSError
            #endif
            # evaluate the python code returned as output:
            exec output

        #endif
        
    #endif  # switch between modules version

#enddef  # Module
