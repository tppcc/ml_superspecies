#! /usr/bin/env python

"""
MakeDep  -  create Makefile dependencies for Fortran sources

USAGE

EXTERNAL PROGRAMS

  makedepf90
    http://personal.inet.fi/private/erikedelmann/makedepf90/

  
"""

class Dependencies( object ) :

    """
    Base class for Fortran source file dependencies.
    """
    
    def __init__( self, flags='', files='*.[Ff]*', expl={} ) :

        """
        Return list with dependency lines for Fortran source.
        This routine is actually an interface to the 'makedepf90' tool.

        Optional arguments:
          flags
                    String with flags passed to 'makedepf90' tool,
                    for example macro definition:
                      -D with_some_library -D mytest ...
                    See 'makedepf90 --help' for inspiration.
          files
                    String with (list of) filename filters for source files
                    for which dependecies should be made.
                    Default is to use all Fortran files in the current directory.
          expl
                    Dictionairy with explicit dependencies that are otherwise
                    not recoqnized by 'makedepf90', for example files that
                    are no modules. Example:
                      { 'main.o' : ['tools.o','other.o',..], ... }

        Data:
          lines
                    List with dependency lines for a Makefile.
                    Each line has the form:
                      file.o : dependency.o ...
        """

        # modules:
        import logging
        import subprocess

        # tools:
        import go_subprocess

        # external program:
        prog = 'makedepf90'

        # Check for the existence of makedepf90 on this machine:
        try :
            p = go_subprocess.call( [prog,'--version'] )
        except go_subprocess.CallingError as err :
            logging.error( err )
            logging.error( '')
            logging.error( '*******************************************************')
            logging.error( 'Cannot access a version of %s' % prog )
            logging.error( '(or something wrong with call to subprocess).')
            logging.error( 'If it is installed, please check your PATH variable.' )
            logging.error( 'If it is not installed, please install %s .' % prog )
            logging.error( 'For the source code and instruction, see:'  )
            logging.error( '  http://personal.inet.fi/private/erikedelmann/makedepf90/' )
            logging.error( '*******************************************************')
            logging.error( '')
            raise IOError( '%s not found' % prog )
        #endtry

        # get version lines:
        #  ['makedepf90 version 2.8.8',...]
        line = p.stdout[0]
        # extract version number:
        fields = line.split()
        if len(fields) < 3 :
            logging.error( 'could not extract makedepf90 version from line: %s' % line )
            raise Exception
        #endif
        version = fields[2]
        # switch:
        if version == '2.3' :
            # command to generate a makefile:
            command = 'makedepf90 -W %s %s' % (flags,files)  # 2.3 binaries
        else :
            # latest with extra warning:
            command = 'makedepf90 -Wmissing -Wconfused %s %s' % (flags,files)  # 2.8.8
        #endif

        # info ...
        logging.info( '    ' )
        logging.info( '    WARNING - Running makedepf90 ...' )
        logging.info( '    WARNING - If the next command takes too much time and' )
        logging.info( '    WARNING - seems to be in an infinite loop, try to run:' )
        logging.info( '    WARNING -   dos2unix' )
        logging.info( '    WARNING - on all your source files.' )
        logging.info( '    WARNING - You probably used a Windows editor,' )
        logging.info( '    WARNING - and makedepf90 does not like that ...' )
        logging.info( '    ' )
        logging.info( '    run command: %s' % command )
        # run command:
        try :
            # run as a shell command since the file list is probably '*.F90' :
            p = go_subprocess.call( command, shell=True )
        except go_subprocess.CallingError as err :
            logging.error( err )
            raise Exception
        except go_subprocess.StatusError as err :
            for line in err.stderr : logging.error(line)
            logging.error( err )
            raise Exception
        #endtry
        
        # extract output lines:
        lines = p.stdout
        # translate into dictionary with lists,
        # start with empty:
        self.deps = {}
        # loop over lines:
        for line in lines :
            # should have a ':' as seperation:
            if ':' not in line :
                logging.error( 'found dependency line without ":" :' )
                logging.error( line )
                raise Exception
            #endif
            # split:
            target,objs = line.split(':',2)
            # cleanup:
            target = target.strip()
            # store in dictionairy as list:
            self.deps[target] = objs.split()
            # also explicitly defined dependencies ?
            if target in expl.keys() :
                # extend list:
                self.deps[target].extend( expl[target] )
            #endif
        #endfor
        
        # rewrite lines, including the explicitly added dependencies:
        self.lines = []
        for target in self.deps.keys() :
            # start:
            line = '%s :' % target
            # add objects:
            for obj in self.deps[target] : line = line+' '+obj
            # store:
            self.lines.append( line )
        #endfor
        
    #enddef   # __init__
    
    # *
    
    def GetObjects( self, main ) :
    
        """
        Return character string of objects on which a main program depends.
        
        Arguments:
          main
                    Basename of the main program, e.g. 'myprog' .
                    The corresponding objectfile 'myprog.o' is the first value
                    on the returned list. Other object names are added given
                    the dependency information.
        """
        
        # first object file:
        obj = '%s.o' % main
        # init list of object files:
        objs = [obj]
        # loop until length is not changing anymore:
        nobj = len(objs)
        iloop = 1
        while True :
            # loop over current objects:
            for target in objs :
                # has dependencies ?
                if target in self.deps.keys() :
                    # loop over dependencies:
                    for obj in self.deps[target] :
                        # only object files ...
                        if not obj.endswith('.o') : continue
                        # add if not in list yet:
                        if obj not in objs : objs.append( obj )
                    #endfor
                #endif  # target with dependencies
            #endfor  # loop over current objects
            # new length:
            n = len(objs)
            # same ? then finished:
            if n == nobj : break
            # reset length:
            nobj = n
        #endwhile
        
        # sort the values:
        objs.sort()
        
        # write to string:
        line = ''
        for obj in objs : line = line+(' %s' % obj)
        
        # return clean list:
        return line.strip()
        
    #enddef
    
#endclass  # Dependencies
