#
# LE run tools
#


# ***


def Command_Line( rcf, exe, args, in_debugger ) :

    """
    Return command line.
    
    ARGUMENTS
        rcf
                Rcfile with settings.
        exe
                Name of executable.
        args
                Arguments to be passed to executable.
        in_debugger
                Set to 'True' if the job should be run in a debugger.

    RETURN VALUES
        cmndline
                Command line to be executed.
    """

    # external
    import socket
    import logging
    
    # mpi run ?
    if rcf.get('par.mpi','bool') :
         
        # number of mpi tasks:
        ntask = rcf.get('par.ntask','int')
 
        # get command line:
        cmnd_exec = rcf.get('mpirun.command')
        cmnd_args = rcf.get('mpirun.args'   )
 
        # write command file ?
        cmdfile = rcf.get('mpirun.cmdfile',default='')
        if len(cmdfile) > 0 :
            # write command line for each task:
            f = open(cmdfile,'w')
            for i in range(ntask) : f.write( '%s %s\n' % (exe,args) )
            f.close()
        else :
            # otherwise, add the executable and its arguments:
            cmnd_args = '%s %s %s' % (cmnd_args,exe,args)
        #endif

        # write host file ?
        hostfile = rcf.get('mpirun.hostfile',default='')
        if len(hostfile) > 0 :
            # get hostname:
            hname = socket.gethostname()
            # write hostname for each task:
            f = open(hostfile,'w')
            for i in range(ntask) : f.write( '%s\n' % hname )
            f.close()
        #endif
            
    else :
        
        # standard run:
        cmnd_exec = exe
        cmnd_args = args
            
    #endif

    # run in debugger ?
    if in_debugger :

        # debugger type:
        debugger = rcf.get( 'debugger' )
        # get debugger command:
        debugger_call = rcf.get( 'debugger.command' )
        # large differences ...
        if debugger == 'totalview' :
            # syntaxis: totalview <executable> [-a <arguments>]
            # pass executable:
            cmndline = '%s %s' % (debugger_call,cmnd_exec)
            # add arguments ?
            if len(cmnd_args) > 0 :
                cmndline = '%s -a %s' % (cmndline,cmnd_args)
            #endif
        elif debugger == 'idb' :
            # syntaxis: idb [-args <executable> <arguments>]
            # fill executable and arguments:
            cmndline = '%s -args %s %s' % (debugger_call,cmnd_exec,cmnd_args)
        else :
            logging.error('unsupported debugger : %s' % debugger )
            raise Exception
        #endif
        
    else :
    
        # standard line:
        cmndline = '%s %s' % (cmnd_exec,cmnd_args)
    
    #endif
    
    # special runner command ? e.g. :
    #   dplace -s1 test.x args
    runner_command = rcf.get( 'runner.command', default='' )
    # apply ?
    if len(runner_command) > 0 :
        # add command line as argument to runner command:
        cmndline = runner_command+' '+cmndline
    #endif
    
    # ok
    return cmndline
    
#endif


# ***


def WriteAndSubmitNewJob( rcfile, bindir, modpath ) :

    """
    Write first or next rcfile and job files(s) in the job chain;
    if chain is not finished yet, submit a new job.
    
    The argument could be the name of an rcfile or an rcfile object itself,
    since the submit scrip might have changed some values given
    the provided command line arguments.
    
    The following function is used:
    
      submit_le_setup_rcfile.WriteRcfile         # writes the new rcfile
    
    This is placed in a seperate file since users might need to
    change these routines for their specific projects.
    """
    
    # external:
    import sys
    import logging
    import rc
    import traceback
    
    # import setup module:
    import submit_le_setup_rcfile
    
    # name provided ?
    if type(rcfile) == str :
        # load:
        rcf = rc.RcFile( rcfile )
    else :
        # just copy ..
        rcf = rcfile
    #endif

    # write next rcfile, return name:
    try :
        rcfile_next = submit_le_setup_rcfile.WriteRcfile( rcf ) 
    except :
        logging.error( sys.exc_info()[1] )
        logging.error( 'exception from WriteRcfile' )
        raise Exception
    #endtry
    
    # finished ?
    if rcfile_next == None :
        # info ...
        logging.info( '  end of job chain !' )
    else :
        # write job file(s) for this period and return the (first) name;
        # last command in a file should submit the next job if necessary:
        logging.info( '  write jobfile for %s ...' % rcfile_next )
        try :
            jobfile_next = WriteJob( rcfile_next, bindir, modpath )
        except :
            for line in traceback.format_exc().split('\n') : logging.error(line)
            logging.error( 'exception from WriteJob' )
            raise Exception
        #endtry
        # start ...
        logging.info( '  submit next job : %s' % jobfile_next )
        try :
            SubmitJob( jobfile_next, rcfile_next )
        except :
            logging.error( sys.exc_info()[1] )
            logging.error( 'exception from SubmitJob' )
            raise Exception
        #endtry
    #endif
    
    # ok
    return
    
#endif


# ***


def WriteJob( rcfile, bindir, modpath ) :

    """
    jobfile = WriteJob(rcfile)
    Write job file given the settings in rcfile.
    The name of the jobfile is based on the name of the rcfile.
    The last command in the job should submit the next job,
    and the script is therefore written in python.
    """
    
    # external:
    import os
    import rc
    
    # load settings:
    rcf = rc.RcFile( rcfile )
    
    # basename for scripts etc is name of rcfile minus extension:
    bname,ext = os.path.splitext(rcfile)    
    
    # loadleveler supports master job with multiple steps:
    with_master_job = (rcf.get('submit.to') == 'queue') and (rcf.get('queue') == 'loadleveler')

    # which python shell ?
    job_shell_python = rcf.get( 'job.shell.python', default='/usr/bin/env python' )

    # start master job ?    
    if with_master_job :

        # name of jobfile:
        jobfile = '%s.jb' % bname

        # set header:
        header = []
        header.append( '#! %s\n' % job_shell_python )
        header.append( '\n' )

        # init queue options:
        qopt = QueueOptions( bname, rcf, 'default' )
    
        # init job file:
        job = []
        job.append( '# external:\n' )
        job.append( 'import os\n' )
        job.append( 'import sys\n' )
        job.append( 'import subprocess\n' )
        job.append( 'import logging\n' )
        job.append( '\n' )
        job.append( '# setup messages:\n' )
        job.append( "logging.basicConfig( format='%(lineno)-4s:%(filename)-30s [%(levelname)-8s] %(message)s', level=logging.INFO, stream=sys.stdout )\n" )
        job.append( '\n' )
        job.append( '# prepend locations of python modules to search path:\n' )
        job.append( "sys.path.insert( 0, '%s' )\n" % bindir )
        job.append( '\n' )
        job.append( '# tools:\n' )
        job.append( 'import submit_le_tools\n' )
        job.append( '\n' )
        # load module stuff ?
        modname = rcf.get( 'env.module.file', default='' )
        if len(modname) > 0 :
            f.write( "# setup enviroment using a module file:\n" )
            f.write( "import EnvMod\n" )
            f.write( "EnvMod.Module( 'use', '%s' )\n" % modpath )
            f.write( "EnvMod.Module( 'load', '%s' )\n" % modname )
            f.write( "EnvMod.Module( 'unuse', '%s' )\n" % modpath )
            f.write( '\n' )
        #endif
        # continue:
        job.append( '# current loadleveler steps:\n' )
        job.append( 'step_name = os.getenv("LOADL_STEP_NAME")\n' )
        job.append( '\n' )
        
    #endif

    # job step names:
    steps = rcf.get('job.steps').split(' ')
    # number of steps:
    nstep = len(steps)
    
    # loop over job steps:
    for istep in range(nstep) :

        # current:
        step = steps[istep]
        # next:
        if istep < nstep-1 : step_next = steps[istep+1]

        # list with queue option lines for this step:
        qopt_step = QueueOptions( bname, rcf, step )

        # call to acutal script:
        if step == 'run' :
            # get command line:
            exe  = os.path.join( os.curdir, rcf.get('job.step.%s.exe' % step) )
            args = rcfile
            indb = rcf.get('submit.debugger','bool')
            cmndline = Command_Line( rcf, exe, args, indb )
            # replace templates:
            cmndline = cmndline.replace( '%{step}', step )
            # <script> <commandline>
            step_command = '["%s/submit_le_step_%s","%s"]' % (bindir,step,cmndline)
        else :
            # <script> <rcfile>
            step_command = '["%s/submit_le_step_%s","%s","--bindir=%s"]' % (bindir,step,rcfile,bindir)
        #endif

        # add queue options to destintation:
        if with_master_job :

            # add to queue options for master job:
            qopt = qopt + qopt_step

            # add lines to run the step:        
            job.append( '# which step ?\n' )
            job.append( 'if step_name == "%s" :\n' % step )
            job.append( '    \n' )
            job.append( '    # run:\n' )
            job.append( '    retcode = subprocess.call( %s )\n' % step_command )
            job.append( '    if retcode != 0 :\n' )
            job.append( '        logging.error( sys.exc_info()[1] )\n' )
            job.append( '        logging.error( \'exception from subprocess call to : %s\' )\n' % step_command )
            job.append( '        sys.exit(1)\n' )
            job.append( '    #endif\n' )
            job.append( '    \n' )
            ## last step ? then add lines to submit next job:
            #if istep == nstep-1 :
            #    job.append( '    # write and submit next job if necessary:\n' )
            #    job.append( '    submit_le_tools.WriteAndSubmitNewJob( "%s", "%s" )\n' % (rcfile,bindir) )
            #    job.append( '    \n' )
            ##endif
            # close step:
            job.append( '#endif\n' )
            job.append( '\n' )

        else :   # no master job, but seperate files

            # name of step job to be written:
            step_job_template = bname+'_%s.jb'

            # actual name:
            step_job = step_job_template % step
            # open:
            f = open( step_job, 'w' )

            # write header:
            f.write( '#! %s\n' % job_shell_python )
            f.write( '\n' )

            # add queue options:
            for line in qopt_step : f.write(line)

            # add lines to call the actual script:
            f.write( '# external:\n' )
            f.write( 'import sys\n' )
            f.write( 'import os\n' )
            f.write( 'import logging\n' )
            f.write( 'import subprocess\n' )
            f.write( '\n' )
            f.write( '# prepend locations of python modules to search path:\n' )
            f.write( "sys.path.insert( 0, '%s' )\n" % bindir )
            f.write( '\n' )
            f.write( '# tools:\n' )
            f.write( 'import rc\n' )
            f.write( 'import submit_le_tools\n' )
            f.write( '\n' )
            # load module stuff ?
            modname = rcf.get( 'env.module.file', default='' )
            if len(modname) > 0 :
                f.write( "# setup enviroment using a module file:\n" )
                f.write( "import EnvMod\n" )
                f.write( "EnvMod.Module( 'use', '%s' )\n" % modpath )
                f.write( "EnvMod.Module( 'load', '%s' )\n" % modname )
                f.write( "EnvMod.Module( 'unuse', '%s' )\n" % modpath )
                f.write( '\n' )
            #endif
            # OpenMP environment?
            with_openmp = rcf.get( 'par.openmp', 'bool' )
            if (step == 'run') and with_openmp :
                # number of threads:
                nthread = rcf.get( 'par.nthread', 'int' )
                # add env settings:
                f.write( "# define number of OpenMP threads:\n" )
                f.write( "os.putenv( 'OMP_NUM_THREADS', '%i' )\n" % nthread )
                f.write( '\n' )
            #endif
            # continue:
            f.write( '# setup messages:\n' )
            f.write( "logging.basicConfig( format='%(lineno)-4s:%(filename)-30s [%(levelname)-8s] %(message)s', level=logging.INFO, stream=sys.stdout )\n" )
            f.write( '\n' )
            f.write( '# info ...\n' )
            f.write( 'logging.info( "start" )\n' )
            f.write( '\n' )
            f.write( '# go to run directory:\n' )
            f.write( 'os.chdir("%s")\n' % os.getcwd() )
            f.write( '\n' )
            f.write( '# call user script:\n' )
            f.write( 'retcode = subprocess.call( %s )\n' % step_command )
            f.write( 'if retcode != 0:\n' )
            f.write( '    logging.error( sys.exc_info()[1] )\n' )
            f.write( '    logging.error( \'exception from subprocess call to : %s\' )\n' % step_command )
            f.write( '    sys.exit(1)\n' )
            f.write( '#endif\n' )
            f.write( '\n' )

            # add submission of next step?
            if istep < nstep-1 :
                # job script of next step:
                step_job_next = step_job_template % step_next
                # add submission command:
                f.write( '# submit next step:\n' )
                f.write( 'try :\n' )
                f.write( '    submit_le_tools.SubmitJob( "%s", "%s", step="%s" )\n' % (step_job_next,rcfile,step_next)  )
                f.write( 'except:\n' )
                f.write( '    logging.error( sys.exc_info()[1] )\n' )
                f.write( '    logging.error( \'exception from SubmitJob( "%s", "%s", step="%s" )\' )\n' % (step_job_next,rcfile,step_next) )
                f.write( '    sys.exit(1)\n' )
                f.write( '#endtry\n' )
                f.write( '\n' )
            #else :
            #    # last step; might be necessary to submit a new job:
            #    f.write( '# write and submit next job if necessary:\n' )
            #    f.write( 'submit_le_tools.WriteAndSubmitNewJob( "%s", "%s" )\n' % (rcfile,bindir) )
            #    f.write( '\n' )
            #endif
            f.write( '# info ...\n' )
            f.write( 'logging.info( "end" )\n' )
            f.write( '\n' )

            # close:
            f.close()

            # make it executable and readible for all, writable for user only:
            #                   u+r    u+w    u+x    g+r    g-w    g+x    o+r    o-w    o+x
            os.chmod( step_job, 2**8 + 2**7 + 2**6 + 2**5 + 0    + 2**3 + 2**2 + 0    + 2**0 )
            
            # fill return value:
            if istep == 0 : jobfile = step_job

        #endif  # master job of chain
        
    #endfor  # steps

    # write master job:
    if with_master_job :
        # combine:
        job = header + qopt + job
        # write:
        f = open(jobfile,'w')
        f.writelines(job)
        f.close()
        # make it executable and readible for all, writable for user only:
        #                  u+r    u+w    u+x    g+r    g-w    g+x    o+r    o-w    o+x
        os.chmod( jobfile, 2**8 + 2**7 + 2**6 + 2**5 + 0    + 2**3 + 2**2 + 0    + 2**0 )
    #endif
     
    # ok
    return jobfile

#enddef


# ***


def QueueOptions( bname, rcf, step ) :
    
    """
    Return list with queue option lines.
    """

    # submit to queue ?
    if rcf.get('submit.to') == 'queue' :
    
        # queue type:
        queue = rcf.get('queue')

        # different options and commands:
        if queue == 'loadleveler' :
            qopt = QueueOptions_LoadLeveler( bname, rcf, step )
        elif queue == 'bsub' :
            qopt = QueueOptions_BSub( bname, rcf, step )
        elif queue == 'qsub' :
            qopt = QueueOptions_QSub( bname, rcf, step )
        elif queue == 'slurm' :
            qopt = QueueOptions_Slurm( bname, rcf, step )
        else :
            # not supported ...
            logging.error( 'unsupported queue : %s' % queue )
            raise Exception
        #endif

    else :
    
        # sumitting to shell (background or foreground):
        qopt = ShellOptions( bname, rcf, step )
        
    #endif

    # ok
    return qopt
    
#enddef


# ***


def SubmitJob( job_script, rcfile, step=None ) :

    """
    Submit jobscript.
    Where to submit to (foreground,background, queue) is read from rcfile settings.
    """

    # external:
    import sys
    import logging
    import rc
    
    # read settings:
    rcf = rc.RcFile( rcfile )
    
    # where to ?
    submit_to = rcf.get('submit.to')
    # info ...
    logging.info( 'submit %s to %s ...' % (job_script,submit_to) )

    # allowed destination:
    allowed = rcf.get('submit.to.allowed').split()
    # check ...
    if submit_to not in allowed :
        logging.error( 'unsupported run environment : %s' % submit_to )
        logging.error( '  on this machine allowed   : %s' % str(allowed) )
        raise Exception
    #endif
        # call specific submit routines:
    if submit_to == 'foreground' :

        # call run script, catch errors:
        try :
            Run_Job_In_Foreground( job_script )
        except :
            logging.error( sys.exc_info()[1] )
            logging.error( 'from Run_Job_In_Foreground for %s' % job_script )
            raise Exception
        #endtry
        
    elif submit_to == 'background' :
    
        # call run script, catch errors:
        try :
            Submit_Job_To_Background( job_script, rcf )
        except :
            logging.error( 'from Submit_Job_To_Background for %s' % job_script )
            raise Exception
        #endtry

    elif submit_to == 'queue' :
    
        # queue type:
        queue = rcf.get('queue')

        # different options and commands:
        if queue == 'loadleveler' :
            Submit_Job_To_LoadLeveler( job_script, rcf, step=step )
        elif queue == 'bsub' :
            Submit_Job_To_BSub( job_script, rcf, step=step )
        elif queue == 'qsub' :
            Submit_Job_To_QSub( job_script, rcf, step=step )
        elif queue == 'slurm' :
            Submit_Job_To_Slurm( job_script, rcf, step=step )
        else :
            # not supported ...
            logging.error( 'unsupported queue : %s' % queue )
            raise Exception
        #endif

    else :
    
        # not supported ...
        logging.error( 'unsupported run environment : %s' % submit_to )
        sys.exit(1)
        
    #endif

    # ok
    return
    
#endif    


# ======================================================================
# ===
# === foreground
# ===
# ======================================================================


def Run_Job_In_Foreground( job_script ) :

    """
    Run job script in foreground.
    """
    
    # external:
    import sys
    import os
    import logging
    import subprocess

    # setup command line, e.g. './myscript.jb' :
    command = os.path.join(os.curdir,job_script)

    # execute:
    retcode = subprocess.call( command )
    if retcode != 0 :
        logging.error( sys.exc_info()[1] )
        logging.error( 'from subprocess call to : %s' % command )
        raise Exception
    #endif
    
    # ok
    return

#enddef


# ======================================================================
# ===
# === background
# ===
# ======================================================================


def  Submit_Job_To_Background( job_script, rcf ) :

    """
    Submit job to background.
    """
    
    # external:
    import sys
    import os
    import logging
    import subprocess

    # basename for scripts etc is name of rcfile minus extension:
    bname,ext = os.path.splitext(job_script)    
    
    # output files:    
    job_stdout = bname+'.out'
    job_stderr = bname+'.err'
    job_info   = bname+'.info'

    # setup command line, e.g. './myscript.jb' :
    command = os.path.join(os.curdir,job_script)

    # re-direct standard output:
    command  = command+' > %s' % job_stdout
    # write error messages to seperate file:
    #command  = command+' 2> %s' % job_stderr
    # write error messages to same file:
    command  = command+' 2>&1'
    
    # run in background, return process object:
    logging.info( 'run shell command : "%s" ...' % command )
    p = subprocess.Popen( command, shell=True )

    # info ...
    infotext = []
    infotext.append( '\n' )
    infotext.append( 'Summary:\n' )
    infotext.append( '\n' )
    infotext.append( '  job script      : %s\n' % job_script )
    infotext.append( '  standard output : %s\n' % job_stdout )
    infotext.append( '  standard error  : %s\n' % job_stderr )
    infotext.append( '\n' )
    infotext.append( 'Process snapshot:\n')
    infotext.append( '\n')
    p2 = subprocess.Popen( '/bin/ps -f -p %i' % p.pid, shell=True, 
                             stdout=subprocess.PIPE, stderr=subprocess.STDOUT )
    for line in p2.stdout.readlines() : infotext.append( line )
    infotext.append( '\n')
    infotext.append( 'To manage this process:\n' )
    infotext.append( '\n' )
    infotext.append( '  # show process snapshot:\n' )
    infotext.append( '  ps -f -p %i\n' % p.pid )
    infotext.append( '  \n' )
    infotext.append( '  # kill process:\n' )
    infotext.append( '  kill %i\n' % p.pid )
    infotext.append( '  \n' )
    infotext.append( '  # follow standard output:\n' )
    infotext.append( '  tail -f %s\n' % job_stdout )
    infotext.append( '\n' )

    # write to log:
    for line in infotext : logging.info( line.strip() )

    # write to file:
    f = open( job_info, 'w' )
    f.writelines(infotext)
    f.close()
        
    # wait ?
    wait = rcf.get( 'submit.background.wait', 'bool' )
    if wait :
        logging.info( 'wait for completion ...' )
        returncode = p.wait()
        logging.info( '   ... finished; returncode : %i' % returncode )
    #endif
    
    # ok
    return
    
#enddef


# ======================================================================
# ===
# === Shell options
# ===
# ======================================================================


def ShellOptions( bname, rcf, step ) :

    """
    Return list with shell settings (in python).
    """

    # read shell lines directly from rcfile;
    # seperated by '\n' texts:
    lines = rcf.get( 'shell.options.%s' % step, default='' ).split('\\n')
    # add including newline:
    qopt = []
    for line in lines : qopt.append( '%s\n' % line.strip() )
    qopt.append( '\n' )

    # ok
    return qopt
    
#enddef




# ======================================================================
# ===
# === LoadLeveler queue
# ===
# ======================================================================


def QueueOptions_LoadLeveler( bname, rcf, step ) :

    """
    Return list with queue options.
    """

    # external:
    import math
    
    # init result:
    qopt = []

    # which step ?
    if step == 'default' :
    
        # list with options:
        opts = rcf.get( 'queue.ll.options.%s' % step ).split()
        # default options:
        for opt in opts :
            # get value:
            val = rcf.get( 'queue.ll.option.%s.%s' % (step,opt) )
            # write:
            qopt.append( '#@ %-20s = %s\n' % (opt,val) )
        #endfor
        # layout ...
        qopt.append( '\n' )

    else :
    
        # list with options:
        opts = rcf.get( 'queue.ll.options.%s' % step ).split()
        # default options:
        for opt in opts :
            # get value:
            val = rcf.get( 'queue.ll.option.%s.%s' % (step,opt) )
            # to be set ?
            if val == '<auto>' :
                # differs per option ...
                if opt == 'output' :
                    val = '%s_%s.out' % (bname,step)
                elif opt == 'error' :
                    val = '%s_%s.err' % (bname,step)
                #endif
            #endif
            # no, empty, or normal value ?
            if val == '<none>' :
                # skip this keyword:
                continue
            elif val == '' :
                # just the keyword:
                qopt.append( '#@ %s\n' % opt )
            else :
                # keyword and value:
                qopt.append( '#@ %-20s = %s\n' % (opt,val) )
            #endif
        #endfor
        # layout ...
        qopt.append( '\n' )
                
    #endif
    
    # ok
    return qopt
    
#enddef


# ***


def Submit_Job_To_LoadLeveler( job_script, rcf, step=None ) :

    """
    Submit job to LoadLeveler queue.
    """
    
    # external:
    import sys
    import os
    import logging
    import subprocess

    # basename for scripts etc is name of rcfile minus extension:
    bname,ext = os.path.splitext(job_script)    
    
    # output files:    
    job_info   = bname+'.info'

    # options passed directly to submission command:
    qopts = rcf.get( 'queue.ll.args' )
    # add step specific options:
    if step != None : qopts = qopts+' '+rcf.get('queue.bsub.args.%s' % step )
    # add options passed to submit script:
    qopts = qopts+' '+rcf.get('submit.options')

    # info ...
    logging.info( '    launch ...' )

    # setup command line:
    command = 'llsubmit '+qopts
    # last argument is script:
    command = command+' '+job_script

    # info ...
    logging.info( '      command: %s' % command )
    
    # init submission info file:
    infotext = []
    infotext.append(  '\n' )

    # call submit command, trap errors:
    try:
        # submit; redirect errors to standard output:
        p = subprocess.Popen( command.split(), stdout=subprocess.PIPE, stderr=subprocess.STDOUT )
    except :
        logging.error( sys.exc_info()[1] )
        logging.error( 'from subprocess.Popen( %s )' % command.split() )
        raise Exception
    #endtry
    # extract:
    outlines = p.stdout.readlines()
    # add to help info message:
    infotext = infotext + outlines
    # extract job id from last line:
    #   llsubmit: The job "c1a0303.4290133" with 3 job steps has been submitted.
    firstwords = 'llsubmit: The job'
    lastline = outlines[-1]
    if lastline.startswith(firstwords) :
        job_id = lastline.lstrip(firstwords).split()[0].replace('"','')
    else :
        job_id = '<job-id>'
    #endif
    
    # add help text to submission info:
    infotext.append(  '\n' )
    infotext.append(  'To manage LoadLeveler jobs:\n' )
    infotext.append(  '\n' )
    infotext.append(  '  llq [-u ${USER}]         # list [your] current jobs\n' )
    infotext.append(  '  llq %s             # list this job\n' % job_id )
    infotext.append(  '  llcancel %s        # kill this job\n' % job_id )
    infotext.append(  '\n' )

    # write to log:
    for line in infotext : logging.info( line.rstrip() )

    # write to file:
    f = open( job_info, 'w' )
    f.writelines(infotext)
    f.close()
    
    # ok
    return

#enddef
        

# ======================================================================
# ===
# === BSUB queue
# ===
# ======================================================================


def QueueOptions_BSub( bname, rcf, step ) :

    """
    Return list with queue options.
    """
    
    # modules:
    import logging

    # init result:
    qopt = []

    ## specials ...
    #if step == 'run' :    
    #    # mpi job ?
    #    if rcf.get('par.mpi','bool') :
    #        # number of MPI tasks:
    #        ntask = rcf.get('par.ntask','int')
    #        # parallel job:
    #        bsub_n = ntask
    #    else:
    #        # serial job:
    #        bsub_n = 1
    #    #endif
    #else :
    #    # serial step:
    #    bsub_n = 1
    ##endif

    # lists with options:
    opts_default = rcf.get( 'queue.bsub.options.default' ).split()
    opts_step    = rcf.get( 'queue.bsub.options.%s' % step ).split()
    # expand:
    for opt in opts_default+opts_step :
        # get value:
        if opt in opts_default :
            val = rcf.get( 'queue.bsub.option.%s' % opt )
        else :
            val = rcf.get( 'queue.bsub.option.%s.%s' % (step,opt) )
        #endif
        # value starts with flag ?
        if val.startswith('-') :
            # split in flag and remaining values:
            flag,val = val.split(None,1)
        else :
            # flag is the option:
            flag = '-%s' % opt
        #endif
        # to be set ?
        if val == '<auto>' :
            # differs per option ...
            if opt in ['o','oo'] :
                val = '%s_%s.out' % (bname,step)
            elif opt in ['e','eo'] :
                val = '%s_%s.err' % (bname,step)
            #elif opt == 'n' :
            #    val = str(bsub_n)
            else :
                logging.error( 'could not set "%s" for BSUB queue option "%s" in step "%s"' % (val,opt,step) )
                logging.error( 'from call : %s' % command )
            #endif
        #endif  # automatic setting
        # fill option line:
        qopt.append( '#BSUB %s %s\n' % (flag,val) )
    #endfor
    # layout ...
    qopt.append( '\n' )

    # read queue specific shell lines directly from rcfile;
    # seperated by '\n' texts:
    lines_default = rcf.get( 'queue.bsub.code.%s' % 'default' ).split('\\n')
    lines_step    = rcf.get( 'queue.bsub.code.%s' % step ).split('\\n')
    # add including newline:
    for line in lines_default+lines_step :
        qopt.append( '%s\n' % line.strip() )
    #endfor
    # layout ...
    qopt.append( '\n' )
                
    # ok
    return qopt
    
#enddef   # QueueOptions_BSub


# ***


def Submit_Job_To_BSub( job_script, rcf, step=None ) :

    """
    Submit job to BSUB queue.
    """
    
    # external:
    import os
    import logging
    import subprocess

    # basename for scripts etc is name of rcfile minus extension:
    bname,ext = os.path.splitext(job_script)    
    
    # output files:    
    job_info   = bname+'.info'

    # options passed directly to submission command:
    qopts = rcf.get( 'queue.bsub.args' )
    # add step specific options:
    if step != None : qopts = qopts+' '+rcf.get('queue.bsub.args.%s' % step )
    # add options passed to submit script:
    qopts = qopts+' '+rcf.get('submit.options')
    
    # info ...
    logging.info( '    launch ...' )

    # setup command line:
    command = 'bsub '+qopts

    # info ...
    logging.info( '      command: %s < %s' % (command,job_script) )

    # open job script for reading the script:
    fjb = open( job_script, 'r' )
    # prepare for OS errors (file does not exist etc.)    
    try:
        # submit; read job script from std.input,
        # redirect errors to standard output:
        p = subprocess.Popen( command.split(), 
                                stdin=fjb, 
                                stdout=subprocess.PIPE, 
                                stderr=subprocess.PIPE )
    except OSError as err :
        logging.error( 'OSError: '+err.strerror )
        logging.error( 'from call : %s' % command )
        raise Exception
    #endtry
    # exchange input and output:
    stdout,stderr = p.communicate()
    # convert:
    stdout = stdout.decode('utf-8')
    stderr = stderr.decode('utf-8')
    # remove final end-of-lines, split at remaining new lines:
    stdout = stdout.strip('\n').split('\n')
    stderr = stderr.strip('\n').split('\n')
    # display:
    for line in stdout : logging.info ( '      %s' % line.rstrip() )
    for line in stderr : logging.info ( '      %s' % line.rstrip() )
    # standard output is:
    #   <jobname> <jobnr>
    # extract job nr from first line:
    job_nr = stdout[0].split()[1].strip('<>')
    # close job script:
    fjb.close()

    # info ...
    infotext = []
    infotext.append(  '\n' )
    infotext.append(  'Summary:\n' )
    infotext.append(  '\n' )
    infotext.append(  '  current dir     : %s\n' % os.getcwd() )
    infotext.append(  '  job script      : %s\n' % job_script )
    infotext.append(  '\n' )
    infotext.append(  'To manage this job:\n' )
    infotext.append(  '  \n' )
    infotext.append(  '  # kill job:\n' )
    infotext.append(  '  bkill %s\n' % job_nr )
    infotext.append(  '  \n' )
    infotext.append(  'To show all your running jobs:\n' )
    infotext.append(  '  \n' )
    infotext.append(  '  bjobs\n' )
    infotext.append(  '  \n' )

    # write to log:
    for line in infotext : logging.info( line.rstrip() )

    # write to file:
    f = open( job_info, 'w' )
    f.writelines(infotext)
    f.close()
    
    # ok
    return

#enddef # Submit_Job_To_BSub
        

# ======================================================================
# ===
# === QSUB queue
# ===
# ======================================================================


def QueueOptions_QSub( bname, rcf, step ) :

    """
    Return list with queue options.
    """

    # init result:
    qopt = []

    # read the value for the '-pe' option:
    qsub_pe = rcf.get( 'queue.qsub.option.pe.%s' % step )


    # read the value for the '-l' resource specification option:
    qsub_l = rcf.get( 'queue.qsub.option.l.%s' % step )

    # list with options:
    opts = rcf.get( 'queue.qsub.options' ).split()
    # default options:
    for opt in opts :
        if opt == 'pe':
            val = qsub_pe
        elif opt == 'l':
            val = qsub_l
        else:
            # get value:
            val = rcf.get( 'queue.qsub.option.%s' % opt )

        # fill option line:
        qopt.append( '#$ -%s %s\n' % (opt,val) )
    #endfor
    # layout ...
    qopt.append( '\n' )
                
    # ok
    return qopt
    
#enddef


# ***


def Submit_Job_To_QSub( job_script, rcf, step=None ) :

    """
    Submit job to QSUB queue.
    """
    
    # external:
    import os
    import logging
    import subprocess

    # basename for scripts etc is name of rcfile minus extension:
    bname,ext = os.path.splitext(job_script)    
    
    # output files:    
    job_info   = bname+'.info'

    # options passed directly to submission command:
    qopts = rcf.get( 'queue.qsub.args' )
    # add step specific options:
    if step != None : qopts = qopts+' '+rcf.get('queue.bsub.args.%s' % step )
    # add options passed to submit script:
    qopts = qopts+' '+rcf.get('submit.options')
    
    # info ...
    logging.info( '    launch ...' )

    # setup command line:
    command = 'qsub '+qopts
    # last argument is script:
    command = command+' '+job_script

    # info ...
    logging.info( '      command: %s' % command )

    # prepare for OS errors (file does not exist etc.)    
    try:
        # submit; redirect errors to standard output:
        p = subprocess.Popen( command.split(), stdout=subprocess.PIPE, stderr=subprocess.STDOUT )
    except OSError as err :
        logging.error( 'OSError: '+err.strerror )
        logging.error( 'from call : %s' % command )
        raise Exception
    #endtry
    # extract:
    outlines = p.stdout.readlines()
    # display:
    for line in outlines : logging.info( '      %s' % line.rstrip() )
    # standard output is:
    #    jobnr
    # extract job nr:
    job_nr = outlines[0].split()[0]

    # info ...
    infotext = []
    infotext.append(  '\n' )
    infotext.append(  'Summary:\n' )
    infotext.append(  '\n' )
    infotext.append(  '  current dir     : %s\n' % os.getcwd() )
    infotext.append(  '  job script      : %s\n' % job_script )
    infotext.append(  '\n' )
    infotext.append(  'To manage this job:\n' )
    infotext.append(  '  \n' )
    infotext.append(  '  # kill job:\n' )
    infotext.append(  '  qdel %s\n' % job_nr )
    infotext.append(  '  \n' )
    infotext.append(  'To show all your running jobs:\n' )
    infotext.append(  '  \n' )
    infotext.append(  '  qstat [-u ${USER}]\n' )
    infotext.append(  '  \n' )

    # write to log:
    for line in infotext : logging.info( line.rstrip() )

    # write to file:
    f = open( job_info, 'w' )
    f.writelines(infotext)
    f.close()
    
    # ok
    return

#enddef


        
# ======================================================================
# ===
# === SLURM queue
# ===
# ======================================================================

def QueueOptions_Slurm( bname, rcf, step ) :

    """
    Return list with queue options directives, that will be at the top of
    script.

    Script for run step should contains:

      #SBATCH -n ${par.nthread}*${ntask}
    
      export OMP_NUM_THREADS=${par.nthread}

      mpiexec.hydra -machinefile ./mpd.hosts -np ${ntask} ./$bin
    
    """
    #pdb.set_trace()
    qopt = []                           # init result

    #  (1) list of options
    opts = rcf.get( 'queue.slurm.options' ).split()

    # (2) read value of each option and format it for inclusion into script
    for opt in opts :

        # default value, not step specific:
        val_default = rcf.get( 'queue.slurm.option.%s' % opt )
        # specific value, or default if not defined:
        val = rcf.get( 'queue.slurm.option.%s.%s' % (step,opt), default=val_default )

        if val == '<auto>' :
            if opt in ['o','output'] :
                val = '%s_%s.out' % (bname,step)
            elif opt in ['e','error'] :
                val = '%s_%s.err' % (bname,step)
            #endif
        #endif
         
        # short or long option?
        if len(opt) == 1 :
            if len(val) > 0 :
                qopt.append( '#SBATCH -%s %s\n' % (opt,val) )
            else :
                qopt.append( '#SBATCH -%s\n' % (opt) )
            #endif
        else :
            if len(val) > 0 :
                qopt.append( '#SBATCH --%s=%s\n' % (opt,val) )
            else :
                qopt.append( '#SBATCH --%s\n' % (opt) )
            #endif
        #endif
        
    #endfor
    
    # layout ...
    qopt.append( '\n' )
                
    # ok
    return qopt

#enddef  # QueueOptions_Slurm


# ***


def Submit_Job_To_Slurm( job_script, rcf, step=None ) :

    """
    Submit job to SLURM queue.
    """
    
    # external:
    import os
    import logging
    import subprocess

    # basename for scripts etc is name of rcfile minus extension:
    bname,ext = os.path.splitext(job_script)    
    
    # output files:    
    job_info   = bname+'.info'

    # Options to pass directly unmodified at the submission command :
    # (1) from pycassso-queue-slurm.rc
    qopts = rcf.get( 'queue.slurm.args' )
    if step != None : qopts = qopts+' '+rcf.get('queue.slurm.args.%s' % step)
    # (2) from pycasso-tm5-expert.rc
    qopts = qopts+' '+rcf.get('submit.options')
    
    # info ...
    logging.info( '    Submitting job to Slurm queue' )
    logging.info( '    launch ...' )

    # setup command line:
    command = 'sbatch '+qopts
    
    # last argument is script:
    command = command+' '+job_script

    # info ...
    logging.info( '      command: %s' % command )


    # prepare for OS errors (file does not exist etc.)    
    try:
        # submit; redirect errors to standard output:
        p = subprocess.Popen( command.split(), stdout=subprocess.PIPE, stderr=subprocess.STDOUT )
    except OSError as err :
        logging.error( 'OSError: '+err.strerror )
        logging.error( 'from call : %s' % command )
        raise Exception
    
    # extract:
    outlines = p.stdout.readlines()

    # display:
    for line in outlines : logging.info( '      %s' % line.rstrip() )

    # standard output is:
    #    jobnr
    # extract job nr:
    job_nr = outlines[0].split()[0]

    # info ...
    infotext = []
    infotext.append(  '\n' )
    infotext.append(  'Summary:\n' )
    infotext.append(  '\n' )
    infotext.append(  '  current dir     : %s\n' % os.getcwd() )
    infotext.append(  '  job script      : %s\n' % job_script )
    infotext.append(  '\n' )
    infotext.append(  'To manage this job:\n' )
    infotext.append(  '  \n' )
    infotext.append(  '  # kill job:\n' )
    infotext.append(  '  scancel %s\n' % job_nr )
    infotext.append(  '  \n' )
    infotext.append(  'To show all your running jobs:\n' )
    infotext.append(  '  \n' )
    infotext.append(  '  squeue [-u ${USER}]\n' )
    infotext.append(  '  \n' )

    # write to log:
    for line in infotext : logging.info( line.rstrip() )

    # write to file:
    f = open( job_info, 'w' )
    f.writelines(infotext)
    f.close()
    
    return
    
#enddef  # Submit_Job_To_Slurm
        

# ======================================================================
# ===
# === end
# ===
# ======================================================================

