package april.procman;

import java.io.*;
import java.util.*;

import april.config.*;
import april.util.*;
import april.lcmtypes.*;
import lcm.lcm.*;

/**
 * This is the daemon which is in charge of monitoring all process as instructed by ProcMan.
 *
 * NOTE: In cases where applications fail to exit immediately after
 *        the destroy() call, we are unable to recover any additional
 *        output on stdin/out because the destroy() call closes the
 *        streams before the process is finished. This results in a
 *        stream closure exception (see referral in ReaderThread).
 *        This is mostly because .destroy() is non-blocking.  To work
 *        around this, we would need to re-implement procman in
 *        another language
 */

public class ProcManDaemon implements Runnable
{
    private static final int PROCESS_CHECK_PERIOD = 200; // 5 Hz

    static LCM lcm = LCM.getSingleton();

    HashMap<Integer, ProcRecordD> records = new HashMap<Integer, ProcRecordD>();

    // host defaulted to System.getenv("PROCMAN_HOST");
    String host;

    procman_process_list_t next_proc_list;

    public boolean verbose;

    boolean persistent;

    class ProcRecordD extends ProcRecord
    {
        Process  process; // System.Process, if the process has been started.
        long     lastExitUtime = Long.MAX_VALUE; // utime of when noticed last exit

        boolean  visited; // used to detect processes that disappear in our orders.

        ReaderThread stderrReader, stdoutReader;

        boolean startedOnce;

        public synchronized int getLastExitCode()
        {
            return lastExitCode;
        }

        public synchronized boolean isRunning()
        {
            return stdoutReader != null && stdoutReader.isAlive();
        }

        public synchronized void startProcess()
        {

            // if proc is already running, restart the process.
            while (isRunning()) {
                stopProcess();
                TimeUtil.sleep(50);
            }

            // try to start the process.
            try {
                process = null;
                process = Runtime.getRuntime().exec(cmdline);
            } catch (IOException ex) {
                System.out.println("WRN: On process start: "+ex);
            }

            // If the process started, read stderr/stdout until it dies.
            if (process != null) {
                // create reader threads that will quit automatically
                // when process ends.
                String filename = "/tmp/procman_proc"+procid+".log";
                try {
                    BufferedWriter fouts = new BufferedWriter(new FileWriter(filename));
                    stdoutReader = new ReaderThread(this, process.getInputStream(), 0, fouts);
                    stdoutReader.start();
                    stderrReader = new ReaderThread(this, process.getErrorStream(), 1, fouts);
                    stderrReader.start();
                } catch (IOException ex) {
                    System.out.println("WRN: "+ex);
                }
            }
            startedOnce = true;
        }

        public synchronized void stopProcess()
        {
            if (process != null)
                process.destroy();
        }
    }

    public ProcManDaemon()
    {
        this(System.getenv("PROCMAN_HOST"), false);
    }

    public ProcManDaemon(String host, boolean _persistent)
    {
        persistent = _persistent;
        if (host == null || host.equals("")) {
            host = "localhost";
            System.out.println("NFO: PROCMAN_HOST not defined. Using "+host+" instead");
        } else
            System.out.println("NFO: using host: " + host);

        this.host = host;

        // Runtime.getRuntime().addShutdownHook(new ShutdownHandler());
        lcm.subscribe("PROCMAN_PROCESS_LIST", new MySubscriber());
    }

    /**
     *  Main loop for ProcManDaemon. This has two main functions:
     *    1) Handles the latest process list
     *    2) Restart failed processes according to their policy
     *  NOTE: Under two circumstances, the daemon will attempt to stop all active processes before proceeding:
     *    a) if the exit flag is set in the proc_list
     *    b) if a new proc_list with a newer init time is presented (then all processes from the previous
     *       proc_list are first stopped before the new proc_list may be processed
     */
    public void run()
    {
        long stopUtime = 0; // Keep track of first time when we try to stop all processes
        long lastWarnUtime = Long.MAX_VALUE;
        long lastValidInitUtime = 0;

        while (true) {
            TimeUtil.sleep(PROCESS_CHECK_PERIOD);
            long now = TimeUtil.utime();
            procman_process_list_t orig_list = next_proc_list;

            sendStatusMessage(orig_list);

            // XXX Currently, we exit when the controller exits. This
            // is because we cannot run the daemon as an upstart
            // service because the classpaths are not set correctly.
            if (!persistent && orig_list != null && orig_list.exit && records.size() == 0) {
                System.out.println("NFO: All processes exited, ProcManDaemon exiting.");
                System.exit(0);
            }

            if (orig_list == null)
                continue;

            procman_process_list_t proc_list = orig_list;
            if (proc_list.exit ||                              // a) the controller is exiting
                (proc_list.init_utime > lastValidInitUtime &&  // b) a new controller has started
                 records.size() > 0)) {
                proc_list = null; // Signal to stop all processes
                if (stopUtime == 0)
                    lastWarnUtime = stopUtime = now;
            }

            // 1)
            handleNewProcessList(proc_list);

            if (proc_list != null) {
                // 2)
                manageRunningProcesses(proc_list);

                // 3)
                lastValidInitUtime = proc_list.utime;
                stopUtime = 0;
            } else if (records.size() > 0 && now - lastWarnUtime > 1000000) {
                lastWarnUtime = now;
                System.out.printf("WRN: ProcManDaemon waiting for %d processes to exit after %g seconds\n",
                                  records.size(), (now - stopUtime)/1e6);
            }

        }
    }

    private void handleNewProcessList(procman_process_list_t proc_list)
    {
        // any processes not mentioned in the process list are
        // implicitly killed.  first, we mark all procs as not
        // visisted.
        for (ProcRecordD pr : records.values()) {
            pr.visited = false;
        }

        if (proc_list != null) {
            // Deal with all procs mentioned in the orders.
            for (int i = 0; i < proc_list.nprocs; i++) {
                procman_process_t p = proc_list.processes[i];

                // skip any procs not for us.
                if (!p.host.equals(host))
                    continue;

                ProcRecordD pr = records.get(p.procid);
                if (pr == null) {
                    pr = new ProcRecordD();
                    pr.procid = p.procid;
                    records.put(p.procid, pr);
                }

                pr.cmdline = p.cmdline;
                pr.name = p.name;
                pr.restartDelayMS = p.restart_delay_ms;
                pr.visited = true;
            }
        }

        // any processes that weren't mentioned should be
        // stopped and cleaned up.
        Iterator<ProcRecordD> itr = records.values().iterator();

        for (; itr.hasNext(); ) {
            ProcRecordD pr = itr.next();
            if (!pr.visited) {
                pr.stopProcess();
                if (!pr.isRunning())
                    itr.remove();
            }
        }
    }

    /**
     * This method ensures:
     *    1) that those processes that should be running are
     *    2) that those that should be restarted are
     *    3) that stopped processes can be restarted manually
     */
    private void manageRunningProcesses(procman_process_list_t proc_list)
    {
        for (int i = 0; i < proc_list.nprocs; i++) {
            procman_process_t p = proc_list.processes[i];

            if (!p.host.equals(host)) {
                if (verbose)
                    System.out.println("NFO: Host mismatch. Expected "+host+" got "+p.host);
                continue;
            }

            ProcRecordD pr = records.get(p.procid);
            // Process is stopped and we may need to (re)start it
            if (!pr.isRunning())
            {
                long udiff = TimeUtil.utime() - pr.lastExitUtime;
                if (p.running) {
                    // 1)
                    if(!pr.startedOnce)
                        pr.startProcess();
                    // 2)
                    else if(p.auto_restart &&
                            pr.lastExitCode != 0 &&
                            udiff > pr.restartDelayMS*1000) {
                        pr.stopProcess();
                        pr.stderrReader.publish("Warning: ProcManDaemon auto "+
                                                "restarting this process after "
                                                +udiff/1000+" milliseconds!");
                        pr.startProcess();
                    }
                }// 3)
                else {
                    // If the controller says were stopped, restart this
                    // so when the controller says "go" we're ready
                    pr.startedOnce = false;
                }
            }// Process is running and we may need to stop it
            else if(!p.running)
            {
                pr.stopProcess();
            }
        }
    }

    private void sendStatusMessage(procman_process_list_t proc_list)
    {
        // Send the status message.
        procman_status_list_t psl = new procman_status_list_t();
        psl.utime = TimeUtil.utime();
        psl.host = host;
        psl.nprocs = records.size();
        psl.statuses = new procman_status_t[psl.nprocs];
        psl.received_utime = proc_list == null? 0 : proc_list.utime;
        psl.received_init_utime = proc_list == null? 0 : proc_list.init_utime;

        int idx = 0;
        for (ProcRecordD pr : records.values()) {
            procman_status_t ps = new procman_status_t();
            ps.procid = pr.procid;
            ps.running = pr.isRunning();
            ps.last_exit_code = pr.getLastExitCode();
            ps.restarts = pr.restartCount;
            psl.statuses[idx] = ps;
            idx++;
        }

        lcm.publish("PROCMAN_STATUS_LIST", psl);
    }

    class MySubscriber implements LCMSubscriber
    {
        long newestRecievedInitUtime;

        public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
        {
            try {
                if (channel.equals("PROCMAN_PROCESS_LIST")) {
                    procman_process_list_t list = new procman_process_list_t(ins);

                    // Only accept messages from monotonically increasing controllers
                    if (list.init_utime >= newestRecievedInitUtime) {
                        if (list.init_utime > newestRecievedInitUtime)
                            System.out.println("NFO: New ProcMan instance detected");
                        newestRecievedInitUtime = list.init_utime;
                        next_proc_list = list;
                    }
                }
            } catch (IOException ex) {
                System.out.println("WRN: LCM ex: "+ex);
            }
        }
    }

    ////////////////////////////////////////////////////////////////
    class ReaderThread extends Thread
    {
        ProcRecordD     pr;
        BufferedReader ins;
        int            stream;
        BufferedWriter     fouts;

        ReaderThread(ProcRecordD pr, InputStream _ins, int stream, BufferedWriter _fouts)
        {
            this.pr = pr;
            this.ins = new BufferedReader(new InputStreamReader(_ins));
            this.stream = stream;
            this.fouts = _fouts;
        }

        public void run()
        {
            while (true)
            {
                try {
                    String s = ins.readLine();
                    if (s == null)
                        break;
                    publish(s);
                    writeFile(s);

                } catch (IOException ex) {
                    // See note at top of file
                    System.out.println("WRN ReaderThread "+stream+" exiting due to ex: "+ex);
                    break;
                }
            }

            try {
                pr.process.waitFor();
            } catch (InterruptedException ex) {
            }

            if (stream == 0) {
                pr.lastExitCode  = pr.process.exitValue();
                pr.lastExitUtime = TimeUtil.utime();
            }

            try {
                fouts.close();
            } catch (IOException ex) {
                System.out.println("WRN: "+ex);
            }
        }

        public void publish(String s)
        {
            procman_output_t po = new procman_output_t();
            po.host = host;
            po.utime = TimeUtil.utime();
            po.procid = pr.procid;
            po.stream = stream;
            po.data = s;

            lcm.publish("PROCMAN_OUTPUT", po);
        }

        public void writeFile(String s)
        {
            try {
                synchronized(fouts) {
                    fouts.write(s+"\n");
                    fouts.flush();
                }
            } catch (IOException ex) {
                System.out.println("WRN: "+ex);
            }
        }
    }

    public class ShutdownHandler extends Thread
    {
        public void run()
        {
            // Ensure we've closed all the sub processes
            System.out.println("WRN: XXXXX Need to stop sub processes on exit");
        }
    }

    public static void main(String args[])
    {
        GetOpt opts  = new GetOpt();

        opts.addString('n',"hostname",System.getenv("PROCMAN_HOST"),
                       "Hostname wle uses env($PROCMAN_HOST)");
        opts.addBoolean('p',"persistent",false,"Don't exist when the controller quits");
        opts.addBoolean('h',"help",false,"See this help screen");

        if (!opts.parse(args))
            System.out.println("option error: "+opts.getReason());

        if(opts.getBoolean("help")){
            System.out.println("Usage:");
            opts.doHelp();
            System.out.println("Dump:");
            opts.dump();
            System.exit(1);
        }

        ProcManDaemon pm = new ProcManDaemon(opts.getString("hostname"), opts.getBoolean("persistent"));
        pm.run();
    }
}
