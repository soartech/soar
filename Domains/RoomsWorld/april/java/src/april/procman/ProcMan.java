package april.procman;

import java.io.*;
import java.util.*;

import lcm.lcm.*;

import april.config.*;
import april.util.*;

import april.lcmtypes.*;

/**
 * ProcMan is a process manager for Java. A config file specifies
 * which processes should be managed, and the policy for when to run
 * these, and what to do if they fail.
 *
 * The config file is a list of processes in the proc# format.
 * Each processes must specify the following properties:
 *   - host, auto-restart, restart-delay-ms, auto-start, cmd
 * In order, these do the following:
 *   - host                  Which host to run the process on
 *   - auto-restart          On fail, specifies to restart process
 *   - restart-delay-ms      If auto-restart is true, duration before restart
 *   - auto-start            If process should be started automatically on ProcMan start
 *   - cmd                   Actual command to be run
 *   - group                 Group this process belongs to (e.g. debug, simulation)
 *
 * These properties are usually inherited from a default namespace
 * An Example config file is presented below, with a default namespace and two commands:
 *
 * std {
 *     host = "localhost"; // Host to run processes on
 *     auto-restart = "true"; // restart on fail?
 *     restart-delay-ms = "500";
 *     auto-start = "true";
 *     group = "std";
 * }
 *
 * proc# : std {
 *     cmd = "ps aux";
 * }
 *
 * proc# : std {
 *     cmd = "ls";
 *     auto-start="false";
 * }
 *
 *
 * TODO: Ensure that the latency is being calculated correclty in GUI.
 * TODO: Make sure the GUI is only keeping fixed history
 * TODO: Test behavior when daemon doesnt die, and a new proc list is enabled.
 */
public class ProcMan// implements Runnable
{
    static final int LIST_BROADCAST_PERIOD = 200;

    ArrayList<ProcRecord> processes = new ArrayList<ProcRecord>();
    HashMap<Integer, ProcRecord> processesMap = new HashMap<Integer, ProcRecord>();

    static LCM lcm = LCM.getSingleton();

    Spy gui;

    int nextProcId = 1;
    boolean verbose;
    boolean exit;
    long initUtime = TimeUtil.utime();

    public ProcMan(String _configPath, String _runtimes, boolean _useGUI, boolean _verbose)
    {
        verbose = _verbose;

        // Why?: Reading from config file must be after nearly everything else is done
        loadConfig(_configPath,_runtimes);

        if (_useGUI) {
            gui = new Spy(this);
        }

        // Ensures we send messages to the ProcManDaemon to shutdown all the processes
        Runtime.getRuntime().addShutdownHook(new ShutdownHandler());
    }

    private void loadConfig(String path, String runtimeList)
    {
        Config config = null;
        try{
            config = new ConfigFile(new File(path));
        }catch(IOException e){
            System.out.println("ERR: ProcMan failed to read config");
            System.exit(1);
        }

        String runtimes[] = new String[0];
        if (!runtimeList.equals(""))
            runtimes = runtimeList.split(":");

        String keys[] = config.getKeys();

        for (String key : keys) {
            //Identify matching processes
            if (key.startsWith("proc") && key.endsWith(".cmd")) {
                int id  = Integer.parseInt(key.substring(4,key.length()-4));
                String proc = "proc" + id + ".";

                // Only run the specified 'names'
                if (runtimes.length  > 0) {
                    String group = config.getString(proc+"group");
                    boolean matches = false;
                    for (String runtime : runtimes) {
                        if (runtime.equals(group)) {
                            matches = true;
                            break;
                        }
                    }
                    if (!matches) {
                        System.out.println("Skipping runtime in "+group+": "+
                                           config.getString(proc+"cmd"));
                        continue;
                    }
                }

                String host = config.requireString(proc+"host");
                String commandEnv = config.requireString(proc+"cmd");
                String command = StringUtil.replaceEnvironmentVariables(commandEnv);
                boolean autoRestart = config.requireBoolean(proc+"auto-restart");
                int restartDelayMS = config.requireInt(proc+"restart-delay-ms");
                boolean autoStart = config.requireBoolean(proc+"auto-start");
                String name = config.getString(proc+"name", "unknown");

                if (host == null || command == null) {
                    System.err.println("ERR: Malformatted config file."+
                                       " host or cmd is null. Exiting");
                    System.exit(1);
                }

                addProc(host, name, command, autoRestart, restartDelayMS, autoStart);
            }
        }

    }

    synchronized void addProc(String host, String name, String cmdline, boolean restart,
                              int restartDelay, boolean autorun)
    {
        ProcRecord pr = new ProcRecord();
        pr.procid = nextProcId++;
        pr.host = host;
        pr.cmdline = cmdline;
        pr.name = name;
        pr.autoRestart = restart;
        pr.restartDelayMS = restartDelay;
        pr.running = autorun;

        if (verbose)
            System.out.printf("Adding new proc: id=%d host=%s name=%s cmd=%s"+
                              " autorestart=%s restart-delay=%d running=%s\n",
                              pr.procid,
                              pr.host,
                              pr.name,
                              pr.cmdline,
                              ""+pr.autoRestart,
                              pr.restartDelayMS,
                              ""+pr.running);


        processes.add(pr);
        processesMap.put(pr.procid, pr);
        if (gui != null)
            gui.processTableModel.fireTableRowsInserted(processes.size()-1,
                                                        processes.size()-1);
    }

    // How to control the processes from an external class
    synchronized void toggleRunStatus(int procid)
    {
        ProcRecord pr = processesMap.get(procid);
        pr.running = !pr.running;
    }

    synchronized void setRunStatus(int procid, boolean value)
    {
        ProcRecord pr = processesMap.get(procid);
        pr.running = value;
    }

    synchronized boolean getRunStatus(int procid)
    {
        ProcRecord pr = processesMap.get(procid);
        return pr.running;
    }

    public void run()
    {
        while (true) {
            TimeUtil.sleep(LIST_BROADCAST_PERIOD);
            sendProcessList();
        }
    }

    synchronized void sendProcessList()
    {
        procman_process_list_t ppl = new procman_process_list_t();
        ppl.utime = TimeUtil.utime();
        ppl.init_utime = initUtime;
        ppl.exit = exit;

        ppl.nprocs = processes.size();
        ppl.processes = new procman_process_t[ppl.nprocs];
        for (int i = 0; i < processes.size(); i++) {
            procman_process_t pp = new procman_process_t();
            ProcRecord pr = processes.get(i);
            pp.procid = pr.procid;
            pp.cmdline = pr.cmdline;
            pp.name = pr.name;
            pp.host = pr.host;
            pp.auto_restart = pr.autoRestart;
            pp.restart_delay_ms = pr.restartDelayMS;
            pp.running = pr.running;

            ppl.processes[i] = pp;
        }

        lcm.publish("PROCMAN_PROCESS_LIST", ppl);
    }


    public class ShutdownHandler extends Thread implements LCMSubscriber
    {
        procman_status_list_t status;

        public ShutdownHandler() {
            lcm.subscribe("PROCMAN_STATUS_LIST", this);
        }

        public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
        {
            try {
                if (channel.equals("PROCMAN_STATUS_LIST")) {
                    status = new procman_status_list_t(ins);
                }
            } catch (IOException ex) {
                System.out.println("WRN: LCM ex: "+ex);
            }
        }

        public void run()
        {
            System.out.println("NFO: ProcMan beginning the shutdown process...");

            // notify the daemon
            exit = true;
            for (int i = 0; i < 5; i++) {
                sendProcessList();
                TimeUtil.sleep(10);
            }

            for (int i = 0; i < 4; i++) { // wait max. of 2 secs for exit
                System.out.println("NFO: ProcMan awaiting confirmation from the daemon... "+(i+1));
                TimeUtil.sleep(500);
                if (status != null && (status.received_init_utime != initUtime ||
                                       status.nprocs == 0)) {
                    System.out.println("NFO: ProcManDaemon confirms processes ended...");
                    return;
                }
            }
            System.err.println("ERR: Unable to confirm from daemon that processes have exited!");
        }
    }

    public static void main(String args[])
    {
        GetOpt opts  = new GetOpt();

        opts.addString('c',"config","","Location of the procman config"+
                       " file (Required)");
        opts.addString('r',"runtimes","","Which process groups to run "+
                       "separated by ':'.");
        opts.addBoolean('d',"daemon",false,"Start a daemon automatically");

        opts.addBoolean('g',"gui",false,"Enables procman gui");
        opts.addBoolean('h',"help",false,"See this help screen");
        opts.addBoolean('v',"verbose",false,"Display extra information");

        if (!opts.parse(args))
	    {
            System.out.println("option error: "+opts.getReason());
	    }

        if (opts.getBoolean("help")) {
            System.out.println("Usage:");
            opts.doHelp();
            System.out.println("Dump:");
            opts.dump();
            System.exit(1);
        }

        if (opts.getBoolean("daemon")) {
            System.out.println("Starting daemon process");
            new Thread(new ProcManDaemon()).start();
        }

        ProcMan pm = new ProcMan(EnvUtil.expandVariables(opts.getString("config")),
                                 opts.getString("runtimes"),
                                 opts.getBoolean("gui"),
                                 opts.getBoolean("verbose"));
        pm.run();
    }
}
