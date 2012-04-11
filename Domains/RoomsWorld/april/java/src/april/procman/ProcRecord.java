package april.procman;

class ProcRecord
{
    int    procid;
    String host;
    String cmdline;
    String name;

    int restartCount;
    int lastExitCode;

    boolean autoRestart; // auto restart this process on exit
    int     restartDelayMS; // num. millisecs until restarted if failed
    boolean running; // try to stop the proc.
}
