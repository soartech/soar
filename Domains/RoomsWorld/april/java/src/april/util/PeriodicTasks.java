package april.util;

import java.util.concurrent.*;
import java.util.*;

/** Like java.util.Timer, except allows multiple threads to process
 * tasks, and supports pausing execution (without shutting down the
 * timer.
 *
 * At creation, it is in the non-running state.
 **/
public class PeriodicTasks
{
    PriorityBlockingQueue<Record> queue = new PriorityBlockingQueue<Record>();
    int nthreads;

    ArrayList<WorkerThread> workers = new ArrayList<WorkerThread>();

    static class Record implements Comparable<Record>
    {
        Task task;
        long lastRunTime; // milliseconds, ala System.currentTimeMillis()
        long nextRunTime; // milliseconds, ala System.currentTimeMillis()
        int period; // milliseconds
        boolean fixedRate; // true: constant rate. false: constant delay.

        public int compareTo(Record t)
        {
            long v = nextRunTime - t.nextRunTime;
            if (v < 0)
                return -1;
            if (v == 0)
                return 0;
            return 1;
        }
    }

    public interface Task
    {
        /** dt: the amount of time that should be considered to have
         * elapsed between the now and the last invocation,
         * subtracting time when we were paused. **/
        public void run(double dt);
    }

    public PeriodicTasks()
    {
        this(1);
    }

    public PeriodicTasks(int nthreads)
    {
        this.nthreads = nthreads;
        setRunning(false);
    }

    public synchronized void addFixedRate(Task task, double dt)
    {
        Record r = new Record();
        r.task = task;
        r.period = (int) (dt * 1000);
        r.fixedRate = true;

        queue.put(r);
    }

    public synchronized void addFixedDelay(Task task, double dt)
    {
        Record r = new Record();
        r.task = task;
        r.period = (int) (dt * 1000);
        r.fixedRate = false;

        queue.put(r);
    }

    public synchronized boolean isRunning()
    {
        return workers.size() > 0;
    }

    public synchronized void setRunning(boolean b)
    {
        if (b) {
            if (workers.size() == 0) {

                for (Record r : queue) {
                    r.lastRunTime = 0;
                    r.nextRunTime = 0;
                }

                for (int i = 0; i < nthreads; i++) {
                    WorkerThread worker = new WorkerThread();
                    worker.start();
                    workers.add(worker);
                }
            }
        } else {
            // send N signals that we should exit. Each worker will
            // consume one.
            for (WorkerThread worker : workers)
                queue.put(new Record());

            workers.clear();
        }
    }

    class WorkerThread extends Thread
    {
        public void run()
        {
            while (true) {
                Record r = null;
                long now;

                try {
                    r = queue.take();

                    if (r.task == null) {
                        return;
                    }

                    now = System.currentTimeMillis();

                    long delay = r.nextRunTime - now;
                    if (delay > 0) {
                        Thread.sleep(delay);
                        now = System.currentTimeMillis();
                    }

                } catch (InterruptedException ex) {
                    System.out.println("ex: "+ex);
                    break;
                }

                // if the first time we're running the task, set the run time.
                if (r.lastRunTime == 0) {
                    r.lastRunTime = now;
                    r.nextRunTime = now;
                }

                double dt = (now - r.lastRunTime) / 1000.0;
                r.task.run(dt);
                long end = System.currentTimeMillis();

                r.lastRunTime = now;

                if (r.fixedRate)
                    r.nextRunTime += r.period;
                else
                    r.nextRunTime = end + r.period;

                queue.put(r);
            }
        }
    }
}
