package april.util;

public class ExpiringMessageCache<T>
{
    T    t;
    long receivedUtime;  // local host time
    double maxAge;       // in seconds

    // prevent stale message from becoming current message
    boolean ensureMonotonicUtimes;
    long msgUtime;  // may be another host's utime

    public ExpiringMessageCache(double maxAge)
    {
        this.maxAge = maxAge;
    }

    public ExpiringMessageCache(double maxAge, boolean increasingUtimes)
    {
        ensureMonotonicUtimes = increasingUtimes;
        this.maxAge = maxAge;
    }

    public void put(T t, long utime)
    {
        put(t, utime, utime);
    }

    /**
     * Put new message in cache that can ensure monotonic msgUtimes
     * even when the message comes form a different host.
     * @param T the data to cache
     * @param msgUtime utime in message (host time of sender)
     * @param receivedUtime generally, local TimeUtil.utime(), but not always
     * @return boolean for whether the cache was updated
     **/
    public synchronized boolean put(T t, long msgUtime, long receivedUtime)
    {
        // this is more strict than monotonic in that it req's ">" as opposed to ">="
        if (!ensureMonotonicUtimes || msgUtime > this.msgUtime) {
            this.t = t;
            this.msgUtime = msgUtime;
            this.receivedUtime = receivedUtime;
            return true;
        }
        return false;
    }

    public synchronized T get()
    {
        if (t == null)
            return null;

        long now = TimeUtil.utime();
        double age = (now - receivedUtime) / 1000000.0;
        if (age > maxAge)
            return null;

        return t;
    }

    public synchronized Pair<T, Double> getWithAge()
    {
        if (t == null)
            return null;

        long now = TimeUtil.utime();
        double age = (now - receivedUtime) / 1000000.0;
        if (age > maxAge)
            return null;

        return new Pair<T, Double>(t, age);
    }

    public long getMsgUtime()
    {
        return msgUtime;
    }
}
