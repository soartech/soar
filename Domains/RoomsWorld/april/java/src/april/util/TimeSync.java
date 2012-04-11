package april.util;

import java.util.*;

public class TimeSync
{
    // last known time on the device. This is used to detect
    // wrap-around: when the device clock is less than the previous
    // value, we've wrapped.
    long last_device_ticks_wrapping;

    // device_ticks_offset represents the offset that should be added
    // to the device's tick counter so that the resulting value is
    // "unwrapped".
    long device_ticks_offset;

    // after how many ticks does the device's clock wrap around?
    long device_ticks_wrap;

    // what is the nominal counting rate for the device?
    double device_ticks_per_second;

    // how large is the error between the host and sensor clock, as
    // measured in intervals of the sensor clock? e.g., for 1% error,
    // use 0.01. Setting this value too large causes looser time
    // synchronization. Setting the value too small can cause
    // divergence!
    double rate_error;

    // If synchronization appears to be off by this much, restart
    // synchronization. This handles the case where the sensor might
    // reset unexpectedly.
    double reset_time;

    // Synchronization data
    //
    // (p = device/sensor, q = host)
    //
    // Note that p_ticks are "unwrapped", i.e., they increase
    // monotonically even if the sensor's clock wraps around.
    long p_ticks, q_ticks;

    // This field indicates the magnitude of the estimated
    // synchronization error. If this field is consistently large, it
    // may indicate divergence in the estimator. (seconds).
    public double last_sync_error;

    // How many times have we resynchronized? (useful for diagnostics)
    public int resync_count;

    /**
       @param device_ticks_per_second How fast does the counter on the device count, in Hz?
       @param device_ticks_wrap After how many ticks does the device counter "roll over"? Use 0 if it does not roll over.
       @param rate_error What is the rate error? (usually a small number like 0.01)
       @param reset_time Force a resynchronization if the sync error exceeds this many seconds.
    **/
    public TimeSync(double device_ticks_per_second, long device_ticks_wrap,
                    double rate_error, double reset_time)
    {
        this.p_ticks = -1;
        this.device_ticks_per_second = device_ticks_per_second;
        this.device_ticks_wrap = device_ticks_wrap;
        this.rate_error = rate_error;
        this.reset_time = reset_time;
    }


    /** Every host/device pair should be passed to this function, and must
     * occur prior to calling _get_host_utime **/
    synchronized public void update(long host_utime, long device_ticks_wrapping)
    {
        assert(device_ticks_wrapping >= 0);

        // check for wrap-around
        if (device_ticks_wrapping < this.last_device_ticks_wrapping) {
            // wrap around has occurred.
            this.device_ticks_offset += this.device_ticks_wrap;
        }
        this.last_device_ticks_wrapping = device_ticks_wrapping;

        long device_ticks = this.device_ticks_offset + device_ticks_wrapping;

        /* We can rewrite the update equations from the paper:
           pi - qi >= p - q - f(pi-p)

           as

           pi - p >= qi - q - f(pi-p)

           dp >= dq - f(dp)

           This form is superior, because we only need to be able to
           accurately represent changes in time for any single clock. This
           avoids precision problems that can occur in the simple
           formulation when the two clocks have very different magnitudes.
        **/

        long pi_ticks = device_ticks;
        long qi_ticks = host_utime;

        double dp = (pi_ticks - this.p_ticks) / this.device_ticks_per_second;
        double dq = (qi_ticks - this.q_ticks) / 1.0E6;

        this.last_sync_error = Math.abs(dp - dq);

        if (this.p_ticks == -1 || this.last_sync_error >= this.reset_time) {
            // force resynchronize
            this.p_ticks = pi_ticks;
            this.q_ticks = qi_ticks;

            // used for diagnostics/debugging
            this.resync_count++;
            return;
        }

        if (dp >= dq - Math.abs(this.rate_error * dp)) {
            this.p_ticks = pi_ticks;
            this.q_ticks = qi_ticks;
        }
    }

    /** For the given device_time, estimate the corresponding host
     * utime. In the case that device_ticks wraps, the most recent
     * possible instance of the device_time is used. **/
    synchronized public long getHostUtime(long device_ticks_wrapping)
    {
        // timesync_update must called first.
        assert (this.p_ticks != -1);
        assert(device_ticks_wrapping >= 0);

        // pick the most recent device_ticks that could match device_ticks_wrapping.
        // There are two candidates:
        // 1) device_ticks_offset + device_ticks_wrapping
        // and
        // 2) device_ticks_offset + device_ticks_wrapping - device_ticks_wrap
        //
        // We pick the most recent (and not future) one. Note that 1) can
        // be in the future.
        //
        // Now, we require that _update() be called on any host/device
        // pairs before this function is called, so all we need to do is
        // determine whether this device_ticks_wrapping is part of the
        // same epoch.

        long device_ticks;

        if (device_ticks_wrapping <= this.last_device_ticks_wrapping) {
            // They're asking about an earlier timestamp from this epoch.
            device_ticks = this.device_ticks_offset + device_ticks_wrapping;
        } else {
            // They're asking about a timestamp from the previous
            // epoch. (They could be asking about multiple epochs ago, but
            // we don't attempt to resolve that ambiguity.)
            device_ticks = this.device_ticks_offset + device_ticks_wrapping - this.device_ticks_wrap;
        }

        long pi_ticks = device_ticks;

         // dp: time in seconds
        double dp = (pi_ticks - p_ticks) / device_ticks_per_second;

        // return units in usecs.
        return ((long) (dp*1.0E6)) + q_ticks + ((long) (1.0E6*Math.abs(rate_error * dp)));
    }

    public static void main(String args[])
    {
        Random r = new Random();

        if (true) {

            TimeSync ts = new TimeSync(1000000, 0, 0.001, 5);

            double rate = 1.0005;
            rate = 0.9995;

            rate = 1;

            for (int i = 0; i < 1000; i++) {
                long trueTime = i * 1000000;
                long sensorTime = (long) (trueTime * rate);
                long hostTime = trueTime + r.nextInt(50000);

                ts.update(hostTime, sensorTime);
                double estHostTime = ts.getHostUtime(sensorTime);
                System.out.printf("%5d %15f\n", i, estHostTime -trueTime); // should always be positive
            }
        }
    }
}
