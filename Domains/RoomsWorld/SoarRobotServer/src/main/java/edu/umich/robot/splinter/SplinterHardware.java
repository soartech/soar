package edu.umich.robot.splinter;

import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import lcm.lcm.LCM;
import lcm.lcm.LCMDataInputStream;
import lcm.lcm.LCMSubscriber;
import orc.Motor;
import orc.Orc;
import orc.OrcStatus;
import orc.spy.Spy;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import april.lcmtypes.pose_t;
import edu.umich.robot.lcmtypes.differential_drive_command_t;
import edu.umich.robot.util.Odometry;
import edu.umich.robot.util.OdometryPoint;
import edu.umich.robot.util.properties.IntegerPropertyProvider;
import edu.umich.robot.util.properties.PropertyManager;

public class SplinterHardware
{
    private static final Log logger = LogFactory.getLog(SplinterHardware.class);
    
    public static class Builder
    {
        private final PropertyManager properties = new PropertyManager();

        public Builder(String name)
        {
            properties.set(SplinterProperties.NAME, name);
        }
        
        public Builder hostname(String hostname)
        {
            properties.set(SplinterProperties.HOSTNAME, hostname);
            return this;
        }
        
        // TODO
        // HERE is where configuration params should be changed, not in the class after it is constructed.
        // stuff like baseline, tickmeters, etc. etc.
        
        public SplinterHardware build()
        {
            return new SplinterHardware(this);
        }
    }
    
    private final PropertyManager properties;
    
    // These are accessed often
    private final IntegerPropertyProvider leftPort = new IntegerPropertyProvider(SplinterProperties.LEFT_MOTOR_PORT);
    private final IntegerPropertyProvider rightPort = new IntegerPropertyProvider(SplinterProperties.RIGHT_MOTOR_PORT);
    private final IntegerPropertyProvider inputDelayWarn = new IntegerPropertyProvider(SplinterProperties.MILLIS_BEFORE_WARN_NO_INPUT);
    
    private final Orc orc;
    private final double maxThrottleChangePerUpdate;
    private final Motor left;
    private final Motor right;
    private final LCM lcm;

    private double commandLeft = 0;
    private double commandRight = 0;
    private double minCommandLeft = 0.1299;
    private double minCommandRight = 0.1481;

    private differential_drive_command_t dc;
    private long lastSeenDCTime = 0;
    private long lastUtime = 0;
    private boolean failsafeSpew = false;
    
    private CalibrateState calibrated = CalibrateState.YES;

    // for odometry update
    private final Odometry odometry;
    private OdometryPoint oldOdom;
    private final pose_t pose = new pose_t();
    private final ScheduledExecutorService schexec = Executors.newSingleThreadScheduledExecutor();
    private final String poseChannel;

    private SplinterHardware(Builder builder)
    {
        this.properties = builder.properties;
        this.properties.setProvider(SplinterProperties.LEFT_MOTOR_PORT, leftPort);
        this.properties.setProvider(SplinterProperties.RIGHT_MOTOR_PORT, rightPort);
        this.properties.setProvider(SplinterProperties.MILLIS_BEFORE_WARN_NO_INPUT, inputDelayWarn);
        
        this.odometry = new Odometry(
                properties.get(SplinterProperties.ENCODERRATE),
                properties.get(SplinterProperties.BASELINE));
        
        this.orc = Orc.makeOrc(properties.get(SplinterProperties.HOSTNAME));
        
        this.maxThrottleChangePerUpdate = 
            properties.get(SplinterProperties.MAX_THROTTLE_ACCEL) / 
            properties.get(SplinterProperties.UPDATE_HZ);
        
        left = new Motor(orc, 
                leftPort.get(), 
                properties.get(SplinterProperties.LEFT_MOTOR_INVERT));
        right = new Motor(orc, 
                rightPort.get(), 
                properties.get(SplinterProperties.RIGHT_MOTOR_INVERT));

        // initial pose
        OrcStatus currentStatus = orc.getStatus();
        oldOdom = getOdometry(currentStatus);
        pose.orientation[0] = 1;
        poseChannel = SplinterPose.POSE_CHANNEL_BASE +
        properties.get(SplinterProperties.NAME);
        
        lcm = LCM.getSingleton();
        
        final String driveChannel = SplinterDrive.DRIVE_CHANNEL_BASE +
                properties.get(SplinterProperties.NAME);
        
        lcm.subscribe(driveChannel, new LCMSubscriber()
        {
            public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
            {
                try 
                {
                    dc = new differential_drive_command_t(ins);
                } 
                catch (IOException e) 
                {
                    logger.error("Error decoding differential_drive_command_t message: " + e.getMessage());
                }
            }
        });

        double updatePeriodMS = 1000 / properties.get(SplinterProperties.UPDATE_HZ);
        logger.debug("Splinter thread running, period set to " + updatePeriodMS);
        schexec.scheduleAtFixedRate(update, 0, (long) updatePeriodMS, TimeUnit.MILLISECONDS);
    }
    
    private enum CalibrateState
    {
        NO, STOP1, RIGHT, STOP2, LEFT, YES
    }

    private OdometryPoint getOdometry(OrcStatus currentStatus)
    {
        return OdometryPoint.newInstance(
                currentStatus.qeiPosition[leftPort.get()] * -1, 
                currentStatus.qeiPosition[rightPort.get()]);
    }

    private void calibrate(OrcStatus currentStatus)
    {
        double dt = (currentStatus.utime - lastUtime) / 1000000.0;
        System.out.println("dt: " + dt);
        boolean moving = (currentStatus.qeiVelocity[0] != 0) || (currentStatus.qeiVelocity[1] != 0);
        switch (calibrated) {
        case NO:
            logger.info("Calibrating, please wait...");
            commandFailSafe();
            calibrated = CalibrateState.STOP1;
            break;

        case STOP1:
            if (moving) {
                break;
            }
            calibrated = CalibrateState.RIGHT;
            // falls through

        case RIGHT:
            if (!moving) {
                commandRight += 0.10 * dt;
                right.setPWM(commandRight);
                break;
            }
            minCommandRight = commandRight;
            commandFailSafe();
            calibrated = CalibrateState.STOP2;
            break;

        case STOP2:
            if (moving) {
                break;
            }
            calibrated = CalibrateState.LEFT;
            // falls through

        case LEFT:
            if (!moving) {
                commandLeft += 0.10 * dt;
                left.setPWM(commandLeft);
                break;
            }
            lastUtime = 0;
            minCommandLeft = commandLeft;
            commandFailSafe();
            calibrated = CalibrateState.YES;
            // values are always going to be high due to system delay, back of
            // 20%
            minCommandLeft *= .8;
            minCommandRight *= .8;
            logger.info(String.format("Minimum motion throttle l%1.4f r%1.4f", minCommandLeft, minCommandRight));
            break;
        }
        if (calibrated == CalibrateState.YES) {
            lastUtime = 0;
        } else {
            lastUtime = currentStatus.utime;
        }
    }

    private Runnable update = new Runnable()
    {
        public void run()
        {
            try 
            {
                // Get OrcStatus
                OrcStatus currentStatus = orc.getStatus();

                if (calibrated != CalibrateState.YES) 
                {
                    calibrate(currentStatus);
                    return;
                }

                boolean moving = (currentStatus.qeiVelocity[0] != 0) || (currentStatus.qeiVelocity[1] != 0);

                OdometryPoint newOdom = getOdometry(currentStatus);

                // don't update odom unless moving
                if (moving)
                    odometry.propagate(newOdom, oldOdom, pose);

                // save old state
                oldOdom = newOdom;

                pose.utime = currentStatus.utime;
                lcm.publish(poseChannel, pose);

                commandMotors();
            } 
            catch (Exception e) 
            {
                e.printStackTrace();
                logger.error("Uncaught exception: " + e);
                commandFailSafe();
            }
        }
    };

    private double mapThrottle(double input, double minCommand)
    {
        double output = ((1 - minCommand) * Math.abs(input)) + minCommand;
        return Math.signum(input) * output;
    }

    private void commandMotors()
    {
        if (dc == null) {
            if (lastSeenDCTime == 0) {
                lastSeenDCTime = System.currentTimeMillis();
            }
            long millisSinceLastCommand = System.currentTimeMillis() - lastSeenDCTime;
            if (millisSinceLastCommand > inputDelayWarn.get()) {
                logger.warn("No drive command yet");
                lastSeenDCTime = System.currentTimeMillis();
            }
            // we haven't seen a drive command yet
            commandFailSafe();
            return;
        }

        differential_drive_command_t dcNew = dc.copy();

        // is it a new command?
        if (lastUtime != dcNew.utime) {
            // it is, save state
            lastSeenDCTime = System.currentTimeMillis();
            lastUtime = dcNew.utime;
        } else {
            // have we not seen a command in the last second?
            long millisSinceLastCommand = System.currentTimeMillis() - lastSeenDCTime;
            if (millisSinceLastCommand > inputDelayWarn.get()) {
                if (failsafeSpew == false) {
                    logger.error("No recent drive command " + millisSinceLastCommand / 1000.0 + " seconds");
                    failsafeSpew = true;
                }
                commandFailSafe();
                return;
            }
        }

        failsafeSpew = false;
        if (logger.isTraceEnabled()) {
            logger.trace(String.format("Got input %1.2f %1.2f", dcNew.left, dcNew.right));
        }

        if (dcNew.left_enabled) {
            double delta = dcNew.left - commandLeft;

            if (delta > 0) {
                delta = Math.min(delta, maxThrottleChangePerUpdate);
            } else if (delta < 0) {
                delta = Math.max(delta, -1 * maxThrottleChangePerUpdate);
            }

            commandLeft += delta;
            left.setPWM(mapThrottle(commandLeft, minCommandLeft));
        } else {
            left.idle();
        }

        if (dcNew.right_enabled) {
            double delta = dcNew.right - commandRight;

            if (delta > 0) {
                delta = Math.min(delta, maxThrottleChangePerUpdate);
            } else if (delta < 0) {
                delta = Math.max(delta, -1 * maxThrottleChangePerUpdate);
            }

            commandRight += delta;
            right.setPWM(mapThrottle(commandRight, minCommandRight));
        } else {
            right.idle();
        }

        if (logger.isTraceEnabled()) {
            logger.trace("Sending input " + (dcNew.left_enabled ? commandLeft : "(idle)") + " "
                    + (dcNew.right_enabled ? commandRight : "(idle)"));
        }
    }

    private void commandFailSafe()
    {
        commandRight = 0;
        commandLeft = 0;
        left.setPWM(0);
        right.setPWM(0);
    }

    public void shutdown()
    {
        schexec.shutdown();
    }

}
