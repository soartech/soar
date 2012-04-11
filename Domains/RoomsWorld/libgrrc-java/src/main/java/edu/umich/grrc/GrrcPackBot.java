/*
 * Copyright (c) 2011, Regents of the University of Michigan
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package edu.umich.grrc;

import java.io.IOException;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * @author Jonathan Voigt <voigtjr@gmail.com>
 * 
 */
public class GrrcPackBot
{
    private static final long ID = 1;

    private static final float ON = 1.0f;

    private static final float OFF = 0.0f;

    private final GrrcDrivePacket drivePacket;

    private final DatagramSocket sock;

    private final InetAddress ia;

    private final int port;

    private final ScheduledExecutorService schexec = Executors
            .newSingleThreadScheduledExecutor();

    private final GrrcFeedbackThread fbThread;

    private final ScheduledFuture<?> driveTask;

    private final ScheduledFuture<?> streamTask;

    private final GrrcStreamRequestPacket streamPacket;

    private float speed;

    private float turnRate;

    private boolean receivedStreamPacket = false;

    private boolean spam = false;

    public GrrcPackBot(String hostname, int port) throws UnknownHostException,
            SocketException
    {
        sock = new DatagramSocket();
        fbThread = new GrrcFeedbackThread(sock);
        fbThread.addHandler(fbHandler);

        ia = InetAddress.getByName(hostname);
        this.port = port;

        // Drive task sends the drive packets
        drivePacket = new GrrcDrivePacket(ia, port);
        driveTask = schexec.scheduleAtFixedRate(driveTaskBody, 0, 100,
                TimeUnit.MILLISECONDS);

        // Stream task makes sure the feedback stream is running
        streamPacket = getStreamRequestPacket(ia, port);
        streamTask = schexec.scheduleAtFixedRate(streamTaskBody, 0, 1,
                TimeUnit.SECONDS);

    }

    public void setSpam(boolean spam)
    {
        this.spam = spam;
    }

    public boolean getSpam()
    {
        return this.spam;
    }

    public void shutdown()
    {
        driveTask.cancel(true);
        streamTask.cancel(true);

        fbThread.shutdown();
        schexec.shutdown();

        send(new GrrcStreamStopPacket(ID, ia, port));

        sock.close();
    }

    public void setSpeed(float speed)
    {
        synchronized (drivePacket)
        {
            this.speed = speed;
        }
    }

    public void setTurnRate(float turnRate)
    {
        synchronized (drivePacket)
        {
            this.turnRate = turnRate;
        }
    }

    public void setVelocities(float speed, float turnRate)
    {
        synchronized (drivePacket)
        {
            this.speed = speed;
            this.turnRate = turnRate;
        }
    }

    private final Runnable driveTaskBody = new Runnable()
    {
        public void run()
        {
            synchronized (drivePacket)
            {
                drivePacket.setSpeedMps(speed);
                drivePacket.setTurnRateRps(turnRate);
                drivePacket.setBrake((speed == 0.0f && turnRate == 0.0f) ? ON
                        : OFF);
            }
            send(drivePacket);
        }
    };

    private final Runnable streamTaskBody = new Runnable()
    {
        public void run()
        {
            if (receivedStreamPacket)
            {
                receivedStreamPacket = false;
                return;
            }

            send(streamPacket);
        }
    };

    private void send(GrrcPacket packet)
    {
        try
        {
            sock.send(packet.getPacket());
            if (spam)
                System.out.println(packet);
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }

    private final GrrcFeedbackHandler fbHandler = new GrrcFeedbackHandler()
    {
        public void packetReceived(GrrcFeedbackPacket packet)
        {
            List<Float> values = packet.getValues();
            if (packet.getId() == ID)
            {
                if (!decodeStreamPacket(values))
                    System.err.println("Error decoding packet: " + packet);
                else
                    receivedStreamPacket = true;
            }
            else
            {
                System.err.println("Warning: feedback packet with unknown id received: " + packet);
                return;
            }

        }
    };

    private Map<GrrcFeedbackTag, Float> fbValues = new EnumMap<GrrcFeedbackTag, Float>(
            GrrcFeedbackTag.class);

    private static final GrrcFeedbackTag[] FIELDS = new GrrcFeedbackTag[] {
            GrrcFeedbackTag.GRRC_FB_BATTERY_PERCENTAGE,
            GrrcFeedbackTag.GRRC_FB_VELOCITY, GrrcFeedbackTag.GRRC_FB_TURNRATE,
            GrrcFeedbackTag.GRRC_FB_BRAKE, GrrcFeedbackTag.GRRC_FB_ENCODER0,
            GrrcFeedbackTag.GRRC_FB_ENCODER1, };

    private static GrrcStreamRequestPacket getStreamRequestPacket(
            InetAddress ia, int port)
    {
        GrrcStreamRequestPacket p = new GrrcStreamRequestPacket(ID, ia, port);
        for (GrrcFeedbackTag t : FIELDS)
            p.addTag(t);
        return p;
    }

    private boolean decodeStreamPacket(List<Float> values)
    {
        if (values.size() != FIELDS.length)
            return false;
        for (int i = 0; i < FIELDS.length; ++i)
            fbValues.put(FIELDS[i], values.get(i));
        return true;
    }

    public float[] getPos()
    {
        return getPos(null);
    }

    public float[] getPos(float[] dest)
    {
        if (dest == null)
            dest = new float[3];

        if (fbValues.isEmpty())
            return dest;

        dest[0] = fbValues.get(GrrcFeedbackTag.GRRC_FB_ODOM_X);
        dest[1] = fbValues.get(GrrcFeedbackTag.GRRC_FB_ODOM_Y);
        dest[2] = 0;

        return dest;
    }

    public float getVelocity()
    {
        if (fbValues.isEmpty())
            return 0.0f;

        return fbValues.get(GrrcFeedbackTag.GRRC_FB_VELOCITY);
    }

    public float getEncoder0()
    {
        if (fbValues.isEmpty())
            return 0.0f;

        return fbValues.get(GrrcFeedbackTag.GRRC_FB_ENCODER0);
    }

    public float getEncoder1()
    {
        if (fbValues.isEmpty())
            return 0.0f;

        return fbValues.get(GrrcFeedbackTag.GRRC_FB_ENCODER1);
    }

    public float getTurnRate()
    {
        if (fbValues.isEmpty())
            return 0.0f;

        return fbValues.get(GrrcFeedbackTag.GRRC_FB_TURNRATE);
    }

    public boolean getBrake()
    {
        if (fbValues.isEmpty())
            return false;

        return fbValues.get(GrrcFeedbackTag.GRRC_FB_BRAKE) == 0.0f ? false
                : true;
    }
}
