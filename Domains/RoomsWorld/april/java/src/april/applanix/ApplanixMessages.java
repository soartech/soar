package april.applanix;

import java.io.*;

import april.util.*;

public class ApplanixMessages
{
    public static Message decode(byte b[]) throws IOException
    {
        int group = (b[4]&0xff) + (b[5]&0xff)*256;
        EndianDataInputStream ins = new EndianDataInputStream(b, true);

        // discard header.
        byte hdr[] = new byte[8];
//        ins.readFully(hdr);
        ins.readDouble();

        switch (group) {
            case 1:
                return new Group1(ins);
            case 10006:
                return new Group10006(ins);
        }

//        System.out.printf("unknown group %d\n", group);
        return null;
    }

    public static class Message
    {
    }

    public static class TimeDistance
    {
        public double time1, time2;
        public double distanceTag;
        public int timeTypes;
        public int distanceType;

        public TimeDistance(EndianDataInputStream ins) throws IOException
        {
            this.time1 = ins.readDouble();
            this.time2 = ins.readDouble();
            this.distanceTag = ins.readDouble();
            this.timeTypes = ins.read()&0xff;
            this.distanceType = ins.read()&0xff;
        }
    }

    public static class Group1 extends Message
    {
        public TimeDistance timeDistance;
        public double lle[] = new double[3]; // latitude, longitude, altitude;

        public double velocity[] = new double[3]; // north, east, down
        public double rpy[] = new double[3];
        public double wander;

        public double trackAngle, speed;

        public double angularRate[] = new double[3]; // roll, pitch, yaw

        public double accel[] = new double[3]; // longitudinal, transverse, down

        public int status;

        public Group1(EndianDataInputStream ins) throws IOException
        {
            timeDistance = new TimeDistance(ins);

            for (int i = 0; i < 3; i++)
                lle[i] = ins.readDouble();

            for (int i = 0; i < 3; i++)
                velocity[i] = ins.readFloat();

            for (int i = 0; i < 3; i++)
                rpy[i] = ins.readDouble();
            wander = ins.readDouble();

            trackAngle = ins.readFloat();
            speed = ins.readFloat();

            for (int i = 0; i < 3; i++)
                angularRate[i] = ins.readFloat();

            for (int i = 0; i < 3; i++)
                accel[i] = ins.readFloat();

            status = ins.read() & 0xff;
        }
    }

    /** Raw DMI Data Status **/
    public static class Group10006 extends Message
    {
        public TimeDistance timeDistance;
        public int upDown;
        public int upDownRectified;
        public int eventCount;
        public int reservedCount;

        public Group10006(EndianDataInputStream ins) throws IOException
        {
            timeDistance = new TimeDistance(ins);

            upDown = ins.readInt();
            upDownRectified = ins.readInt();
            eventCount = ins.readInt();
            reservedCount = ins.readInt();
        }
    }
}
