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

import java.net.DatagramPacket;
import java.net.InetAddress;
import java.nio.ByteBuffer;

/**
 * <p>
 * This class represents the format_1_drive packet as defined in grrc-com.h:
 * 
 * <pre>
 * struct {
 *         float packet_type;
 *         float speed_mps;
 *         float turn_rate_rps;
 *         float brake;
 *         float flipper_turn_rps;
 * } format_1_drive;
 * </pre>
 * 
 * Documentation from README:
 * 
 * <blockquote> This packet is used to issue commands to the robot. A NAN value
 * for any field tells the robot to keep that value unchanged. A non-NAN value
 * for any field changes the commanded state of the robot. </blockquote>
 * 
 * @author Jonathan Voigt <voigtjr@gmail.com>
 * 
 */
public class GrrcDrivePacket implements GrrcPacket
{
    private static final float PACKET_TYPE = 1.0f;

    private static final int BUFFER_SIZE = 20; // 5 floats

    private final DatagramPacket packet;

    private final ByteBuffer buffer = ByteBuffer.allocate(BUFFER_SIZE);

    private final int SPEED_MPS_OFFSET = 4;

    private final int TURN_RATE_RPS_OFFSET = 8;

    private final int BRAKE_OFFSET = 12;

    private final int FLIPPER_TURN_RPS_OFFSET = 16;

    public GrrcDrivePacket(InetAddress address, int port)
    {
        buffer.putFloat(PACKET_TYPE);
        buffer.putFloat(0);
        buffer.putFloat(0);
        buffer.putFloat(0);
        buffer.putFloat(0);

        packet = new DatagramPacket(buffer.array(), buffer.capacity(), address,
                port);
    }

    public GrrcDrivePacket setSpeedMps(float speedMps)
    {
        buffer.putFloat(SPEED_MPS_OFFSET, speedMps);
        return this;
    }

    public GrrcDrivePacket setTurnRateRps(float turnRateRps)
    {
        buffer.putFloat(TURN_RATE_RPS_OFFSET, turnRateRps);
        return this;
    }

    public GrrcDrivePacket setBrake(float brake)
    {
        buffer.putFloat(BRAKE_OFFSET, brake);
        return this;
    }

    public GrrcDrivePacket setFlipperTurnRps(float flipperTurnRps)
    {
        buffer.putFloat(FLIPPER_TURN_RPS_OFFSET, flipperTurnRps);
        return this;
    }

    public DatagramPacket getPacket()
    {
        return packet;
    }

    @Override
    public String toString()
    {
        buffer.rewind();
        return String.format("GrrcDrivePacket(%1.2f,%1.2f,%1.2f,%1.2f,%1.2f)", buffer
                .getFloat(), buffer.getFloat(), buffer.getFloat(), buffer
                .getFloat(), buffer.getFloat());
    }
}
