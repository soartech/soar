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
 * This class represents the format_4_stream_stop packet as defined in
 * grrc-com.h:
 * 
 * <pre>
 * struct {
 *         float packet_type;
 *         uint32_t id;
 * } format_4_stream_stop;
 * </pre>
 * 
 * <p>
 * Documentation from README:
 * 
 * <blockquote>Cancels a previously established feedback stream.</blockquote>
 * 
 * @author Jonathan Voigt <voigtjr@gmail.com>
 * 
 */
public class GrrcStreamStopPacket implements GrrcPacket
{
    private static final float PACKET_TYPE = 4.0f;

    private static final int BUFFER_SIZE = 8;

    private final DatagramPacket packet;

    private final ByteBuffer buffer = ByteBuffer.allocate(BUFFER_SIZE);

    private static final int ID_OFFSET = 4;

    public GrrcStreamStopPacket(long id, InetAddress address, int port)
    {
        if (!Unsigned.isUnsignedInt(id))
            throw new IllegalArgumentException(
                    "id must be a 32-bit unsigned integer (Java uses long for this)");

        // initialize buffer
        buffer.putFloat(PACKET_TYPE);
        Unsigned.putUnsignedInt(buffer, id);

        packet = new DatagramPacket(buffer.array(), buffer.capacity(), address,
                port);
    }

    public DatagramPacket getPacket()
    {
        return packet;
    }

    @Override
    public String toString()
    {
        return "GrrcStreamStopPacket(" + buffer.getInt(ID_OFFSET) + ")";
    }
}
