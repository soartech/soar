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
 * @author Jonathan Voigt <voigtjr@gmail.com>
 * 
 */
public abstract class GrrcRequestBase implements GrrcPacket
{
    public static final int MAX_REQUEST_TAGS = 256;

    private static final int BUFFER_SIZE = getLength(MAX_REQUEST_TAGS);

    private final DatagramPacket packet;

    private final ByteBuffer buffer = ByteBuffer.allocate(BUFFER_SIZE);

    private final int ID_OFFSET = 4;

    private final int SIZE_OFFSET = 8;

    private final int FIELDS_OFFSET = 12;

    private int tagCount = 0;

    private final boolean SEND_ENTIRE_BUFFER = false;

    /**
     * <p>
     * 2 floats, 1 uint32, tag count * 32-bit enums
     */
    private static final int getLength(int tags)
    {
        return (2 * 4) + (1 * 4) + (tags * 4);
    }

    public GrrcRequestBase(long id, InetAddress address, int port)
    {
        if (!Unsigned.isUnsignedInt(id))
            throw new IllegalArgumentException(
                    "id must be a 32-bit unsigned integer (Java uses long for this)");

        // initialize buffer
        buffer.putFloat(getPacketType());
        Unsigned.putUnsignedInt(buffer, id);

        if (SEND_ENTIRE_BUFFER)
        {
            for (int i = 0; i < MAX_REQUEST_TAGS; ++i)
                buffer.putInt(0);
        }

        packet = new DatagramPacket(buffer.array(), buffer.capacity(), address,
                port);
    }

    protected abstract float getPacketType();

    public void addTag(GrrcFeedbackTag tag)
    {
        if (tagCount == MAX_REQUEST_TAGS)
            throw new IllegalStateException("Maximum number of tags reached.");
        Unsigned.putUnsignedInt(buffer, tag.getId(), FIELDS_OFFSET
                + (4 * tagCount));
        tagCount += 1;
    }

    public DatagramPacket getPacket()
    {
        Unsigned.putUnsignedInt(buffer, tagCount, SIZE_OFFSET);
        if (SEND_ENTIRE_BUFFER)
            packet.setLength(getLength(MAX_REQUEST_TAGS));
        else
            packet.setLength(getLength(tagCount));
        return packet;
    }

    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append(buffer.getInt(ID_OFFSET));
        for (int i = 0; i < tagCount; ++i)
        {
            sb.append(String.format(",%x", buffer.getInt(FIELDS_OFFSET
                    + (4 * i))));
        }

        return sb.toString();
    }
}
